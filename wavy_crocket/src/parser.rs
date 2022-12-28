use std::{fmt::{Debug, Write}, rc::Rc};

type ParseResult<T> = Result<T, String>;

// FYI: a string is just a list of chars
#[derive(PartialEq, Clone)]
pub enum Value {
    Nil,
    Symbol(String),
    Int(i64),
    Float(f64),
    Char(char),
    Quote(Rc<SExpr>),
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil           => f.write_str("nil"),
            Value::Symbol(value) => f.write_str(value),
            Value::Int(value)    => f.write_str(&value.to_string()),
            Value::Float(value)  => f.write_str(&value.to_string()),
            Value::Char(value)   => f.write_str(&format!("?{value}")),
            Value::Quote(sexpr)  => f.write_str(&format!("'{sexpr:?}")),
        }
    }
}

#[derive(PartialEq, Clone)]
pub enum SExpr {
    Atom(usize, Value),
    S(usize, Rc<SExpr>, Rc<SExpr>),
}

impl Debug for SExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SExpr::Atom(_, value) => value.fmt(f),
            SExpr::S(_, lhs, rhs) => {
                f.write_str(&format!("({:?} . {:?})", lhs, rhs))
            },
        }
    }
}

struct ParseContext {
    content: Vec<char>,
    pos: usize,
}

impl ParseContext {
    fn at_end(&self) -> bool {
        self.pos >= self.content.len()
    }

    fn peek(&self) -> Option<char> {
        self.content.get(self.pos).cloned()
    }
}

fn parse_str(s: &str) -> ParseResult<Vec<SExpr>> {
    parse(ParseContext {
        content: s.chars().collect::<Vec<_>>(),
        pos: 0,
    })
}

fn parse(mut ctx: ParseContext) -> ParseResult<Vec<SExpr>> {
    let mut exprs = vec![];
    parse_ws(&mut ctx);
    while !ctx.at_end() {
        exprs.push(parse_expr(&mut ctx)?);
        parse_ws(&mut ctx);
    }
    Ok(exprs)
}

// Returns true if it parsed any amount of whitespace
fn parse_ws(ctx: &mut ParseContext) -> bool {
    let mut has_ws = false;
    while !ctx.at_end() {
        let c = ctx.content[ctx.pos];
        if c != ' ' && c != '\n' && c != '\t' {
            break;
        }
        has_ws = true;
        ctx.pos += 1;
    }
    has_ws
}

fn is_symbol_char(c: char) -> bool {
    match c {
        '_' => true,
        '+' => true,
        '-' => true,
        '/' => true,
        '*' => true,
        '%' => true,
        c if c.is_alphabetic() => true,
        c if c.is_digit(10) => true,
        _ => false,
    }
}

fn is_digit_char(c: char) -> bool {
    c == '.' || c.is_digit(10)
}

// Expects to not be at EOF
fn parse_expr(ctx: &mut ParseContext) -> ParseResult<SExpr> {
    let pos = ctx.pos;
    match ctx.content[ctx.pos] {
        '\'' => {
            ctx.pos += 1;
            let ex = parse_expr(ctx)?;
            Ok(SExpr::Atom(pos, Value::Quote(Rc::new(ex))))
        },
        '\"' => parse_string(ctx),
        '?' => Ok(SExpr::Atom(pos, parse_char(ctx)?)),
        '(' => parse_sexpr(ctx),
        c if c.is_digit(10) || c == '.' => Ok(SExpr::Atom(pos, parse_num(ctx)?)),
        c if is_symbol_char(c) => Ok(SExpr::Atom(pos, parse_symbol(ctx)?)),
        c => Err(format!("abcd Unexpected character: {c}")),
    }
}

fn parse_num(ctx: &mut ParseContext) -> ParseResult<Value> {
    let mut s = String::new();
    while let Some(c) = ctx.peek().filter(|&c| is_digit_char(c)) {
        s.push(c);
        ctx.pos += 1;
    }
    if let Ok(value) = s.parse::<f64>() {
        return Ok(Value::Float(value));
    }
    if let Ok(value) = s.parse::<i64>() {
        return Ok(Value::Int(value));
    }
    Err("Invalid integer".to_owned())
}

// Expects the leading ? to have been parsed
fn parse_char(ctx: &mut ParseContext) -> ParseResult<Value> {
    let pos = ctx.pos;
    ctx.pos += 1;
    if let Some(c) = ctx.peek() {
        ctx.pos += 1;
        Ok(Value::Char(c))
    } else {
        Err("Expected character, found EOF".to_owned())
    }
}

// Expects the first char to already be a double quote
fn parse_string(ctx: &mut ParseContext) -> ParseResult<SExpr> {
    let pos = ctx.pos;
    ctx.pos += 1;
    let mut chars = vec![];
    while let Some(c) = ctx.peek() {
        if c == '"' {
            ctx.pos += 1;
            return Ok(chars.into_iter().rev().fold(SExpr::Atom(pos, Value::Nil), |acc, it| {
                let value = SExpr::Atom(pos, Value::Char(it));
                SExpr::S(pos, Rc::new(value), Rc::new(acc))
            }));
        }
        chars.push(c);
        ctx.pos += 1;
    }
    Err("Hit EOF while parsing string".to_owned())
}

// Assumes the current char is a valid symbol char
fn parse_symbol(ctx: &mut ParseContext) -> ParseResult<Value> {
    let mut s = String::new();
    while let Some(c) = ctx.peek().filter(|&c| is_symbol_char(c)) {
        s.push(c);
        ctx.pos += 1;
    }
    if s == "nil" {
        Ok(Value::Nil)
    } else {
        Ok(Value::Symbol(s))
    }
}

// Assumes we know that the current char is a left paren
fn parse_sexpr(ctx: &mut ParseContext) -> ParseResult<SExpr> {
    ctx.pos += 1;
    parse_ws(ctx);
    let initial_pos = ctx.pos;
    let lhs = parse_expr(ctx)?;
    parse_ws(ctx);
    // Just something nested inside parens, not an actual SExpr
    if ctx.peek() == Some(')') {
        ctx.pos += 1;
        return Ok(lhs);
    }
    if ctx.peek() == Some('.') {
        ctx.pos += 1;
        parse_ws(ctx);
        let rhs = parse_expr(ctx)?;
        parse_ws(ctx);
        if ctx.peek() != Some(')') {
            return Err("Expected )".to_owned());
        }
        ctx.pos += 1;
        return Ok(SExpr::S(initial_pos, Rc::new(lhs), Rc::new(rhs)));
    }
    // Otherwise, assume we're using the list style SExpr syntax
    let mut exprs = vec![(initial_pos, lhs)];
    while ctx.peek() != Some(')') {
        exprs.push((ctx.pos, parse_expr(ctx)?));
        parse_ws(ctx);
    }
    ctx.pos += 1;
    Ok(exprs.into_iter().rev().fold(SExpr::Atom(0, Value::Nil), |acc, it| {
        SExpr::S(it.0, Rc::new(it.1), Rc::new(acc))
    }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_num() {
        assert_eq!("Ok([1234])", format!("{:?}", parse_str("1234")));
        assert_eq!("Ok([3.14])", format!("{:?}", parse_str("3.14")));
    }

    #[test]
    fn test_parse_sexpr() {
        assert_eq!("Ok([(x . y)])", format!("{:?}", parse_str(
                    "(x . y)"
                    )));

        assert_eq!("Ok([(x . (y . nil))])", format!("{:?}", parse_str(
                    "(x y)"
                    )));
        assert_eq!("Ok([(x . (y . (42 . nil)))])", format!("{:?}", parse_str(
                    "(x y 42)"
                    )));
        assert_eq!("Ok([(x . (y . (42 . nil))), (a . b)])", format!("{:?}", parse_str(
                    "(x y 42)\n(a . b)"
                    )));
    }

    #[test]
    fn test_parse_char() {
        assert_eq!("Ok([?x])", format!("{:?}", parse_str("?x")));
    }

    #[test]
    fn test_parse_quote() {
        assert_eq!("Ok(['(a . b)])", format!("{:?}", parse_str("'(a . b)")));
        assert_eq!("Ok(['?a])", format!("{:?}", parse_str("'?a")));
    }

    #[test]
    fn test_parse_string() {
        assert_eq!("Ok([(?a . (?b . (?c . nil)))])", format!("{:?}", parse_str("\"abc\"")));
    }
}
