use std::fmt::{Debug, Write};

type ParseResult<T> = Result<T, String>;

#[derive(PartialEq, Clone)]
enum Value {
    Nil,
    Symbol(String),
    Int(i64),
    Float(f64),
}


impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil           => f.write_str("nil"),
            Value::Symbol(value) => f.write_str(value),
            Value::Int(value)    => f.write_str(&value.to_string()),
            Value::Float(value)  => f.write_str(&value.to_string()),
        }
    }
}

#[derive(PartialEq, Clone)]
enum SExpr {
    Atom(Value),
    S(Box<SExpr>, Box<SExpr>),
}

impl Debug for SExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SExpr::Atom(value) => value.fmt(f),
            SExpr::S(lhs, rhs) => {
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

fn is_symbol_start_char(c: char) -> bool {
    c == '_' || c.is_alphabetic()
}

fn is_symbol_char(c: char) -> bool {
    is_symbol_start_char(c) || c.is_digit(10) || c == '-'
}

fn is_digit_char(c: char) -> bool {
    c == '.' || c.is_digit(10)
}

// Expects to not be at EOF
fn parse_expr(ctx: &mut ParseContext) -> ParseResult<SExpr> {
    match ctx.content[ctx.pos] {
        '(' => parse_sexpr(ctx),
        c if c.is_digit(10) || c == '.' => Ok(SExpr::Atom(parse_num(ctx)?)),
        c if is_symbol_start_char(c) => Ok(SExpr::Atom(parse_symbol(ctx)?)),
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

// Assumes the current char is a valid symbol char
fn parse_symbol(ctx: &mut ParseContext) -> ParseResult<Value> {
    let mut s = String::new();
    while let Some(c) = ctx.peek().filter(|&c| is_symbol_char(c)) {
        s.push(c);
        ctx.pos += 1;
    }
    Ok(Value::Symbol(s))
}

// Assumes we know that the current char is a left paren
fn parse_sexpr(ctx: &mut ParseContext) -> ParseResult<SExpr> {
    ctx.pos += 1;
    parse_ws(ctx);
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
        return Ok(SExpr::S(Box::new(lhs), Box::new(rhs)));
    }
    // Otherwise, assume we're using the list style SExpr syntax
    let mut exprs = vec![lhs];
    while ctx.peek() != Some(')') {
        exprs.push(parse_expr(ctx)?);
        parse_ws(ctx);
    }
    ctx.pos += 1;
    Ok(exprs.into_iter().rev().fold(SExpr::Atom(Value::Nil), |acc, it| {
        SExpr::S(Box::new(it), Box::new(acc))
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
}
