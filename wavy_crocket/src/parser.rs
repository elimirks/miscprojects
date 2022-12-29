use std::{fmt::Debug, rc::Rc, collections::HashMap};

type ParseResult<T> = Result<T, String>;

/// FYI: a string is just a list of chars
#[derive(PartialEq, Clone)]
pub enum Value {
    Symbol(String),
    Builtin(Builtin),
    Int(i64),
    Float(f64),
    Char(char),
    Quote(Rc<SExpr>),
    Function(Vec<String>, Vec<Rc<SExpr>>),
}

impl Value {
    pub fn nil() -> Value {
        Value::Builtin(Builtin::Nil)
    }

    pub fn is_nil(&self) -> bool {
        *self == Value::nil()
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Symbol(value)  => f.write_str(value),
            Value::Int(value)     => f.write_str(&value.to_string()),
            Value::Float(value)   => {
                if value % 1.0 == 0.0 {
                    f.write_str(&format!("{}.0", value.floor()))
                } else {
                    f.write_str(&value.to_string())
                }
            },
            Value::Char(value)    => f.write_str(&format!("?{value}")),
            Value::Quote(sexpr)   => f.write_str(&format!("'{sexpr:?}")),
            Value::Function(args, body) => {
                f.write_str("(lambda (")?;
                f.write_str(&args.join(" "))?;
                f.write_str(") ")?;
                let body_strs = body.iter().map(|expr| format!("{expr:?}")).collect::<Vec<_>>();
                f.write_str(&body_strs.join(" "))?;
                f.write_str(")")?;
                Ok(())
            },
            Value::Builtin(value) => value.fmt(f),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Copy)]
pub enum Builtin {
    Nil,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    /// Sets a value in the current scope
    Set,
    /// Sets a value in the global scope
    Setg,
    Do,
    Lambda,
    Defun,
    Putc,
    If,
    Car,
    Cdr,
    Cons,
    Debug,
    IsFalsy,
    IsEq,
    Exit,
}

impl Debug for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Builtin::Nil => f.write_str("nil"),
            Builtin::Add => f.write_str("+"),
            Builtin::Sub => f.write_str("-"),
            Builtin::Mul => f.write_str("*"),
            Builtin::Div => f.write_str("/"),
            Builtin::Mod => f.write_str("%"),
            Builtin::Set => f.write_str("set"),
            Builtin::Setg => f.write_str("setg"),
            Builtin::Do => f.write_str("do"),
            Builtin::Lambda => f.write_str("lambda"),
            Builtin::Defun => f.write_str("defun"),
            Builtin::Putc => f.write_str("putc"),
            Builtin::If => f.write_str("if"),
            Builtin::Car => f.write_str("car"),
            Builtin::Cdr => f.write_str("cdr"),
            Builtin::Cons => f.write_str("cons"),
            Builtin::Debug => f.write_str("debug"),
            Builtin::IsFalsy => f.write_str("false?"),
            Builtin::Exit => f.write_str("exit"),
            Builtin::IsEq => f.write_str("eq?"),
        }
    }
}

#[derive(PartialEq, Clone)]
pub enum SExpr {
    Atom(usize, Value),
    S(usize, Rc<SExpr>, Rc<SExpr>),
}

impl SExpr {
    pub fn nil() -> SExpr {
        SExpr::Atom(0, Value::nil())
    }

    pub fn truthy() -> SExpr {
        SExpr::Atom(0, Value::Char('t'))
    }

    pub fn is_nil(&self) -> bool {
        match self {
            SExpr::Atom(_, value) => value.is_nil(),
            SExpr::S(_, _, _) => false,
        }
    }
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

pub struct ParseContext {
    content: Vec<char>,
    pos: usize,
    builtins: HashMap<String, Builtin>,
}

impl ParseContext {
    fn new(content: &str) -> ParseContext {
        let mut builtins = HashMap::new();
        builtins.insert("nil".to_owned(), Builtin::Nil);
        builtins.insert("+".to_owned(), Builtin::Add);
        builtins.insert("-".to_owned(), Builtin::Sub);
        builtins.insert("*".to_owned(), Builtin::Mul);
        builtins.insert("/".to_owned(), Builtin::Div);
        builtins.insert("%".to_owned(), Builtin::Mod);
        builtins.insert("set".to_owned(), Builtin::Set);
        builtins.insert("setg".to_owned(), Builtin::Setg);
        builtins.insert("lambda".to_owned(), Builtin::Lambda);
        builtins.insert("do".to_owned(), Builtin::Do);
        builtins.insert("defun".to_owned(), Builtin::Defun);
        builtins.insert("putc".to_owned(), Builtin::Putc);
        builtins.insert("if".to_owned(), Builtin::If);
        builtins.insert("car".to_owned(), Builtin::Car);
        builtins.insert("cdr".to_owned(), Builtin::Cdr);
        builtins.insert("cons".to_owned(), Builtin::Cons);
        builtins.insert("debug".to_owned(), Builtin::Debug);
        builtins.insert("false?".to_owned(), Builtin::IsFalsy);
        builtins.insert("eq?".to_owned(), Builtin::IsEq);
        builtins.insert("exit".to_owned(), Builtin::Exit);
        ParseContext {
            content: content.chars().collect::<Vec<_>>(),
            pos: 0,
            builtins,
        }
    }

    fn at_end(&self) -> bool {
        self.pos >= self.content.len()
    }

    fn peek(&self) -> Option<char> {
        self.content.get(self.pos).cloned()
    }
}

pub fn parse_str(s: &str) -> ParseResult<Vec<SExpr>> {
    parse(ParseContext::new(s))
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

/// Returns true if it parsed any amount of whitespace
fn parse_ws(ctx: &mut ParseContext) -> bool {
    let mut has_ws = false;
    while !ctx.at_end() {
        let c = ctx.content[ctx.pos];
        if c == ';' {
            parse_comment(ctx);
            continue;
        }
        if c != ' ' && c != '\n' && c != '\t' {
            break;
        }
        has_ws = true;
        ctx.pos += 1;
    }
    has_ws
}

fn parse_comment(ctx: &mut ParseContext) {
    while ctx.peek() != Some('\n') {
        ctx.pos += 1;
    }
    ctx.pos += 1;
}

fn is_symbol_char(c: char) -> bool {
    match c {
        '_' => true,
        '+' => true,
        '-' => true,
        '/' => true,
        '*' => true,
        '%' => true,
        '?' => true,
        c if c.is_alphanumeric() => true,
        _ => false,
    }
}

fn is_digit_char(c: char) -> bool {
    c == '.' || c.is_ascii_digit()
}

/// Expects to not be at EOF
fn parse_expr(ctx: &mut ParseContext) -> ParseResult<SExpr> {
    if ctx.at_end() {
        return Err("Hit EOF while parsing expr".to_owned());
    }
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
        c if c.is_ascii_digit() || c == '.' => Ok(SExpr::Atom(pos, parse_num(ctx)?)),
        c if is_symbol_char(c) => Ok(SExpr::Atom(pos, parse_symbol(ctx)?)),
        c => Err(format!("Unexpected character: {c}")),
    }
}

fn parse_num(ctx: &mut ParseContext) -> ParseResult<Value> {
    let mut s = String::new();
    while let Some(c) = ctx.peek().filter(|&c| is_digit_char(c)) {
        s.push(c);
        ctx.pos += 1;
    }
    if let Ok(value) = s.parse::<i64>() {
        return Ok(Value::Int(value));
    }
    if let Ok(value) = s.parse::<f64>() {
        return Ok(Value::Float(value));
    }
    Err("Invalid integer".to_owned())
}

/// Expects the leading ? to have been parsed
fn parse_char(ctx: &mut ParseContext) -> ParseResult<Value> {
    ctx.pos += 1;
    match ctx.peek() {
        Some('#') => {
            ctx.pos += 1;
            let res = match ctx.peek() {
                Some('n') => '\n',
                Some('t') => '\t',
                Some('s') => ' ',
                _ => return Err("Invalid escape character".to_owned()),
            };
            ctx.pos += 1;
            Ok(Value::Char(res))
        },
        Some(c) => {
            ctx.pos += 1;
            Ok(Value::Char(c))
        },
        None => Err("Expected character, found EOF".to_owned()),
    }
}

/// Expects the first char to already be a double quote
fn parse_string(ctx: &mut ParseContext) -> ParseResult<SExpr> {
    let pos = ctx.pos;
    ctx.pos += 1;
    let mut chars = vec![];
    while let Some(c) = ctx.peek() {
        if c == '"' {
            ctx.pos += 1;
            let sexpr = chars.into_iter().rev().fold(SExpr::nil(), |acc, it| {
                let value = SExpr::Atom(pos, Value::Char(it));
                SExpr::S(pos, Rc::new(value), Rc::new(acc))
            });
            return Ok(SExpr::Atom(pos, Value::Quote(Rc::new(sexpr))));
        }
        chars.push(c);
        ctx.pos += 1;
    }
    Err("Hit EOF while parsing string".to_owned())
}

/// Assumes the current char is a valid symbol char
fn parse_symbol(ctx: &mut ParseContext) -> ParseResult<Value> {
    let mut s = String::new();
    while let Some(c) = ctx.peek().filter(|&c| is_symbol_char(c)) {
        s.push(c);
        ctx.pos += 1;
    }

    if let Some(builtin) = ctx.builtins.get(&s) {
        Ok(Value::Builtin(*builtin))
    } else {
        Ok(Value::Symbol(s))
    }
}

/// Assumes we know that the current char is a left paren
fn parse_sexpr(ctx: &mut ParseContext) -> ParseResult<SExpr> {
    ctx.pos += 1;
    parse_ws(ctx);
    // To allow the empty list
    if ctx.peek() == Some(')') {
        ctx.pos += 1;
        return Ok(SExpr::Atom(ctx.pos - 2, Value::Builtin(Builtin::Nil)));
    }
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
    Ok(exprs.into_iter().rev().fold(SExpr::nil(), |acc, it| {
        SExpr::S(it.0, Rc::new(it.1), Rc::new(acc))
    }))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_expr_str(s: &str) -> SExpr {
        parse_expr(&mut ParseContext::new(s)).expect("Failed parsing expr")
    }

    #[test]
    fn test_parse_num() {
        assert_eq!("1234", format!("{:?}", parse_expr_str("1234")));
        assert_eq!("3.14", format!("{:?}", parse_expr_str("3.14")));
        assert_eq!("3.0", format!("{:?}", parse_expr_str("3.000")));
    }

    #[test]
    fn test_parse_sexpr() {
        assert_eq!("(x . y)", format!("{:?}", parse_expr_str("(x . y)")));
        assert_eq!("(x . (y . nil))", format!("{:?}", parse_expr_str("(x y)")));
        assert_eq!("(x . (y . (42 . nil)))", format!("{:?}", parse_expr_str("(x y 42)")));
        assert_eq!("Ok([(x . (y . (42 . nil))), (a . b)])", format!("{:?}", parse_str(
                    "(x y 42)\n(a . b)"
                    )));
    }

    #[test]
    fn test_parse_char() {
        assert_eq!("?x", format!("{:?}", parse_expr_str("?x")));
    }

    #[test]
    fn test_parse_quote() {
        assert_eq!("'(a . b)", format!("{:?}", parse_expr_str("'(a.b)")));
        assert_eq!("'(a . b)", format!("{:?}", parse_expr_str("'(a. b)")));
        assert_eq!("'(a . b)", format!("{:?}", parse_expr_str("'(a   .   b)")));
        assert_eq!("'?a", format!("{:?}", parse_expr_str("'?a")));
    }

    #[test]
    fn test_parse_string() {
        assert_eq!("(?a . (?b . (?c . nil)))", format!("{:?}", parse_expr_str("\"abc\"")));
    }
}
