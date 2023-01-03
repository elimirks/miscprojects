use std::{fmt::Debug, rc::Rc};

type ParseResult<T> = Result<T, String>;

// FIXME: Cons should be a value type, not an expr type
// Then, we know that all exprs must be evaluated, and all values are
// done being evalutaed.
// Quoting things will be more accurate that way.
#[derive(Clone)]
pub enum Value {
    Symbol(String),
    Builtin(Builtin),
    Int(i64),
    Float(f64),
    String(String),
    // Special data type for storing wave data
    // More efficient operations can be done on wavedata, so that we won't be
    // crippled by the poor performance of my Lisp implementation.
    WaveData(Vec<f64>),
    Function(Vec<String>, Vec<Rc<SExpr>>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Symbol(lhs), Value::Symbol(rhs)) => lhs == rhs,
            (Value::Builtin(lhs), Value::Builtin(rhs)) => lhs == rhs,
            (Value::Int(lhs), Value::Int(rhs)) => lhs == rhs,
            (Value::Float(lhs), Value::Float(rhs)) => lhs == rhs,
            (Value::Function(lhs_params, lhs_body), Value::Function(rhs_params, rhs_body)) => {
                if lhs_params.len() != rhs_params.len() || lhs_body.len() != rhs_body.len() {
                    false
                } else {
                    lhs_params.iter().zip(rhs_params).all(|(l, r)| l == r)
                }
            },
            _ => false,
        }
    }
}

impl Eq for Value {
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
            Value::Symbol(value) => f.write_str(value),
            Value::String(value) => f.write_str(&format!("\"{value}\"")),
            Value::Int(value)    => f.write_str(&value.to_string()),
            Value::Float(value)  => {
                if (value % 1.0).abs() == 0.0 {
                    f.write_str(&format!("{}.0", value.floor()))
                } else {
                    f.write_str(&value.to_string())
                }
            },
            Value::Function(args, body) => {
                f.write_str("(lambda (")?;
                f.write_str(&args.join(" "))?;
                f.write_str(") ")?;
                let body_strs = body.iter().map(|expr| format!("{expr:?}")).collect::<Vec<_>>();
                f.write_str(&body_strs.join(" "))?;
                f.write_str(")")?;
                Ok(())
            },
            Value::WaveData(data) => f.write_str(&format!("WaveData([...; {}])", data.len())),
            Value::Builtin(value) => value.fmt(f),
        }
    }
}

static BUILTIN_NAMES: &[(&str, Builtin)] = &[
    ("nil", Builtin::Nil),
    ("quote", Builtin::Quote),
    ("list", Builtin::List),
    ("+", Builtin::Add),
    ("-", Builtin::Sub),
    ("*", Builtin::Mul),
    ("/", Builtin::Div),
    ("%", Builtin::Mod),
    ("set", Builtin::Set),
    ("setg", Builtin::Setg),
    ("lambda", Builtin::Lambda),
    ("progn", Builtin::Progn),
    ("defun", Builtin::Defun),
    ("putc", Builtin::Putc),
    ("if", Builtin::If),
    ("car", Builtin::Car),
    ("cdr", Builtin::Cdr),
    ("cons", Builtin::Cons),
    ("debug", Builtin::Debug),
    ("false?", Builtin::IsFalsy),
    ("eq?", Builtin::IsEq),
    ("cmp", Builtin::Cmp),
    ("exit", Builtin::Exit),
    ("require", Builtin::Require),
    ("to-string", Builtin::ToString),
    ("str-as-list", Builtin::StrAsList),
    ("list-as-str", Builtin::ListAsStr),
    ("to-int", Builtin::ToInt),
    ("pow", Builtin::Pow),
    ("wd-pure-tone", Builtin::WdPureTone),
    ("wd-save", Builtin::WdSave),
    ("wd-play", Builtin::WdPlay),
    ("wd-flat-amplitude", Builtin::WdFlatAmplitude),
    ("wd-multiply", Builtin::WdMultiply),
    ("wd-superimpose", Builtin::WdSuperimpose),
    ("wd-superimpose-insert", Builtin::WdSuperimposeInsert),
    ("wd-len", Builtin::WdLen),
    ("wd-concat", Builtin::WdConcat),
    ("wd-noise", Builtin::WdNoise),
    ("wd-subsample", Builtin::WdSubSample),
    ("wd-slope-up", Builtin::WdSlopeUp),
    ("wd-reverse", Builtin::WdReverse),
    ("wd-plot", Builtin::WdPlot),
    ("wd-shifting-pure-tone", Builtin::WdShiftingPureTone),
    ("wd-spline", Builtin::WdSpline),
];

fn builtin_for_name(name: &str) -> Option<Builtin> {
    BUILTIN_NAMES.iter()
        .find(|(n, _)| *n == name)
        .map(|(_, builtin)| *builtin)
}

fn name_for_builtin(builtin: Builtin) -> String {
    BUILTIN_NAMES.iter()
        .find(|(_, b)| *b == builtin)
        .map(|(n, _)| n.to_string())
        .unwrap()
}

#[derive(Eq, PartialEq, Clone, Copy)]
pub enum Builtin {
    Quote,
    List,
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
    Progn,
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
    Cmp,
    Exit,
    Require,
    ToString,
    StrAsList,
    ListAsStr,
    WdPureTone,
    WdSave,
    WdPlay,
    WdFlatAmplitude,
    WdMultiply,
    WdSuperimpose,
    WdSuperimposeInsert,
    WdLen,
    WdConcat,
    WdNoise,
    WdSubSample,
    WdSlopeUp,
    WdReverse,
    WdPlot,
    WdShiftingPureTone,
    WdSpline,
    ToInt,
    Pow,
}

impl Debug for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&name_for_builtin(*self))
    }
}

#[derive(Eq, Clone)]
pub enum SExpr {
    Atom(Value),
    // An S-expr is evaluated, cons ain't
    S(Rc<SExpr>, Rc<SExpr>),
    Cons(Rc<SExpr>, Rc<SExpr>),
}

impl SExpr {
    pub fn nil() -> SExpr {
        SExpr::Atom(Value::nil())
    }

    pub fn truthy() -> SExpr {
        SExpr::Atom(Value::Int(1))
    }

    pub fn is_nil(&self) -> bool {
        match self {
            SExpr::Atom(value) => value.is_nil(),
            _ => false,
        }
    }

    pub fn quote(&self) -> Self {
        let quote_expr = Rc::new(SExpr::Atom(Value::Builtin(Builtin::Quote)));
        SExpr::S(quote_expr, Rc::new(self.clone()))
    }

    pub fn is_quoted(&self) -> bool {
        match self {
            SExpr::S(car, _) =>
                matches!(car.atom_value(), Some(Value::Builtin(Builtin::Quote))),
            _ => false,
        }
    }

    pub fn rhs(&self) -> Option<Rc<SExpr>> {
        match self {
            SExpr::Atom(_) => None,
            SExpr::S(_, rhs) => Some(rhs.clone()),
            SExpr::Cons(_, rhs) => Some(rhs.clone()),
        }
    }

    pub fn atom_value(&self) -> Option<Value> {
        match self {
            SExpr::Atom(value) => Some(value.clone()),
            _ => None,
        }
    }

    pub fn is_atom(&self) -> bool {
        matches!(self, SExpr::Atom(_))
    }
}

impl PartialEq for SExpr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (SExpr::Atom(lhs), SExpr::Atom(rhs)) => lhs == rhs,
            (SExpr::S(lhs_car, lhs_cdr), SExpr::S(rhs_car, rhs_cdr)) => {
                lhs_car == rhs_car && lhs_cdr == rhs_cdr
            },
            (SExpr::Cons(lhs_car, lhs_cdr), SExpr::Cons(rhs_car, rhs_cdr)) => {
                lhs_car == rhs_car && lhs_cdr == rhs_cdr
            },
            _ => false,
        }
    }
}

impl Debug for SExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SExpr::Atom(value) => value.fmt(f),
            SExpr::S(lhs, rhs) => {
                f.write_str(&format!("({:?} . {:?})", lhs, rhs))
            },
            SExpr::Cons(lhs, rhs) => {
                f.write_str(&format!("({:?} . {:?})", lhs, rhs))
            },
        }
    }
}

pub struct ParseContext {
    content: Vec<char>,
    pos: usize,
}

impl ParseContext {
    fn new(content: &str) -> ParseContext {
        ParseContext {
            content: content.chars().collect::<Vec<_>>(),
            pos: 0,
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
    match ctx.content[ctx.pos] {
        '\'' => {
            ctx.pos += 1;
            Ok(parse_expr(ctx)?.quote())
        },
        '\"' => parse_string(ctx),
        '?' => Ok(SExpr::Atom(parse_char(ctx)?)),
        '(' => parse_sexpr(ctx),
        c if c.is_ascii_digit() || c == '-' => Ok(SExpr::Atom(parse_num(ctx)?)),
        c if is_symbol_char(c) => Ok(SExpr::Atom(parse_symbol(ctx)?)),
        c => Err(format!("Unexpected character: {c}")),
    }
}

fn parse_num(ctx: &mut ParseContext) -> ParseResult<Value> {
    let is_neg = if ctx.peek() == Some('-') {
        ctx.pos += 1;
        true
    } else {
        false
    };
    let mut s = String::new();
    while let Some(c) = ctx.peek().filter(|&c| is_digit_char(c)) {
        s.push(c);
        ctx.pos += 1;
    }
    // Must have been a lone minus symbol - try parsing as a symbol instead
    if s.is_empty() {
        ctx.pos -= 1;
        parse_symbol(ctx)
    } else if let Ok(value) = s.parse::<i64>() {
        Ok(Value::Int(if is_neg { -value } else { value }))
    } else if let Ok(value) = s.parse::<f64>() {
        Ok(Value::Float(if is_neg { -value } else { value }))
    } else {
        Err("Invalid integer".to_owned())
    }
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
            Ok(Value::Int(res as i64))
        },
        Some(c) => {
            ctx.pos += 1;
            Ok(Value::Int(c as i64))
        },
        None => Err("Expected character, found EOF".to_owned()),
    }
}

/// Expects the first char to already be a double quote
fn parse_string(ctx: &mut ParseContext) -> ParseResult<SExpr> {
    ctx.pos += 1;
    let mut s = String::new();
    while let Some(c) = ctx.peek() {
        if c == '"' {
            ctx.pos += 1;
            return Ok(SExpr::Atom(Value::String(s)));
        }
        s.push(c);
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

    if let Some(builtin) = builtin_for_name(&s) {
        Ok(Value::Builtin(builtin))
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
        return Ok(SExpr::Atom(Value::Builtin(Builtin::Nil)));
    }
    let lhs = parse_expr(ctx)?;
    parse_ws(ctx);
    if ctx.peek() == Some('.') {
        ctx.pos += 1;
        parse_ws(ctx);
        let rhs = parse_expr(ctx)?;
        parse_ws(ctx);
        if ctx.peek() != Some(')') {
            return Err("Expected )".to_owned());
        }
        ctx.pos += 1;
        return Ok(SExpr::S(Rc::new(lhs), Rc::new(rhs)));
    }
    // Otherwise, assume we're using the list style SExpr syntax
    let mut exprs = vec![lhs];
    while ctx.peek() != Some(')') {
        exprs.push(parse_expr(ctx)?);
        parse_ws(ctx);
    }
    ctx.pos += 1;
    Ok(exprs.into_iter().rev().fold(SExpr::nil(), |acc, it| {
        SExpr::S(Rc::new(it), Rc::new(acc))
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
        assert_eq!(SExpr::Atom(Value::Int(1234)), parse_expr_str("1234"));
        assert_eq!(SExpr::Atom(Value::Float(3.14)), parse_expr_str("3.14"));
        assert_eq!(SExpr::Atom(Value::Float(3.0)), parse_expr_str("3.0"));
        assert_eq!(SExpr::Atom(Value::Int(-1234)), parse_expr_str("-1234"));
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
        assert_eq!("120", format!("{:?}", parse_expr_str("?x")));
    }

    #[test]
    fn test_parse_quote() {
        assert_eq!("(quote . (a . b))", format!("{:?}", parse_expr_str("'(a.b)")));
        assert_eq!("(quote . (a . b))", format!("{:?}", parse_expr_str("'(a. b)")));
        assert_eq!("(quote . (a . b))", format!("{:?}", parse_expr_str("'(a   .   b)")));
        assert_eq!("(quote . 97)", format!("{:?}", parse_expr_str("'?a")));
        assert_eq!("(quote . (1 . nil))", format!("{:?}", parse_expr_str("'(1)")));
        assert_eq!("(quote . (quote . (1 . nil)))", format!("{:?}", parse_expr_str("''(1)")));
        assert_eq!("(quote . (quote . 1))", format!("{:?}", parse_expr_str("''1")));
        assert_eq!("(quote . foo)", format!("{:?}", parse_expr_str("'foo")));
    }

    #[test]
    fn test_parse_string() {
        assert_eq!("\"abc\"", format!("{:?}", parse_expr_str("\"abc\"")));
    }
}
