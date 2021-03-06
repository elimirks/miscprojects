use std::collections::HashMap;
use std::rc::Rc;
use std::fmt;

enum Token {
    Assign,
    Add,
    Sub,
    Var(Rc<String>),
    Val(f32)
}

fn get_line() -> String {
    let mut buffer = String::new();
    std::io::stdin().read_line(&mut buffer).expect("Failed");
    return buffer;
}

fn is_decimal(s: &String) -> bool {
    s.parse::<f32>().is_ok()
}

fn is_alpha(s: &String) -> bool {
    for c in s.chars() {
        if ! c.is_alphabetic() {
            return false;
        }
    }
    return true;
}

fn tokenize(s: String) -> Token {
    return if is_decimal(&s) {
        Token::Val(s.parse().unwrap())
    } else if is_alpha(&s) {
        Token::Var(Rc::new(s))
    } else if s == "+" {
        Token::Add
    } else if s == "-" {
        Token::Sub
    } else if s == "=" {
        Token::Assign
    } else {
        panic!("crash and burn");
    }
}

enum Expr {
    Add(Rc<Expr>, Rc<Expr>),
    Sub(Rc<Expr>, Rc<Expr>),
    Val(f32),
    Var(Rc<String>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Add(first, second) => write!(f, "({} + {})", first, second),
            Expr::Sub(first, second) => write!(f, "({} - {})", first, second),
            Expr::Val(val) => write!(f, "{}", val),
            Expr::Var(name) => write!(f, "{}", name),
        }
    }
}

fn evaluate(expr: &Expr, vars: &HashMap<Rc<String>, Rc<Expr>>) -> f32 {
    match expr {
        Expr::Val(value) => *value,
        Expr::Add(first, second) => evaluate(&first, vars) + evaluate(&second, vars),
        Expr::Sub(first, second) => evaluate(&first, vars) - evaluate(&second, vars),
        Expr::Var(name) => match vars.get(name) {
            Some(expr) => evaluate(&expr, vars),
            None => panic!("{} is undefined", name)
        }
    }
}

fn to_expr<'a>(tokens: &mut impl Iterator<Item = &'a Token>) -> Option<Rc<Expr>> {
    let mut parse_stack: Vec<Rc<Expr>> = Vec::new();

    while let Some(current) = tokens.next() {
        match current {
            Token::Var(name) => {
                if parse_stack.is_empty() {
                    parse_stack.push(Rc::new(Expr::Var(Rc::clone(name))));
                } else {
                    panic!("Syntax error");
                }
            },
            Token::Val(value) => {
                if parse_stack.is_empty() {
                    parse_stack.push(Rc::new(Expr::Val(*value)));
                } else {
                    panic!("Syntax error");
                }
            },
            Token::Add => {
                let prev_expr = match parse_stack.pop() {
                    Some(expr) => expr,
                    None => panic!("Syntax error")
                };
                let next_expr = match to_expr(tokens) {
                    Some(expr) => expr,
                    None => panic!("Syntax error")
                };
                parse_stack.push(Rc::new(Expr::Add(prev_expr, next_expr)));
            },
            Token::Sub => {
                let prev_expr = match parse_stack.pop() {
                    Some(expr) => expr,
                    None => panic!("Syntax error")
                };
                let next_expr = match to_expr(tokens) {
                    Some(expr) => expr,
                    None => panic!("Syntax error")
                };
                parse_stack.push(Rc::new(Expr::Sub(prev_expr, next_expr)));
            },
            Token::Assign => {
                panic!("Cannot assign within an expression");
            },
        }
    }

    return parse_stack.pop();
}

fn is_stmt_assignment(tokens: &Vec<Token>) -> bool {
    return match (tokens.get(0), tokens.get(1)) {
        (Some(Token::Var(_)), Some(Token::Assign)) => true,
        _                                          => false,
    }
}

fn main() {
    let mut vars: HashMap<Rc<String>, Rc<Expr>> = HashMap::new();

    loop {
        let tokens: Vec<Token> = get_line().split_ascii_whitespace()
            .map(String::from).map(tokenize).collect();

        if is_stmt_assignment(&tokens) {
            if let Some(Token::Var(name)) = tokens.get(0) {
                let mut it = tokens.iter();
                // Skip over the var name and assignment operator
                it.next();
                it.next();

                if let Some(expr) = to_expr(&mut it) {
                    vars.insert(Rc::clone(&name), expr);
                } else {
                    panic!("Something terrible has happened");
                }
            }
        } else {
            if let Some(expr) = to_expr(&mut tokens.iter()) {
                println!("{} = {}", expr, evaluate(&expr, &vars));
            }
        }
    }
}
