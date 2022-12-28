use std::{cell::RefCell, rc::Rc, collections::HashMap};

use crate::parser::*;

type RunResult<T> = Result<T, String>;

#[derive(Debug)]
struct Scope {
    values: HashMap<String, Rc<SExpr>>,
    parent: Option<Rc<RefCell<Scope>>>,
}

impl Scope {
    fn new(parent: Option<Rc<RefCell<Scope>>>) -> Scope {
        Scope {
            values: HashMap::new(),
            parent,
        }
    }

    fn lookup(&self, name: &String) -> Rc<SExpr> {
        if let Some(value) = self.values.get(name) {
            value.clone()
        } else if let Some(parent) = &self.parent {
            parent.borrow().lookup(name)
        } else {
            Rc::new(SExpr::nil())
        }
    }

    fn insert(&mut self, name: &str, value: Rc<SExpr>) {
        self.values.insert(name.to_string(), value);
    }
}

#[derive(Debug)]
struct RunContext {
    scope: Rc<RefCell<Scope>>,
}

impl RunContext {
    fn root_scope(&self) -> Rc<RefCell<Scope>> {
        let mut root = self.scope.clone();
        while root.borrow().parent.is_some() {
            let parent = root.borrow().parent.clone().unwrap();
            root = parent;
        }
        root
    }
}

impl RunContext {
    fn new() -> Self {
        RunContext {
            scope: Rc::new(RefCell::new(Scope::new(None))),
        }
    }
}

pub fn run(root_sexprs: Vec<SExpr>) -> RunResult<()> {
    let mut context = RunContext::new();
    let exprs = root_sexprs.into_iter().map(|sexpr| {
        Rc::new(sexpr)
    }).collect::<Vec<_>>();
    eval_do(&mut context, &exprs)?;
    Ok(())
}

fn eval(ctx: &mut RunContext, expr: Rc<SExpr>) -> RunResult<Rc<SExpr>> {
    match &*expr {
        SExpr::Atom(_, value) => {
            match value {
                Value::Symbol(name) => {
                    let value = ctx.scope.borrow().lookup(name);
                    eval(ctx, value)
                },
                _ => Ok(expr.clone()),
            }
        },
        SExpr::S(_, lhs, rhs) => {
            let func = eval(ctx, lhs.clone())?;
            call(ctx, func, rhs.clone())
        },
    }
}

// The result is the last evaluated expr
fn eval_do(ctx: &mut RunContext, exprs: &Vec<Rc<SExpr>>) -> RunResult<Rc<SExpr>> {
    for expr in exprs.iter().take(exprs.len() - 1) {
        eval(ctx, expr.clone())?;
    }
    eval(ctx, exprs.last().unwrap().clone())
}

fn call(ctx: &mut RunContext, func: Rc<SExpr>, params: Rc<SExpr>) -> RunResult<Rc<SExpr>> {
    match &*func {
        SExpr::Atom(_, value) => {
            match value {
                Value::Builtin(bi) => call_builtin(ctx, *bi, params),
                Value::Symbol(name) => {
                    let func = ctx.scope.borrow().lookup(name);
                    call(ctx, func, params)
                },
                Value::Function(args, body) => {
                    let unfolded = unfold(params);
                    if unfolded.len() != args.len() {
                        return Err("Function call parameter length mismatch".to_owned());
                    }
                    let parent_scope = ctx.scope.clone();
                    let mut fun_scope = Scope::new(Some(parent_scope.clone()));
                    for (arg, param) in args.iter().zip(unfolded.iter()) {
                        fun_scope.insert(arg, param.clone());
                    }
                    ctx.scope = Rc::new(RefCell::new(fun_scope));
                    println!("Push!");
                    println!("{ctx:?}");
                    let result = eval_do(ctx, body)?;
                    ctx.scope = parent_scope;
                    println!("Pop!");
                    Ok(result)
                },
                _ => Err(format!("{value:?} is not callable"))
            }
        },
        // NOTE: This might just be a matter of evaluating the LHS and recursing
        SExpr::S(_, _, _) => todo!(),
    }
}

/// Unfolds an sexpr list into a Vec
/// Unfolding will terminate when it hits an atom value in the rhs
/// If a nil is the terminal element, it isn't included in the return vec
fn unfold(sexpr: Rc<SExpr>) -> Vec<Rc<SExpr>> {
    let mut values = vec![];
    let mut current = sexpr;
    while let SExpr::S(_, lhs, rhs) = &*current {
        values.push(lhs.clone());
        current = rhs.clone();
    }
    if !current.is_nil() {
        values.push(current);
    }
    values
}

fn call_builtin(ctx: &mut RunContext, func: Builtin, folded_params: Rc<SExpr>) -> RunResult<Rc<SExpr>> {
    let mut params = unfold(folded_params);
    match func {
        Builtin::Lambda => eval_lambda(params),
        Builtin::Defun => {
            if params.len() < 3 {
                return Err("defun must have at least three params".to_owned());
            }
            if is_symbol(&params[0]) {
                let fun_sym = params[0].clone();
                params.remove(0);
                let fun = eval_lambda(params.clone())?;
                eval_set(ctx, ctx.root_scope(), vec![fun_sym, fun])
            } else {
                Err("The first param to defun be a symbol".to_owned())
            }
        },
        Builtin::Nil => Ok(Rc::new(SExpr::nil())),
        Builtin::Set => eval_set(ctx, ctx.scope.clone(), params),
        Builtin::Setg => eval_set(ctx, ctx.root_scope(), params),
        Builtin::Do => eval_do(ctx, &params),
        Builtin::Putc => {
            if params.len() != 1 {
                return Err("putc must accept exactly 1 char argument".to_owned());
            }
            let param = eval(ctx, params[0].clone())?; 
            if let Some(c) = get_expr_char(&param) {
                print!("{c}");
                Ok(Rc::new(SExpr::nil()))
            } else {
                Err("putc must accept exactly 1 char argument".to_owned())
            }
        },
        Builtin::Add | Builtin::Sub | Builtin::Mul | Builtin::Div | Builtin::Mod => {
            if params.len() != 2 {
                return Err(format!("Call to {func:?} must have two params"));
            }
            let lhs = eval(ctx, params[0].clone())?;
            let rhs = eval(ctx, params[1].clone())?;
            match (&*lhs, &*rhs) {
                (SExpr::Atom(_, lhs), SExpr::Atom(_, rhs)) =>
                    Ok(Rc::new(SExpr::Atom(0, eval_arithmetic(func, lhs.clone(), rhs.clone())?))),
                _ => Err(format!("{func:?} must be called on two values of the same type")),
            }
        },
    }
}

fn get_expr_char(expr: &SExpr) -> Option<char> {
    match expr {
        SExpr::Atom(_, Value::Char(c)) => Some(*c),
        _ => None,
    }
}

fn eval_lambda(mut params: Vec<Rc<SExpr>>) -> RunResult<Rc<SExpr>> {
    if params.len() < 2 {
        return Err("function definitions must have at least two params".to_owned());
    }
    let mut arg_names = vec![];
    for arg in unfold(params[0].clone()).iter() {
        if let Some(name) = get_symbol_name(arg) {
            arg_names.push(name.clone());
        } else {
            return Err("The first param of a function definition must by a symbol list".to_owned());
        }
    }
    params.remove(0);
    let f = Value::Function(arg_names, params);
    Ok(Rc::new(SExpr::Atom(0, f)))
}

fn eval_set(ctx: &mut RunContext, scope: Rc<RefCell<Scope>>, params: Vec<Rc<SExpr>>) -> RunResult<Rc<SExpr>> {
    if params.len() != 2 {
        return Err("set must be called on exactly two parameters".to_owned());
    }
    if let Some(name) = get_symbol_name(&params[0]) {
        let set_value = eval(ctx, params[1].clone())?;
        scope.borrow_mut().insert(name, set_value.clone());
        Ok(set_value)
    } else {
        Err("The first param to `set` must be a symbol".to_owned())
    }
}

fn get_symbol_name(sexpr: &SExpr) -> Option<&String> {
    match sexpr {
        SExpr::Atom(_, value) => match value {
            Value::Symbol(name) => Some(name),
            _ => None,
        },
        SExpr::S(_, _, _) => None,
    }
}

fn is_symbol(sexpr: &SExpr) -> bool {
    get_symbol_name(sexpr).is_some()
}

// Assumes the given builtin is a valid arithmetic op
fn eval_arithmetic(builtin: Builtin, lhs: Value, rhs: Value) -> RunResult<Value> {
    match (lhs, rhs) {
        (Value::Int(lhs), Value::Int(rhs))     => Ok(eval_arithmetic_int(builtin, lhs, rhs)),
        (Value::Int(lhs), Value::Float(rhs))   => Ok(eval_arithmetic_float(builtin, lhs as f64, rhs)),
        (Value::Float(lhs), Value::Int(rhs))   => Ok(eval_arithmetic_float(builtin, lhs, rhs as f64)),
        (Value::Float(lhs), Value::Float(rhs)) => Ok(eval_arithmetic_float(builtin, lhs, rhs)),
        (lhs, rhs) if lhs.is_nil() || rhs.is_nil() => {
            Err("Cannot perform arithmetic on nil".to_owned())
        },
        _ => unreachable!()
    }
}

fn eval_arithmetic_int(builtin: Builtin, lhs: i64, rhs: i64) -> Value {
    Value::Int(match builtin {
        Builtin::Add => lhs + rhs,
        Builtin::Sub => lhs - rhs,
        Builtin::Mul => lhs * rhs,
        Builtin::Div => lhs / rhs,
        Builtin::Mod => lhs % rhs,
        _ => unreachable!()
    })
}

fn eval_arithmetic_float(builtin: Builtin, lhs: f64, rhs: f64) -> Value {
    Value::Float(match builtin {
        Builtin::Add => lhs + rhs,
        Builtin::Sub => lhs - rhs,
        Builtin::Mul => lhs * rhs,
        Builtin::Div => lhs / rhs,
        Builtin::Mod => lhs % rhs,
        _ => unreachable!()
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    // Returns the last evaluated expr
    fn eval_str(s: &str) -> Rc<SExpr> {
        let exprs = parse_str(s).unwrap()
            .into_iter()
            .map(Rc::new)
            .collect::<Vec<_>>();
        let mut ctx = RunContext::new();
        eval_do(&mut ctx, &exprs).unwrap()
    }

    #[test]
    fn test_eval_arithmetic() {
        assert_eq!("4", format!("{:?}", eval_str("(+ 1 3)")));
        assert_eq!("-2", format!("{:?}", eval_str("(- 1 3)")));
        assert_eq!("17", format!("{:?}", eval_str("(+ 1 (+ 7 9))")));
        assert_eq!("2", format!("{:?}", eval_str("(/ 5 2)")));
        assert_eq!("8", format!("{:?}", eval_str("(* 4 2)")));
        assert_eq!("0", format!("{:?}", eval_str("(/ 1 (* 7 9))")));
        assert_eq!("0.0", format!("{:?}", eval_str("(* 0.0 7)")));
        assert_eq!("0.125", format!("{:?}", eval_str("(/ 1 (* 4.0 2))")));
        assert_eq!("2", format!("{:?}", eval_str("(% 42 5)")));
    }

    #[test]
    fn test_eval_do() {
        assert_eq!("2", format!("{:?}", eval_str("(do (% 42 5))")));
        assert_eq!("2", format!("{:?}", eval_str("(do 6 nil (+ 4 2) (% 42 5))")));
        assert_eq!("nil", format!("{:?}", eval_str("(do nil)")));
    }

    #[test]
    fn test_eval_set() {
        assert_eq!("8", format!("{:?}", eval_str("(set x 4) (+ x x)")));
    }

    #[test]
    fn test_eval_fun() {
        assert_eq!("3", format!("{:?}", eval_str("(defun add (a b) (+ a b)) (add 1 2)")));
        assert_eq!("3", format!("{:?}", eval_str("((lambda (a b) (+ a b)) 1 2)")));
    }
}
