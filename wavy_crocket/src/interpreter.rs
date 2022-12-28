use std::{cell::RefCell, rc::Rc, collections::HashMap};

use crate::parser::*;

type RunResult<T> = Result<T, String>;

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
            return value.clone();
        } else if let Some(parent) = &self.parent {
            parent.borrow().lookup(name)
        } else {
            Rc::new(SExpr::Atom(0, Value::Nil))
        }
    }
}

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

fn run(root_sexprs: Vec<SExpr>) -> RunContext {
    let mut context = RunContext::new();
    context
}

fn eval(ctx: &mut RunContext, expr: Rc<SExpr>) -> RunResult<Rc<SExpr>> {
    match &*expr {
        SExpr::Atom(_, value) => {
            Ok(match value {
                Value::Symbol(name) => ctx.scope.borrow().lookup(name),
                _ => expr.clone(),
            })
        },
        SExpr::S(_, lhs, rhs) => {
            let func = eval(ctx, lhs.clone())?;
            call(ctx, func, rhs.clone())
        },
    }
}

fn call(ctx: &mut RunContext, func: Rc<SExpr>, params: Rc<SExpr>) -> RunResult<Rc<SExpr>> {
    match &*func {
        SExpr::Atom(_, value) => {
            match value {
                Value::Bulitin(bi) => call_builtin(ctx, *bi, params),
                _ => Err(format!("{value:?} is not callable"))
            }
        },
        SExpr::S(_, _, _) => todo!(),
    }
}

// Unfolds an sexpr list into a Vec
// Unfolding will terminate when it hits an atom value in the rhs
fn unfold(sexpr: Rc<SExpr>) -> Vec<Rc<SExpr>> {
    let mut values = vec![];
    let mut current = sexpr;
    while let SExpr::S(_, lhs, rhs) = &*current {
        values.push(lhs.clone());
        current = rhs.clone();
    }
    if *current != SExpr::Atom(0, Value::Nil) {
        values.push(current);
    }
    values
}

fn call_builtin(ctx: &mut RunContext, func: Builtin, folded_params: Rc<SExpr>) -> RunResult<Rc<SExpr>> {
    let mut params = unfold(folded_params);
    match func {
        Builtin::Nil => Ok(Rc::new(SExpr::Atom(0, Value::Nil))),
        Builtin::Set => todo!(),
        Builtin::Setg => todo!(),
        Builtin::Fun => todo!(),
        Builtin::Add | Builtin::Sub | Builtin::Mul | Builtin::Div | Builtin::Mod => {
            if params.len() != 2 {
                println!("{params:?}");
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

// Assumes the given builtin is a valid arithmetic op
fn eval_arithmetic(builtin: Builtin, lhs: Value, rhs: Value) -> RunResult<Value> {
    match (lhs, rhs) {
        (Value::Int(lhs), Value::Int(rhs))     => Ok(eval_arithmetic_int(builtin, lhs, rhs)),
        (Value::Int(lhs), Value::Float(rhs))   => Ok(eval_arithmetic_float(builtin, lhs as f64, rhs)),
        (Value::Float(lhs), Value::Int(rhs))   => Ok(eval_arithmetic_float(builtin, lhs, rhs as f64)),
        (Value::Float(lhs), Value::Float(rhs)) => Ok(eval_arithmetic_float(builtin, lhs, rhs)),
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

    fn eval_str(s: &str) -> Rc<SExpr> {
        let expr = Rc::new(parse_expr_str(s));
        eval(&mut RunContext::new(), expr).unwrap()
    }

    #[test]
    fn test_eval_arithmetic() {
        assert_eq!("4", format!("{:?}", eval_str("(+ 1 3)")));
        assert_eq!("-2", format!("{:?}", eval_str("(- 1 3)")));
        assert_eq!("17", format!("{:?}", eval_str("(+ 1 (+ 7 9))")));
        assert_eq!("2", format!("{:?}", eval_str("(/ 5 2)")));
        assert_eq!("8", format!("{:?}", eval_str("(* 4 2)")));
        assert_eq!("0", format!("{:?}", eval_str("(/ 1 (* 7 9))")));
        assert_eq!("0.125", format!("{:?}", eval_str("(/ 1 (* 4.0 2))")));
        assert_eq!("2", format!("{:?}", eval_str("(% 42 5)")));
    }
}
