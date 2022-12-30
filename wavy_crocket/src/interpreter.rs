use std::{fmt::Debug, cell::RefCell, rc::Rc, collections::{HashMap, HashSet}, process::exit, f64::consts::PI};

use crate::parser::*;
use crate::wave_file_handler::save_wave;

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

    fn lookup(&self, name: &str) -> Rc<SExpr> {
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

struct RunContext {
    scope: Rc<RefCell<Scope>>,
    required_paths: HashSet<String>,
}

impl RunContext {
    fn new() -> Self {
        RunContext {
            scope: Rc::new(RefCell::new(Scope::new(None))),
            required_paths: HashSet::new(),
        }
    }

    fn root_scope(&self) -> Rc<RefCell<Scope>> {
        let mut root = self.scope.clone();
        while root.borrow().parent.is_some() {
            let parent = root.borrow().parent.clone().unwrap();
            root = parent;
        }
        root
    }
}

impl Debug for RunContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("RunContext:\n")?;
        let mut frame_index = 0;
        let mut current = Some(self.scope.clone());
        while let Some(scope) = current {
            f.write_str(&format!("Stack frame {frame_index}:\n"))?;
            for (name, value) in scope.borrow().values.iter() {
                f.write_str(&format!("{name} -> {value:?}\n"))?;
            }
            current = scope.borrow().parent.clone();
            frame_index += 1;
        }
        Ok(())
    }
}

pub fn run(file_path: &str) -> RunResult<()> {
    run_with_context(&mut RunContext::new(), file_path)
}

fn run_with_context(ctx: &mut RunContext, file_path: &str) -> RunResult<()> {
    let cwd = std::env::current_dir()
        .expect("Can't find cwd!");
    let path = format!("{}/{file_path}.lisp", cwd.to_str().unwrap());
    if ctx.required_paths.contains(&path) {
        return Ok(());
    }
    ctx.required_paths.insert(path.clone());
    if let Ok(content) = std::fs::read_to_string(&path) {
        let root_exprs = parse_str(&content)?;
        let exprs = root_exprs.into_iter().map(|sexpr| {
            Rc::new(sexpr)
        }).collect::<Vec<_>>();
        eval_progn(ctx, &exprs)?;
        Ok(())
    } else {
        Err(format!("Failed reading {path}"))
    }
}

fn eval(ctx: &mut RunContext, expr: Rc<SExpr>) -> RunResult<Rc<SExpr>> {
    match &*expr {
        SExpr::Atom(value) => {
            match value {
                Value::Symbol(name) => {
                    let value = ctx.scope.borrow().lookup(name);
                    assert!(get_symbol_name(&value) != Some(name));
                    eval(ctx, value)
                },
                _ => Ok(expr.clone()),
            }
        },
        SExpr::S(car, cdr) => {
            let callee = eval(ctx, car.clone())?;
            if callee.atom_value() == Some(Value::Builtin(Builtin::Quote)) {
                return Ok(expr);
            }
            let should_eval_elems = match callee.atom_value() {
                // These take care of their own evaluation
                Some(Value::Builtin(builtin)) => {
                    !matches!(builtin, Builtin::Defun | Builtin::Lambda | Builtin::If)
                },
                _ => true,
            };
            let params = if should_eval_elems {
                let mut params = vec![];
                for sub in unfold(cdr.clone()).into_iter() {
                    params.push(eval(ctx, sub)?);
                }
                params
            } else {
                unfold(cdr.clone())
            };
            call(ctx, callee, &params)
        },
    }
}

// The result is the last evaluated expr
fn eval_progn(ctx: &mut RunContext, exprs: &[Rc<SExpr>]) -> RunResult<Rc<SExpr>> {
    if exprs.is_empty() {
        return Ok(Rc::new(SExpr::nil()));
    }
    for expr in exprs.iter().take(exprs.len() - 1) {
        eval(ctx, expr.clone())?;
    }
    eval(ctx, exprs.last().unwrap().clone())
}

fn call(ctx: &mut RunContext, func: Rc<SExpr>, params: &[Rc<SExpr>]) -> RunResult<Rc<SExpr>> {
    match &*func {
        SExpr::Atom(value) => {
            match value {
                Value::Builtin(bi) => call_builtin(ctx, *bi, params),
                Value::Function(args, body) => {
                    if params.len() != args.len() {
                        return Err("Function call parameter length mismatch".to_owned());
                    }
                    let parent_scope = ctx.scope.clone();
                    let mut fun_scope = Scope::new(Some(parent_scope.clone()));
                    for (name, param) in args.iter().zip(params.iter()) {
                        fun_scope.insert(name, param.clone());
                    }
                    ctx.scope = Rc::new(RefCell::new(fun_scope));
                    let result = eval_progn(ctx, body)?;
                    ctx.scope = parent_scope;
                    Ok(result)
                },
                _ => Err(format!("{value:?} is not callable")),
            }
        },
        SExpr::S(_, _) => unreachable!(),
    }
}

/// Unfolds an sexpr list into a Vec
/// Unfolding will terminate when it hits an atom value in the rhs
/// If a nil is the terminal element, it isn't included in the return vec
fn unfold(sexpr: Rc<SExpr>) -> Vec<Rc<SExpr>> {
    let mut values = vec![];
    let mut current = sexpr;
    while let SExpr::S(lhs, rhs) = &*current {
        values.push(lhs.clone());
        current = rhs.clone();
    }
    if !current.is_nil() {
        values.push(current);
    }
    values
}

fn param_count_eq(func: Builtin, params: &[Rc<SExpr>], n: usize) -> RunResult<()> {
    if params.len() != n {
        Err(format!("{func:?} must have exactly {n} params"))
    } else {
        Ok(())
    }
}

fn param_count_ge(func: Builtin, params: &[Rc<SExpr>], n: usize) -> RunResult<()> {
    if params.len() < n {
        Err(format!("{func:?} must have at least {n} params"))
    } else {
        Ok(())
    }
}

fn call_builtin(ctx: &mut RunContext, func: Builtin, params: &[Rc<SExpr>]) -> RunResult<Rc<SExpr>> {
    match func {
        Builtin::Lambda => eval_lambda(params),
        Builtin::Defun => {
            param_count_ge(func, params, 3)?;
            if is_symbol(&params[0]) {
                let fun_sym = params[0].clone();
                let new = params[1..].to_vec();
                let fun = eval_lambda(&new)?;
                eval_set(ctx.root_scope(), &[fun_sym, fun])
            } else {
                Err("The first param to defun be a symbol".to_owned())
            }
        },
        Builtin::Nil => Ok(Rc::new(SExpr::nil())),
        Builtin::Set => eval_set(ctx.scope.clone(), params),
        Builtin::Setg => eval_set(ctx.root_scope(), params),
        Builtin::Progn => eval_progn(ctx, params),
        Builtin::Putc => {
            param_count_eq(func, params, 1)?;
            if let Some(c) = try_get_char(&params[0]) {
                print!("{c}");
                Ok(Rc::new(SExpr::nil()))
            } else {
                Err("putc must accept exactly 1 char argument".to_owned())
            }
        },
        Builtin::Debug => {
            param_count_eq(func, params, 1)?;
            let param = params[0].clone(); 
            eprintln!("DEBUG: {param:?}");
            Ok(Rc::new(SExpr::nil()))
        },
        Builtin::If => {
            param_count_eq(func, params, 3)?;
            let cond = eval(ctx, params[0].clone())?;
            if is_truthy(cond) {
                eval(ctx, params[1].clone())
            } else {
                eval(ctx, params[2].clone())
            }
        },
        Builtin::Car => {
            param_count_eq(func, params, 1)?;
            if let Some(car) = get_car(&params[0]) {
                Ok(car)
            } else {
                Err("The argument to car must be a list".to_owned())
            }
        },
        Builtin::Cdr => {
            param_count_eq(func, params, 1)?;
            if let Some(car) = get_cdr(&params[0]) {
                Ok(car)
            } else {
                Err("The argument to cdr must be a list".to_owned())
            }
        },
        Builtin::Cons => {
            param_count_eq(func, params, 2)?;
            let head = params[0].clone();
            let tail = params[1].clone();
            Ok(Rc::new(SExpr::S(head, unquote(tail)).quote()))
        },
        Builtin::IsFalsy => {
            param_count_eq(func, params, 1)?;
            if is_truthy(params[0].clone()) {
                Ok(Rc::new(SExpr::nil()))
            } else {
                Ok(Rc::new(SExpr::truthy()))
            }
        },
        Builtin::IsEq => {
            param_count_eq(func, params, 2)?;
            if params[0] == params[1] {
                Ok(Rc::new(SExpr::truthy()))
            } else {
                Ok(Rc::new(SExpr::nil()))
            }
        },
        Builtin::Exit => {
            param_count_eq(func, params, 1)?;
            if let Some(status) = get_int(&params[0]).filter(|v| *v >= 0 && *v <= 255) {
                exit(status as i32);
            } else {
                Err(format!("{func:?} must be called on an int value between 0-255"))
            }
        },
        Builtin::Require => {
            param_count_eq(func, params, 1)?;
            if let Some(file_path) = try_get_string(&params[0]) {
                run_with_context(ctx, &file_path)?;
                Ok(Rc::new(SExpr::nil()))
            } else {
                Err(format!("{func:?} accepts exactly 1 string argument"))
            }
        },
        Builtin::Add | Builtin::Sub | Builtin::Mul | Builtin::Div | Builtin::Mod => {
            param_count_eq(func, params, 2)?;
            let lhs = params[0].clone();
            let rhs = params[1].clone();
            match (&*lhs, &*rhs) {
                (SExpr::Atom(lhs), SExpr::Atom(rhs)) =>
                    Ok(Rc::new(SExpr::Atom(eval_arithmetic(func, lhs.clone(), rhs.clone())?))),
                _ => Err(format!("{func:?} must be called on two values of the same type")),
            }
        },
        Builtin::WdPureTone => {
            param_count_eq(func, params, 2)?;
            wd_pure_tone(ctx, params)
        },
        Builtin::WdSave => {
            param_count_eq(func, params, 2)?;
            wd_save(ctx, params)
        },
        Builtin::ToString => {
            param_count_eq(func, params, 1)?;
            Ok(Rc::new(sexpr_as_string(&params[0])))
        },
        // By definition, `quote` should never be evaluated!
        Builtin::Quote => unreachable!(),
        Builtin::StrAsList => {
            param_count_eq(func, params, 1)?;
            string_as_char_list(&params[0])
        },
        Builtin::ListAsStr => {
            param_count_eq(func, params, 1)?;
            char_list_as_string(params[0].clone())
        },
    }
}

fn wd_pure_tone(ctx: &RunContext, params: &[Rc<SExpr>]) -> RunResult<Rc<SExpr>> {
    let sr = ctx.scope.borrow().lookup(&"wd-sample-rate");
    let sample_rate = try_get_int(&sr)
        .ok_or("wd-sample-rate must be globally set as an int")?;
    let frequency = try_get_float(&params[0])
        .ok_or("frequency parameter be a float")?;
    let duration = try_get_float(&params[1])
        .ok_or("duration parameter be a float")?;

    let sample_count = duration * (sample_rate as f64);
    let wave_count = sample_count / frequency;

    let mut data = vec![];
    for t in 0..sample_count as usize {
        let x = wave_count * 2.0 * PI * t as f64 / sample_count;
        data.push(x.sin());
    }
    Ok(Rc::new(SExpr::Atom(Value::WaveData(data))))
}

fn wd_save(ctx: &RunContext, params: &[Rc<SExpr>]) -> RunResult<Rc<SExpr>> {
    let sr = ctx.scope.borrow().lookup(&"wd-sample-rate");
    let sample_rate = try_get_int(&sr)
        .ok_or("wd-sample-rate must be globally set as an int")?;
    let wavedata = try_get_wavedata(&params[0])
        .ok_or("wavadata parameter be a wavedata object")?;
    let file_path = try_get_string(&params[1])
        .ok_or("file-path parameter must be a String")?;

    if save_wave(sample_rate as u32, &wavedata, &file_path).is_ok() {
        Ok(Rc::new(SExpr::nil()))
    } else {
        Err(format!("Failed saving wave file to {file_path}"))
    }
}

fn char_list_as_string(expr: Rc<SExpr>) -> RunResult<Rc<SExpr>> {
    let mut s = String::new();
    for c_expr in unfold(unquote(expr)).iter() {
        if let Some(c) = try_get_char(c_expr) {
            s.push(c);
        } else {
            return Err("Invalid character list".to_owned());
        }
    }
    Ok(Rc::new(SExpr::Atom(Value::String(s))))
}

fn string_as_char_list(expr: &SExpr) -> RunResult<Rc<SExpr>> {
    if let Some(s) = try_get_string(expr) {
        Ok(Rc::new(s.chars().rev().fold(Rc::new(SExpr::nil()), |acc, it| {
            Rc::new(SExpr::S(Rc::new(SExpr::Atom(Value::Int(it as i64))), acc))
        }).quote()))
    } else {
        Err(format!("Value is not a string: {expr:?}"))
    }
}

fn sexpr_as_string(expr: &SExpr) -> SExpr {
    SExpr::Atom(Value::String(format!("{expr:?}")))
}

fn unquote(expr: Rc<SExpr>) -> Rc<SExpr> {
    let mut result = expr;
    if result.is_quoted() {
        result = result.rhs().expect("How can this be quoted without an RHS?!");
    }
    result
}

fn is_truthy(sexpr: Rc<SExpr>) -> bool {
    match &*unquote(sexpr) {
        SExpr::Atom(value) => match value {
            Value::Int(value)   => *value != 0,
            Value::Float(value) => *value != 0.0,
            Value::Builtin(Builtin::Nil) => false,
            _                   => true,
        },
        SExpr::S(_, _) => true,
    }
}

fn get_int(sexpr: &SExpr) -> Option<i64> {
    match sexpr {
        SExpr::Atom(Value::Int(value)) => Some(*value),
        _ => None,
    }
}

fn get_car(expr: &SExpr) -> Option<Rc<SExpr>> {
    match expr {
        SExpr::Atom(Value::Builtin(Builtin::Nil)) => Some(Rc::new(SExpr::nil())),
        expr @ SExpr::S(_, _) if expr.is_quoted() => {
            match &*expr.rhs().unwrap() {
                SExpr::Atom(Value::Builtin(Builtin::Nil)) => Some(Rc::new(SExpr::nil())),
                SExpr::Atom(_) => None,
                SExpr::S(car, _) if car.is_atom() => Some(car.clone()),
                SExpr::S(car, _) => {
                    Some(Rc::new(car.quote()))
                },
            }
        },
        SExpr::S(car, _) => Some(car.clone()),
        _ => None,
    }
}

fn get_cdr(expr: &SExpr) -> Option<Rc<SExpr>> {
    match expr {
        SExpr::Atom(Value::Builtin(Builtin::Nil)) => Some(Rc::new(SExpr::nil())),
        expr @ SExpr::S(_, _) if expr.is_quoted() => {
            match &*expr.rhs().unwrap() {
                SExpr::Atom(Value::Builtin(Builtin::Nil)) => Some(Rc::new(SExpr::nil())),
                SExpr::Atom(_) => None,
                SExpr::S(_, cdr) if cdr.is_atom() => Some(cdr.clone()),
                SExpr::S(_, cdr) => {
                    Some(Rc::new(cdr.quote()))
                },
            }
        },
        SExpr::S(car, _) => Some(car.clone()),
        _ => None,
    }
}

fn try_get_char(expr: &SExpr) -> Option<char> {
    try_get_int(expr).and_then(|value| char::from_u32(value as u32))
}

fn try_get_int(expr: &SExpr) -> Option<i64> {
    match expr {
        SExpr::Atom(Value::Int(value)) => Some(*value),
        _ => None,
    }
}

fn try_get_float(expr: &SExpr) -> Option<f64> {
    match expr {
        SExpr::Atom(Value::Float(value)) => Some(*value),
        _ => None,
    }
}

fn try_get_string(expr: &SExpr) -> Option<String> {
    match expr {
        SExpr::Atom(Value::String(value)) => Some(value.clone()),
        _ => None,
    }
}

fn try_get_wavedata(expr: &SExpr) -> Option<Vec<f64>> {
    match expr {
        SExpr::Atom(Value::WaveData(value)) => Some(value.clone()),
        _ => None,
    }
}

fn eval_lambda(params: &[Rc<SExpr>]) -> RunResult<Rc<SExpr>> {
    param_count_ge(Builtin::Lambda, params, 2)?;
    let mut arg_names = vec![];
    for arg in unfold(params[0].clone()).iter() {
        if let Some(name) = get_symbol_name(arg) {
            arg_names.push(name.clone());
        } else {
            return Err("The first param of a function definition must by a symbol list".to_owned());
        }
    }
    let f = Value::Function(arg_names, params[1..].to_vec());
    Ok(Rc::new(SExpr::Atom(f)))
}

fn eval_set(scope: Rc<RefCell<Scope>>, params: &[Rc<SExpr>]) -> RunResult<Rc<SExpr>> {
    param_count_eq(Builtin::Set, params, 2)?;
    if let Some(name) = get_symbol_name(&unquote(params[0].clone())) {
        let set_value = params[1].clone();
        scope.borrow_mut().insert(name, set_value.clone());
        Ok(set_value)
    } else {
        Err("The first param to `set` must be a symbol".to_owned())
    }
}

fn get_symbol_name(sexpr: &SExpr) -> Option<&String> {
    match sexpr {
        SExpr::Atom(value) => match value {
            Value::Symbol(name) => Some(name),
            _ => None,
        },
        SExpr::S(_, _) => None,
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
        eval_progn(&mut ctx, &exprs).unwrap()
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
    fn test_eval_progn() {
        assert_eq!("2", format!("{:?}", eval_str("(progn (% 42 5))")));
        assert_eq!("2", format!("{:?}", eval_str("(progn 6 nil (+ 4 2) (% 42 5))")));
        assert_eq!("nil", format!("{:?}", eval_str("(progn nil)")));
    }

    #[test]
    fn test_eval_set() {
        assert_eq!("8", format!("{:?}", eval_str("(set 'x 4) (+ x x)")));
    }

    #[test]
    fn test_eval_fun() {
        assert_eq!("3", format!("{:?}", eval_str("(defun add (a b) (+ a b)) (add 1 2)")));
        assert_eq!("3", format!("{:?}", eval_str("((lambda (a b) (+ a b)) 1 2)")));
    }

    #[test]
    fn test_eval_if() {
        assert_eq!("2", format!("{:?}", eval_str("(if nil 1 2)")));
    }

    #[test]
    fn test_eval_false() {
        assert_eq!("1", format!("{:?}", eval_str("(false? nil)")));
        assert_eq!("1", format!("{:?}", eval_str("(false? 'nil)")));
        assert_eq!("1", format!("{:?}", eval_str("(false? '())")));
        assert_eq!("nil", format!("{:?}", eval_str("(false? 42)")));
        assert_eq!("nil", format!("{:?}", eval_str("(false? '(4))")));
    }

    #[test]
    fn test_eval_eq() {
        assert_eq!("1", format!("{:?}", eval_str("(eq? '(2 3 4) '(2 3 4))")));
        assert_eq!("nil", format!("{:?}", eval_str("(eq? '(2 3 4) '(2 3 5))")));
        assert_eq!("nil", format!("{:?}", eval_str("(eq? '(2 3 4) 4)")));
    }

    #[test]
    fn test_eval_cons() {
        assert_eq!("(quote . (1 . (2 . nil)))", format!("{:?}", eval_str("(cons 1 '(2))")));
        assert_eq!("(quote . (1 . nil))", format!("{:?}", eval_str("(cons 1 '())")));
        assert_eq!("(quote . (1 . nil))", format!("{:?}", eval_str("(cons 1 nil)")));
    }
}
