use std::collections::HashMap;

use crate::common::*;

#[derive(Eq, PartialEq, Clone, Debug)]
enum Expr {
    Num(i64),
    Var(String),
    Arithmetic(Op, Box<Expr>, Box<Expr>),
    Unknown,
}

impl Expr {
    fn prettify(&self) -> String {
        match self {
            Expr::Num(n) => format!("{n}"),
            Expr::Var(name) => name.to_string(),
            Expr::Arithmetic(op, lhs, rhs) => {
                let pretty_op = match op {
                    Op::Add => '+',
                    Op::Sub => '-',
                    Op::Mul => '*',
                    Op::Div => '/',
                };
                format!("({}{}{})", lhs.prettify(), pretty_op, rhs.prettify())
            },
            Expr::Unknown => "x".to_string(),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

pub fn day21() -> AocResult<()> {
    let exprs = parse_root(&std::fs::read_to_string("data/day21.txt")?);
    println!("Part 1: {}", part1(&exprs));
    println!("Part 2: {}", part2(exprs));
    Ok(())
}

fn part1(exprs: &HashMap<String, Expr>) -> i64 {
    let folded_expr = fold_expr(exprs, exprs.get("root").unwrap());
    if let box Expr::Num(n) = reduce_expr(&folded_expr) {
        n
    } else {
        panic!("Cannot find value for root!")
    }
}

fn part2(mut exprs: HashMap<String, Expr>) -> i64 {
    exprs.insert("humn".to_owned(), Expr::Unknown);
    let (lhs, rhs) = {
        if let Some(Expr::Arithmetic(_, lhs, rhs)) = exprs.get("root") {
            (reduce_expr(&fold_expr(&exprs, lhs)), reduce_expr(&fold_expr(&exprs, rhs)))
        } else {
            panic!("root expr not valid")
        }
    };

    println!("{} = {}", lhs.prettify(), rhs.prettify());

    let (e, target) = if let box Expr::Num(target) = rhs {
        unravel(&lhs, target)
    } else if let box Expr::Num(target) = lhs {
        unravel(&rhs, target)
    } else {
        panic!("Multiple variables detected! I'm not that clever!");
    };
    println!("{} = {}", e.prettify(), target);
    unimplemented!()
}

// "unravels" the expr into the target as much as possible
fn unravel(expr: &Expr, target: i64) -> (Box<Expr>, i64) {
    match expr {
        expr @ Expr::Arithmetic(op, lhs, rhs) => {
            if let box Expr::Num(rhs_num) = rhs {
                match op {
                    Op::Add => unravel(lhs, target - rhs_num),
                    Op::Sub => unravel(lhs, target + rhs_num),
                    Op::Div => unravel(lhs, target * rhs_num),
                    // To unravel this properly, I need fractions!
                    Op::Mul => (Box::new(expr.clone()), target),
                }
            } else if let box Expr::Num(lhs_num) = lhs {
                match op {
                    Op::Add => unravel(rhs, target - lhs_num),
                    Op::Sub => unravel(rhs, target + lhs_num),
                    _ => (Box::new(expr.clone()), target),
                }
            } else {
                (Box::new(expr.clone()), target)
            }
        },
        Expr::Unknown => (Box::new(Expr::Unknown), target),
        expr => panic!("how did I get here? {expr:?}"),
    }
}

fn fold_expr(exprs: &HashMap<String, Expr>, expr: &Expr) -> Box<Expr> {
    match expr {
        Expr::Var(name) => fold_expr(exprs, exprs.get(name).unwrap()),
        Expr::Arithmetic(op, lhs, rhs) => {
            Box::new(Expr::Arithmetic(*op, fold_expr(exprs, lhs), fold_expr(exprs, rhs)))
        },
        expr => Box::new(expr.clone()),
    }
}

fn reduce_expr(expr: &Expr) -> Box<Expr> {
    match expr {
        Expr::Arithmetic(op, lhs, rhs) => {
            let reduced_lhs = reduce_expr(lhs);
            let reduced_rhs = reduce_expr(rhs);
            if let box Expr::Num(lhs_num) = reduced_lhs &&
                let box Expr::Num(rhs_num) = reduced_rhs {
                Box::new(Expr::Num(match op {
                    Op::Add => lhs_num + rhs_num,
                    Op::Sub => lhs_num - rhs_num,
                    Op::Mul => lhs_num * rhs_num,
                    Op::Div => lhs_num / rhs_num,
                }))
            } else {
                Box::new(Expr::Arithmetic(*op, reduced_lhs, reduced_rhs))
            }
        },
        expr => Box::new(expr.clone()),
    }
}

/*
fn part1_solve(
    all_exprs: &HashMap<String, Expr>,
    expr: Box<Expr>,
) -> i64 {
    if let Some(expr) = exprs.get(name) {
        match expr {
            Expr::Num(n) => *n,
            Expr::Arithmetic(op, lhs_name, rhs_name) => {
                let lhs = part1_solve(&exprs, lhs_name);
                let rhs = part1_solve(&exprs, rhs_name);
                match op {
                    Op::Add => lhs + rhs,
                    Op::Sub => lhs - rhs,
                    Op::Mul => lhs * rhs,
                    Op::Div => lhs / rhs,
                }
            },
        }
    } else {
        panic!("{name} not found")
    }
}
*/

fn parse_root(s: &str) -> HashMap<String, Expr> {
    s.lines().map(|line| line.chars().collect::<Vec<_>>()).map(|chars| {
        let name = chars[..4].iter().collect::<String>();
        let op = if chars[6].is_ascii_digit() {
            let n = chars[6..].iter().collect::<String>().parse::<i64>().unwrap();
            Expr::Num(n)
        } else {
            let lhs = chars[6..10].iter().collect::<String>();
            let rhs = chars[13..17].iter().collect::<String>();
            let op = match chars[11] {
                '+' => Op::Add,
                '-' => Op::Sub,
                '*' => Op::Mul,
                '/' => Op::Div,
                c   => panic!("Unexpected operator: {c}"),
            };
            Expr::Arithmetic(op, Box::new(Expr::Var(lhs)), Box::new(Expr::Var(rhs)))
        };
        (name, op)
    }).collect::<HashMap<String, Expr>>()
}
// ((634+((3049550*(((((7*4)*((176*(((3*7)/3)*((((17+((3+(2*((7*2)* 2)))+13))*2)/2)+14)))+(3*((((2*(((3*(9+(2*((16/2)+(7+(2*8))))))*10)+((((2*(2*((2*((4*4)+1))-11)))+((((3*2)+5)*2)*3))+(7*5))-12)))-(2*((10*(3*3))+((5*(2*19))+(((10+3)*2)+((3 *(3*3))+(((3*4)*2)+10)))))))+((((5*3)*(7+1))/5)*((2+5)*4)))+((3*(((((5*2)+19)*4)*2)+(17*5)))+(((((((14/2)*2)+(6+1))+(2*((2*(2+5))+3)))-(3*3))/2)*2))))))-((((((2*4)*(((5*17) /5)*2))+(((5*2)*5)-9))*((((2*((5*17)+((((((13*2)+(5*3))*2)+(11+20))+18)*2)))-(((19*3)-6)*3))-(19*4))+(2*(16+3))))+(((((((3*5)+((17*18)/6))-((((7*3)*2)/3)-1))*2)-(((((2*13)+ (3*2))*20)/5)/4))+((((3*(19-6))-9)+((5+(10*4))+(5+3)))+6))*(((3*4)/2)*(5*5))))*3))+(((4*(2*(((15*2)*((2*5)+(4+(((2*(19-6))/2)*3))))-((5*(3+(14*2)))+8))))+(((3*6)+(17+((((14 /2)*(((5+4)-3)+1))+(4*11))+(2+((((((5*(((1+12)*(2*3))/6))/5)*3)/3)+(2*3))+4)))))*((((((5+(5+5))*5)+((2*3)*(9-2)))*3)+((2*((((3*3)+(2*17))-(3*3))+3))*2))+((5*5)+((6*5)+((19* 2)+((3*(2*(5+(2*4))))+(5*5))))))))*((13*3)+3)))*4))-(((((2*((((3*(12+((2*5)+1)))+((10+(2*3))*(2*(4+8))))+(7*9))+(((14*(((1+(2*3))+(2*12))*2))+(((((2*((((((9*11)+((((2*((((( (((((((((5*((((((5*5)*4)+(12*(2*(3+4))))+(((((1+18)*4)+((((((((((((17*2)+(3*((10+16)+(((2*6)-3)*3))))+((((7*2)/2)+(12*3))*2))*3)+(((((3+(((((6+7)+(2*5))*3)+((2*11)+((17+(3* (9-2)))*2)))*2))+((((2*(((674+((((((14*9)+11)+x)/3)-467)*81))/2)-387))-865)+365)/8))*11)-215)/4))*4)+998)*2)-794)/2)-602)*2))/2)-414))/(8-1))+614))+81)*2)-562)/6)-829)*3)+173)/7)+871)/6)-888)*40)-128))+519)+941)/12))*9)-582)/4)+844))-191)/3)-386)/5))*15)))-338)/4)+88)*3)))/11) = 792784087587
