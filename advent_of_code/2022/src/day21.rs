use std::collections::HashMap;
use num::integer::gcd;

use crate::common::*;

#[derive(Eq, PartialEq, Clone, Debug)]
enum Expr {
    Num(i64),
    Var(String),
    Arithmetic(Op, Box<Expr>, Box<Expr>),
    Unknown,
}

impl Expr {
    #[allow(dead_code)]
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

struct Fraction {
    num: i64,
    den: i64,
}

impl Fraction {
    fn new(num: i64, den: i64) -> Fraction {
        let divisor = gcd(num, den);
        Fraction {
            num: num / divisor,
            den: den / divisor,
        }
    }

    fn add(&self, other: i64) -> Fraction {
        Fraction::new(self.num + other * self.den, self.den)
    }

    fn sub(&self, other: i64) -> Fraction {
        Fraction::new(self.num - other * self.den, self.den)
    }

    fn mul(&self, other: i64) -> Fraction {
        Fraction::new(self.num * other, self.den)
    }

    fn div(&self, other: i64) -> Fraction {
        Fraction::new(self.num, self.den * other)
    }

    fn negate(&self) -> Fraction {
        Fraction::new(-self.num, self.den)
    }

    fn invert(&self) -> Fraction {
        Fraction::new(self.den, self.num)
    }
}

impl std::fmt::Debug for Fraction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}/{}", self.num, self.den))
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
    if let box Expr::Num(n) = expand_expr(exprs, exprs.get("root").unwrap()) {
        n
    } else {
        panic!("Cannot find value for root!")
    }
}

fn part2(mut exprs: HashMap<String, Expr>) -> i64 {
    exprs.insert("humn".to_owned(), Expr::Unknown);
    let (lhs, rhs) = {
        if let Some(Expr::Arithmetic(_, lhs, rhs)) = exprs.get("root") {
            (expand_expr(&exprs, lhs), expand_expr(&exprs, rhs))
        } else {
            panic!("root expr not valid")
        }
    };

    let (e, answer) = if let box Expr::Num(target) = rhs {
        unravel(&lhs, Fraction::new(target, 1))
    } else if let box Expr::Num(target) = lhs {
        unravel(&rhs, Fraction::new(target, 1))
    } else {
        panic!("Multiple variables detected! I'm not that clever!");
    };
    assert!(matches!(e, box Expr::Unknown));
    assert!(answer.den == 1);
    answer.num
}

// "unravels" the expr into the target as much as possible
fn unravel(expr: &Expr, target: Fraction) -> (Box<Expr>, Fraction) {
    match expr {
        expr @ Expr::Arithmetic(op, lhs, rhs) => {
            if let box Expr::Num(rhs_num) = rhs {
                match op {
                    Op::Add => unravel(lhs, target.sub(*rhs_num)),
                    Op::Sub => unravel(lhs, target.add(*rhs_num)),
                    Op::Div => unravel(lhs, target.mul(*rhs_num)),
                    Op::Mul => unravel(lhs, target.div(*rhs_num)),
                }
            } else if let box Expr::Num(lhs_num) = lhs {
                match op {
                    Op::Add => unravel(rhs, target.sub(*lhs_num)),
                    Op::Sub => unravel(rhs, target.negate().add(*lhs_num)),
                    Op::Mul => unravel(rhs, target.div(*lhs_num)),
                    Op::Div => unravel(rhs, target.invert().mul(*lhs_num)),
                }
            } else {
                (Box::new(expr.clone()), target)
            }
        },
        Expr::Unknown => (Box::new(Expr::Unknown), target),
        expr => panic!("how did I get here? {expr:?}"),
    }
}

fn expand_expr(exprs: &HashMap<String, Expr>, expr: &Expr) -> Box<Expr> {
    reduce_expr(&fold_expr(exprs, expr))
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
