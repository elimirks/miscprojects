use crate::parser::*;

struct RunContext {
}

fn run(root_sexprs: Vec<SExpr>) -> RunContext {
    let mut context = RunContext {};
    context
}

// fn eval(ctx: &mut RunContext, expr: &SExpr) -> SExpr {
//     match expr {
//         expr @ SExpr::Atom(_, _) => expr.clone(),
//         expr @ SExpr::S(_, lhs, rhs) => {
//             match lhs {
//                 box SExpr::Atom(_, _) => todo!(),
//                 subexpr => eval(ctx, &subexpr),
//             };
//             todo!()
//         },
//     }
// }
