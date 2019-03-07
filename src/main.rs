extern crate rsmt2;

use rsmt2::example::simple::{Cst, Expr, Op, Parser};
use rsmt2::{SmtRes, Solver};

fn main() {
    let mut solver = Solver::default(Parser).expect("could not spawn solver kid");

    let v_1 = "v_1".to_string();
    let v_2 = "v_2".to_string();

    solver
        .declare_const(&v_1, &"Bool")
        .expect("while declaring v_1");
    solver
        .declare_const(&v_2, &"Int")
        .expect("while declaring v_2");

    let expr = Expr::O(
        Op::Disj,
        vec![
            Expr::O(Op::Ge, vec![Expr::cst(-7), Expr::V(v_2.clone())]),
            Expr::V(v_1.clone()),
        ],
    );

    solver.assert(&expr).expect("while asserting an expression");

    if solver.check_sat().expect("during check sat") {
        let model = solver.get_model_const().expect("while getting model");

        println!("{:?}", model);

        let mut okay = false;
        for (ident, typ, value) in model {
            if ident == v_1 {
                assert_eq!(typ, "Bool");
                match value {
                    Cst::B(true) => okay = true,
                    Cst::B(false) => (),
                    Cst::I(int) => panic!("value for v_1 is `{}`, expected boolean", int),
                }
            } else if ident == v_2 {
                assert_eq!(typ, "Int");
                match value {
                    Cst::I(i) if -7 >= i => okay = true,
                    Cst::I(_) => (),
                    Cst::B(b) => panic!("value for v_2 is `{}`, expected isize", b),
                }
            }
        }

        if !okay {
            panic!("got sat, but model is spurious")
        }
    } else {
        panic!("expected sat, got unsat")
    }

    solver.kill().unwrap()
}
