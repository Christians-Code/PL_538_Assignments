/*
 * Reverse Polish Notation: parser.rs
 * See `rpn.md` for the overview.
 */

extern crate rand;

use std::io::{self, Write};

use super::rpn;

pub fn rpn_repl() -> rpn::Result<()> {
    let mut stack = rpn::Stack::new();
    let mut input = String::new();

    // Read-eval-print loop
    loop {
        // Clear the input buffer
        input.clear();

        // Prompt the user
        print!("> ");
        io::stdout().flush().map_err(rpn::Error::IO)?;

        // Read a line and evaluate it
        io::stdin().read_line(&mut input).map_err(rpn::Error::IO)?;
        evaluate_line(&mut stack, &input)?;

        // A successful run should end with a stack with a exactly one item: the result
        let res = stack.pop()?;
        if stack.empty() {
            println!("Reply> {:?}", res);
        } else {
            return Err(rpn::Error::Extra);
        }
    }
}

fn evaluate_line(stack: &mut rpn::Stack, buf: &str) -> rpn::Result<()> {
    // Trim whitespace and split; this gives an iterator of tokens.
    let tokens = buf.trim().split_whitespace();

    /*
     * Write the main loop processing the tokens. The `parse` method for Strings will be useful for
     * parsing integers. See here for examples:
     *
     * https://doc.rust-lang.org/std/primitive.str.html#method.parse
     */
    for tok in tokens {
        let bool_res = tok.parse::<bool>();
        match bool_res {
            Ok(bool_val) => match stack.push(rpn::Item::Bool(bool_val)) {
                Ok(()) => (),
                Err(e_1) => return Err(e_1),
            },
            Err(_) => {
                let int_res = tok.parse::<i32>();
                match int_res {
                    Ok(int_val) => match stack.push(rpn::Item::Int(int_val)) {
                        Ok(()) => (),
                        Err(e_2) => return Err(e_2),
                    },
                    Err(_) => {
                        let res = if tok == "+" {
                            stack.eval(rpn::Op::Add)
                        } else if tok == "=" {
                            stack.eval(rpn::Op::Eq)
                        } else if tok == "~" {
                            stack.eval(rpn::Op::Neg)
                        } else if tok == "<->" {
                            stack.eval(rpn::Op::Swap)
                        } else if tok == "#" {
                            stack.eval(rpn::Op::Rand)
                        } else if tok == "?" {
                            stack.eval(rpn::Op::Cond)
                        } else if tok == "quit" {
                            stack.eval(rpn::Op::Quit)
                        } else {
                            Err(rpn::Error::Syntax)
                        };

                        match res {
                            Ok(()) => (),
                            Err(e_3) => return Err(e_3),
                        }
                    }
                }
            }
        }
    }

    Ok(())
}
