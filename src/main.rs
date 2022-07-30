use std::io::{self, Write};

use ansi_term::Color;
use rusty_scheme::Interpreter;

macro_rules! print_flush {
    ( $( $x:expr ),* ) => {
        print!( $($x, )* );
        std::io::stdout().flush().expect("failed to flush stdout");
    };
}

fn main() {
    let mut interpreter = Interpreter::new();

    loop {
        print_flush!("scm> ");

        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("failed to flush stdin");

        if ["q", "quit", "exit"].contains(&input.trim()) {
            break;
        } else if input.chars().all(char::is_whitespace) {
            continue;
        }

        match interpreter.interpret_str(&input) {
            Ok(val) => {
                if let Some(val) = val {
                    println!("{}", val)
                }
            }
            Err(err) => {
                eprintln!("{} Error interpreting: {}", Color::Red.paint("!>"), err);
            }
        }
    }
}
