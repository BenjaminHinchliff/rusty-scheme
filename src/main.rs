use std::io::{self, Write};

use ansi_term::{Color, ANSIString, ANSIStrings};
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
        let bytes = io::stdin()
            .read_line(&mut input)
            .expect("failed to flush stdin");

        // EOF or exit prompt
        if bytes == 0 || ["q", "quit", "exit"].contains(&input.trim()) {
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
                let strings: &[ANSIString<'static>] = &[
                    Color::Red.paint("!> Error interpreting: "),
                    Color::Red.bold().paint(err.to_string()),
                ];
                eprintln!("{}", ANSIStrings(strings));
            }
        }
    }
}
