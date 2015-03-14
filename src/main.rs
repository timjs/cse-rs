#![feature(io)]
#![feature(std_misc)]

use std::io;
use std::io::BufReader;
use std::io::BufReadExt;
use std::iter::Peekable;
use std::str::Chars;
use std::fmt;
use std::collections::hash_map::HashMap;
use std::collections::hash_map::Entry::*;

use Expr::*;

// Expressions

#[derive(PartialEq,Eq,Hash)]
enum Expr {
    App(Name, Box<Expr>, Box<Expr>),
    Var(Name),
    Sub(Repl),
}

type Name = String;
type Repl = i32;

// Display

impl fmt::Display for Expr {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            App(ref n, ref l, ref r) => write!(f, "{}({},{})", n, l, r),
            Var(ref n) => write!(f, "{}", n),
            Sub(ref r) => write!(f, "{}", r),
        }
    }

}

// Parser

struct Parser<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> Parser<'a> {

    fn new(s: &'a str) -> Parser<'a> {
        Parser{chars: s.chars().peekable()}
    }

    fn take_while<F: Fn(char) -> bool>(&mut self, pred: F) -> String {
        let mut name = String::new();
        while let Some(&c) = self.chars.peek() {
            if pred(c) {
                name.push(c);
                self.chars.next();
            } else {
                break;
            }
        }
        name
    }

    fn name(&mut self) -> Name {
        self.take_while(CharExt::is_alphabetic)
    }

    fn var(&mut self, name: Name) -> Expr {
        Var(name)
    }

    fn app(&mut self, name: Name) -> Expr {
        self.chars.next(); // '('
        let left = self.expr();
        self.chars.next(); // ','
        let right = self.expr();
        self.chars.next(); // ')'
        App(name, Box::new(left), Box::new(right))
    }

    fn expr(&mut self) -> Expr {
        // let name = self.chars.take_while(|&c| c.is_alphabetic()).collect() /// Moves uit of `chars`!
        let name = self.name();
        match self.chars.peek() {
            Some(&'(') => self.app(name),
            Some(_) => self.var(name),
            _ => unreachable!(),
        }
    }

}

// Elimination

struct State<'a> {
    num: Repl,
    map: HashMap<&'a Expr, Repl>,
}

impl<'a> State<'a> {

    fn new() -> State<'a> {
        State{num: 1, map: HashMap::new()}
    }

}

impl Expr {

    // fn cse<'a>(&'a self, state: &mut State<'a>) -> Expr {
    //     if let Some(&repl) = state.map.get(self) {
    //         Sub(repl)
    //     } else {
    //         state.map.insert(&self, state.num);
    //         state.num += 1;
    //         match *self {
    //             App(ref n, ref l, ref r) => {
    //                 let l_ = l.cse(state);
    //                 let r_ = r.cse(state);
    //                 App(n.clone(), Box::new(l_), Box::new(r_))
    //             },
    //             Var(ref n) => Var(n.clone()),
    //             Sub(_) => unreachable!(),
    //         }
    //     }
    // }

    fn cse<'a>(&'a self, state: &mut State<'a>) -> Expr {
        match state.map.entry(self) {
            Occupied(e) => {
                return Sub(*e.get());
            },
            Vacant(e) => {
                e.insert(state.num);
                state.num += 1;
                // match on self has to be out of this match to satisfy the borrowchecker
            }
        }
        match *self {
            App(ref n, ref l, ref r) => {
                let l_ = l.cse(state);
                let r_ = r.cse(state);
                return App(n.clone(), Box::new(l_), Box::new(r_));
            },
            Var(ref n) => return Var(n.clone()),
            Sub(_) => unreachable!(),
        }
    }

}

// Main

fn main() {
    let mut lines = BufReader::new(io::stdin()).lines();
    lines.next(); // line_count

    // for read in lines {
    while let Some(Ok(line)) = lines.next() {
        // let line = read.unwrap();
        println!(">> Creating parser");
        let mut parser = Parser::new(&line);
        println!(">> Parsing");
        let expr = parser.expr();
        println!(">> Creating state");
        let mut state = State::new();
        println!(">> Eliminating");
        let result = expr.cse(&mut state);
        println!("{}", result);
    }
}

