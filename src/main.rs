#![feature(old_io)]
#![feature(std_misc)]

use std::old_io::stdio::stdin;
use std::iter::Peekable;
use std::str::Chars;
use std::fmt;
use std::collections::HashMap;
use std::collections::hash_map::Entry::*;

use Expr::{App,Var,Sub};

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

    fn name(&mut self) -> Name {
        let mut name = String::new();
        loop {
            match self.chars.peek() {
                Some(&c) if c.is_alphabetic() => {
                    name.push(c);
                    self.chars.next();
                },
                _ => break,
            }
        }
        name
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
        let name = self.name();
        match self.chars.peek() {
            Some(&'(') => self.app(name),
            Some(_) => self.var(name),
            _ => unreachable!(),
        }
    }

    fn parse(&mut self) -> Expr {
        self.expr()
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

    fn cse<'a>(&'a self, state: &mut State<'a>) -> Expr {
        match state.map.entry(self) {
            Occupied(e) => {
                return Sub(*e.get());
            },
            Vacant(e) => {
                e.insert(state.num);
                state.num += 1;
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
    let first_line = stdin().read_line().unwrap();
    let line_count = first_line.trim().parse::<u32>().unwrap();
    for _ in (0..line_count) {
        let line = stdin().read_line().unwrap();
        let mut parser = Parser::new(&line);
        let expr = parser.parse();
        let mut state = State::new();
        let result = expr.cse(&mut state);
        println!("{}", result);
    }
}

