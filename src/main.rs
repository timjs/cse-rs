use std::collections::hash_map::HashMap;
use std::collections::hash_map::Entry::*;

use std::hash::Hash;
use std::hash::Hasher;
use std::hash::SipHasher;

use std::io;
use std::io::BufRead;
use std::io::BufReader;

use std::iter::Peekable;
use std::str::Chars;

use std::fmt;

use std::thread;

use Expr::*;

// Expressions

#[derive(PartialEq,Eq)]
enum Expr {
    App(Id, Name, Box<Expr>, Box<Expr>),
    Var(Id, Name),
    Sub(Repl),
}

type Id = u64;
type Name = String;
type Repl = i32;

// Display

impl fmt::Display for Expr {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            App(_, ref n, ref l, ref r) => write!(f, "{}({},{})", n, l, r),
            Var(_, ref n) => write!(f, "{}", n),
            Sub(ref r) => write!(f, "{}", r),
        }
    }

}

// Hash

impl Hash for Expr {

    fn hash<H>(&self, state: &mut H) where H: Hasher {
        match *self {
            App(i, _, _, _) => i.hash(state),
            Var(i, _) => i.hash(state),
            Sub(_) => unreachable!(),
        }
    }

}

// Parser

struct Parser<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> Parser<'a> {

    fn new(s: &'a str) -> Parser<'a> {
        Parser { chars: s.chars().peekable() }
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
        self.take_while(char::is_alphabetic)
    }

    fn var(&mut self, name: Name) -> Expr {
        let mut hasher = SipHasher::new();
        name.hash(&mut hasher);
        let id = hasher.finish();

        Var(id, name)
    }

    fn app(&mut self, name: Name) -> Expr {
        self.chars.next(); // '('
        let left = self.expr();
        self.chars.next(); // ','
        let right = self.expr();
        self.chars.next(); // ')'

        let mut hasher = SipHasher::new();
        name.hash(&mut hasher);
        left.hash(&mut hasher);
        right.hash(&mut hasher);
        let id = hasher.finish();

        App(id, name, Box::new(left), Box::new(right))
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
    map: HashMap<&'a Expr, Repl>,
    num: Repl,
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
    //             App(i, ref n, ref l, ref r) => {
    //                 let l_ = l.cse(state);
    //                 let r_ = r.cse(state);
    //                 App(i, n.clone(), Box::new(l_), Box::new(r_))
    //             },
    //             Var(i, ref n) => Var(i, n.clone()),
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
                // match on self has to be out of this match to satisfy the borrow of state
            }
        }
        match *self {
            App(i, ref n, ref l, ref r) => {
                let l_ = l.cse(state);
                let r_ = r.cse(state);
                return App(i, n.clone(), Box::new(l_), Box::new(r_));
            },
            Var(i, ref n) => return Var(i, n.clone()),
            Sub(_) => unreachable!(),
        }
    }

}

// Main

fn run() {
    let mut lines = BufReader::new(io::stdin()).lines();
    lines.next(); // = line_count

    for read in lines {
    // while let Some(Ok(line)) = lines.next() {
        let line = read.unwrap();
        let mut parser = Parser::new(&line);
        let expr = parser.expr();
        let mut state = State::new();
        let result = expr.cse(&mut state);
        println!("{}", result);
    }
}

fn main () {
    let thread = thread::Builder::new().stack_size(16_000_000);
    let handle = thread.spawn(run);
    handle.unwrap().join();
}

