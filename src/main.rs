use std::io::{BufRead, BufReader};

struct Parser {
    pub idx: usize,
    pub src: String,
}

impl Parser {
    pub fn new(src: String) -> Self {
        Self {
            idx: 0,
            src: src.replace(" ", ""),
        }
    }

    pub fn peek(&self) -> TokenKind {
        use TokenKind::*;
        match self.src.chars().nth(self.idx) {
            Some('(') => ParenLeft,
            Some(')') => ParenRight,
            Some('*') | Some('∧') => And,
            Some('+') | Some('∨') => Or,
            Some('\'') => Not,
            Some('1') => True,
            Some('0') => False,
            Some(_) => Identifier,
            None => Eof,
        }
    }

    pub fn next(&mut self) -> (TokenKind, char) {
        let kind = self.peek();
        let c = self.src.chars().nth(self.idx).unwrap_or('\0');
        self.idx += 1;
        (kind, c)
    }

    pub fn parse_expr(&mut self) -> Expr {
        // Collect the first term, save for later
        let mut lhs = self.parse_term();

        // Gather all following terms into a Vec of (Expr, BinaryOpKind)
        let mut ops = Vec::new();
        loop {
            let (kind, tok) = self.next();

            use TokenKind::*;
            match kind {
                And => {
                    let rhs = self.parse_term();
                    ops.push((rhs, BinaryOpKind::And));
                }
                Or => {
                    let rhs = self.parse_term();
                    ops.push((rhs, BinaryOpKind::Or));
                }
                ParenRight | Eof => break,
                _ => panic!("Unexpected token {}", tok),
            }
        }

        // Reduce the mixed And and Or ops into Or-only ops
        let mut ors = Vec::new();
        for term in ops.into_iter() {
            match term.1 {
                BinaryOpKind::And => {
                    // Overwrite the left-hand side with a nested And op
                    lhs = lhs.add_rhs(term.0, term.1);
                }
                BinaryOpKind::Or => {
                    // Replace the left-hand side with the next term
                    let or = std::mem::replace(&mut lhs, term.0);

                    // Push the old left-hand side to the Vec of Or ops
                    ors.push(or);
                }
            }
        }

        // Push the spare term
        ors.push(lhs);

        // Combine the flat list of Or ops into a nested Expr tree
        let mut ors = ors.into_iter();
        let mut expr = ors.next().unwrap();
        for rhs in ors {
            expr = expr.add_rhs(rhs, BinaryOpKind::Or);
        }

        expr
    }

    pub fn parse_term(&mut self) -> Expr {
        let (kind, tok) = self.next();

        use TokenKind::*;
        let mut lhs = match kind {
            True => Expr::Literal(true),
            False => Expr::Literal(false),
            Identifier => Expr::Identifier(tok),
            ParenLeft => self.parse_expr(),
            ParenRight => panic!("Unexpected closing paren"),
            _ => panic!("Unexpected token {}", tok),
        };

        while self.peek() == Not {
            self.next();
            lhs = Expr::UnaryOp(UnaryOpKind::Not, Box::new(lhs));
        }

        lhs
    }
}

#[derive(Debug, PartialEq, Eq)]
enum TokenKind {
    ParenLeft,
    ParenRight,
    Identifier,
    And,
    Or,
    Not,
    Eof,
    True,
    False,
}

#[derive(Debug, PartialEq, Eq)]
enum BinaryOpKind {
    And,
    Or,
}

#[derive(Debug)]
enum UnaryOpKind {
    Not,
}

type ExprPair = Box<(Expr, Expr)>;

#[derive(Debug)]
enum Expr {
    BinaryOp(BinaryOpKind, ExprPair),
    UnaryOp(UnaryOpKind, Box<Expr>),
    Identifier(char),
    Literal(bool),
}

impl Expr {
    pub fn add_rhs(self, rhs: Self, op: BinaryOpKind) -> Self {
        Self::BinaryOp(op, Box::new((self, rhs)))
    }
}

fn parse(src: &str) -> Expr {
    let mut parser = Parser::new(src.to_string());
    let expr = parser.parse_expr();
    assert_eq!(parser.peek(), TokenKind::Eof);
    println!("{}:\n{:#?}", src, expr);
    expr
}

fn eval(expr: Expr) -> bool {
    match expr {
        Expr::BinaryOp(kind, pair) => match kind {
            BinaryOpKind::Or => eval(pair.0) || eval(pair.1),
            BinaryOpKind::And => eval(pair.0) && eval(pair.1),
        }
        Expr::UnaryOp(kind, expr) => match kind {
            UnaryOpKind::Not => !eval(*expr),
        }
        Expr::Identifier(identifier) => todo!(),
        Expr::Literal(literal) => literal
    }
}

fn main() {
    println!("{}", eval(parse("1+0*0")));
    println!("{}", eval(parse("0*0+1")));
    println!("{}", eval(parse("(1+0)*(1*0)'")));
    println!("{}", eval(parse("1*(1+(0*1))")));

    for line in BufReader::new(std::io::stdin()).lines() {
        match line {
            Ok(input) => {
                if input == "q" || input == "quit" || input == "exit" {
                    break;
                }

                let expr = parse(&input);
                println!("{:#?}", expr);
                println!("{}", eval(expr));
            }
            Err(e) => println!("Failed to read stdin: {e}")
        }
    }
}
