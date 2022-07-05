#![allow(dead_code)]
use std::iter::Peekable;
use std::collections::VecDeque;

#[derive(Debug, PartialEq, Clone, Copy)]
enum Assoc {
    Left,
    Right,
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum OperatorKind {
    Plus,
    Minus,
    Mult,
    Div,
}

#[derive(Debug, PartialEq, Clone, Copy)]
struct Operator {
    kind: OperatorKind,
    precedence: i32,
    assoc: Assoc,
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Token {
    Number(f32),
    OpenParen,
    CloseParen,
    Operator(Operator),
}

#[derive(Debug, Clone)]
struct Lexer<Chars: Iterator<Item=char>> {
    chars: Peekable<Chars>
}

impl<Chars: Iterator<Item=char>> Lexer<Chars> {
    fn from_iter(chars: Chars) -> Self {
        Self {chars: chars.peekable()}
    }
}

fn print_type_of<T>(_: &T) {
    println!("{}", std::any::type_name::<T>())
}

impl<Chars: Iterator<Item=char>> Iterator for Lexer<Chars> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        while let Some(_) = self.chars.next_if(|x| x.is_whitespace()) {} 

        if let Some(x) = self.chars.next() {
            let mut text = String::new();
            text.push(x);
            match x {
                '(' => Some(Token::OpenParen),
                ')' => Some(Token::CloseParen),
                '+' => Some(Token::Operator(Operator {kind: OperatorKind::Plus, precedence: 2, assoc: Assoc::Left})),
                '-' => Some(Token::Operator(Operator {kind: OperatorKind::Minus, precedence: 2, assoc: Assoc::Left})),
                '*' => Some(Token::Operator(Operator {kind: OperatorKind::Mult, precedence: 3, assoc: Assoc::Left})),
                '/' => Some(Token::Operator(Operator {kind: OperatorKind::Div, precedence: 3, assoc: Assoc::Left})),
                '.' => {
                    None // TODO: Implement floats starting with '.', like .7
                },
                _ => {
                    let mut dot_seen = false;
                    if !x.is_numeric(){
                        panic!("Unexpected token found during lexing.");
                    }
                    while let Some(y) = self.chars.next_if(|x : &char| x.is_numeric() || (*x == '.' && dot_seen == false)) {
                        if y == '.' {
                            dot_seen = true;
                        }
                        text.push(y);
                    }
                    Some(Token::Number(text.parse::<f32>().unwrap()))
                },
            }
        } else {
            None
        }
    }
} 

fn shunting_yard(lexer: impl Iterator<Item=Token>) -> VecDeque<Token> {
    let mut out_queue = VecDeque::<Token>::new();
    let mut op_stack  = Vec::<Token>::new();
    for token in lexer {
        match token {
            Token::Number(_) => out_queue.push_back(token),
            Token::Operator(x) => {
                while op_stack.len() > 0 {
                    match op_stack[op_stack.len() - 1] {
                        Token::OpenParen => break,
                        Token::Operator(y) => {
                            if y.precedence > x.precedence || y.precedence == x.precedence && x.assoc == Assoc::Left {
                                out_queue.push_back(op_stack.pop().unwrap());
                            } 
                            else { break };
                        }
                        _ => panic!("Unexpected token found while parsing."),
                    }
                }
                op_stack.push(token);
            }
            Token::OpenParen => op_stack.push(token),
            Token::CloseParen => {
                while (op_stack.len() > 0) && (op_stack[op_stack.len() - 1] != Token::OpenParen) {
                    //panic!("CloseParen found!");
                    out_queue.push_back(op_stack.pop().unwrap());
                }
                if op_stack.len() == 0 {
                    panic!("No OpenParen found while parsing.");
                }
                else { op_stack.pop() };
            }
        }
    }

    while op_stack.len() > 0 {
        if op_stack[op_stack.len() - 1] == Token::OpenParen {
            panic!("Mismatched parenthesis found while parsing.");
        }
        else { out_queue.push_back(op_stack.pop().unwrap()) };
    }
    out_queue
}

fn evaluate_expression(tokens: VecDeque<Token>) -> Token {
    let mut s = Vec::<f32>::new();

    for token in tokens {
        match token {
            Token::Number(x) => s.push(x),
            Token::Operator(y) =>
            {
                match y.kind {
                    OperatorKind::Plus => {
                        let right = s.pop().unwrap();
                        let left = s.pop().unwrap();
                        s.push(right + left);
                    },
                    OperatorKind::Mult => {
                        let right = s.pop().unwrap();
                        let left = s.pop().unwrap();
                        s.push(right * left);
                    },
                    OperatorKind::Minus => {
                        let right = s.pop().unwrap();
                        let left = s.pop().unwrap();
                        s.push(left - right);
                    },
                    OperatorKind::Div => {
                        let right = s.pop().unwrap();
                        let left = s.pop().unwrap();
                        s.push(left / right);
                    },
                }
            }
            _ => panic!("Found unexpected token during evaluation."),
        }
    }
    return Token::Number(s.pop().unwrap())
}

#[test]
fn test_lexer() {
    let mut tokens: Vec<Token> = Lexer::from_iter(".1 .+ 12 ".to_string().chars()).collect();
    assert_eq!(tokens, vec![]);
    tokens = Lexer::from_iter("1 + 12 ".to_string().chars()).collect();
    assert_eq!(tokens, vec![Token::Number(1.0), Token::Operator(Operator {kind: OperatorKind::Plus, precedence: 2, assoc: Assoc::Left}), Token::Number(12.0)]);
}

#[test]
fn test_eval() {
    let expr = "((1+2) - 7 * (5    -2) - 1)/2".to_string();
    let lexed = Lexer::from_iter(expr.chars());
    let parsed = shunting_yard(lexed);
    let evaluated = evaluate_expression(parsed);
    assert_eq!(evaluated, Token::Number(-9.5));
}

fn main() {

    fn prompt(s: &mut String) -> Result<usize, std::io::Error> {
        use std::io::Write;
        print!("λ ");
        std::io::stdout().flush().unwrap();
        s.clear();
        std::io::stdin().read_line(s) 
    }

    let mut buffer = String::new();

    while let Ok(_) = prompt(&mut buffer) {
        let expr = buffer.chars();
        let lexed = Lexer::from_iter(expr);        
        let parsed = shunting_yard(lexed);
        let evaluated = evaluate_expression(parsed);
        println!("{:?}", evaluated);
    }

}
