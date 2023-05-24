use std::str::Chars;

use crate::error;




// chars that signify the end of an identifier
const ID_BREAK: [char; 22] = [
    ' ',
    ',',
    '.',
    '>',
    '<',
    '!',
    '|',
    '&',
    ';', 
    '(',
    ')',
    '[',
    ']',
    '{',
    '}',
    '=',
    '+',
    '-',
    '*',
    '/',
    '\n', 
    ':'
];


#[derive(Debug, Clone)]
pub enum Token {
    Identifier(String), // stuff like variables and keywords
    Number(String),
    Char(String), // something encased in single quotes

    BinaryOperator(String), // + - * / etc
    Equals,
    And, // &
    Bar, // |
    Bang, // !
    DoubleQuote, 
    SingleQuote,
    Comma,
    Dot, 
    Colon,

    Angle   { open: bool }, // <>
    Bracket { open: bool }, // []
    Brace   { open: bool }, // {}
    Paren   { open: bool }, // ()

    EqualityOp,
    LessEqual,
    GreaterEqual,
    NotEqual,
    BinAnd,
    BinOr,
    
    Endline,
    Unassigned(String),
}
impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        use Token::*;
        
        match (self, other) {
            (Identifier(_), Identifier(_)) => true,
            (Number(_), Number(_)) => true,
            (Char(_), Char(_)) => true,
            (BinaryOperator(_), BinaryOperator(_)) => true,
            (Equals, Equals) => true,
            (And, And) => true,
            (Bar, Bar) => true,
            (Bang, Bang) => true,
            (DoubleQuote, DoubleQuote) => true,
            (SingleQuote, SingleQuote) => true,
            (Comma, Comma) => true,
            (Dot, Dot) => true,
            (Angle{open: o1}, Angle{open: o2}) if o1 == o2 => true,
            (Bracket{open: o1}, Bracket{open: o2}) if o1 == o2 => true,
            (Brace{open: o1}, Brace{open: o2}) if o1 == o2 => true,
            (Paren{open: o1}, Paren{open: o2}) if o1 == o2 => true,
            (EqualityOp, EqualityOp) => true,
            (LessEqual, LessEqual) => true,
            (GreaterEqual, GreaterEqual) => true,
            (NotEqual, NotEqual) => true,
            (BinOr, BinOr) => true,
            (BinAnd, BinAnd) => true,
            (Endline, Endline) => true,
            (Colon, Colon) => true,
            (Unassigned(v1), Unassigned(v2)) if v1 == v2 => true,
            _ => false,
        }
    }
}
impl Token {
    pub fn value(&self) -> String {
        use Token::*;
        match self {
            Identifier(id) => id.to_string(),
            Number(num) => num.to_string(),
            Char(ch) => ch.to_string(),
            BinaryOperator(symbol) => symbol.to_string(),
            Equals => "=".to_string(),
            And => "&".to_string(),
            Bar => "|".to_string(),
            Bang => "!".to_string(),
            DoubleQuote => "\"".to_string(),
            SingleQuote => "'".to_string(),
            Comma => ",".to_string(),
            Dot => ".".to_string(),
            Colon => ":".to_string(),
            Angle { open } => {
                if *open { "<".to_string() }
                else { ">".to_string() }
            },
            Bracket { open } => {
                if *open { "[".to_string() }
                else { "]".to_string() }
            },
            Brace { open } => {
                if *open { "{".to_string() }
                else { "}".to_string() }
            },
            Paren { open } => {
                if *open { "(".to_string() }
                else { ")".to_string() }
            },
            EqualityOp => "==".to_string(),
            LessEqual => "<=".to_string(),
            GreaterEqual => ">=".to_string(),
            NotEqual => "!=".to_string(),
            Endline => ";".to_string(),
            BinOr => "||".to_string(),
            BinAnd => "&&".to_string(),
            Unassigned(name) => name.to_string(),
        }
    }
}




pub fn tokenize(src: &str) -> Vec<Token> {
    use Token::*;
    let mut tokens = vec![];
    let mut chars = src.chars();

    let mut c = ' ';
    let mut next = true;
    loop {
        if next { c = if let Some(c) = chars.next() { c } else { break; }; }
        //println!("{}",c);    

        if c == '"' {
            c = if let Some(c) = chars.next() { c } else { break; };
            let (ch, mut str) = lex_char_arr(&mut chars, c);
            c = ch;
            tokens.append(&mut str);
            next = true; continue;
        }

        if c == '\'' {
            c = if let Some(c) = chars.next() { c } else { break; };
            let character = c.to_string();
            c = if let Some(c) = chars.next() { c } else { break; };
            if c != '\'' { panic!("Unclosed char"); }
            tokens.push(Char(character));
            c = if let Some(c) = chars.next() { c } else { break; };

            //next = ; continue;
        }

        if is_alpha(&c) {
            let (ch, tkn) = lex_identifier(&mut chars, c);
            c = ch;
            tokens.push(tkn);
            next = false; continue;
        }

        if is_num(&c) {
            let (ch, tkn) = lex_number(&mut chars, c);
            c = ch;
            tokens.push(tkn);
            next = false; continue;
        }


        match c {
            '+' => tokens.push(BinaryOperator(c.to_string())),
            '-' => tokens.push(BinaryOperator(c.to_string())),
            '*' => tokens.push(BinaryOperator(c.to_string())),
            '/' => tokens.push(BinaryOperator(c.to_string())),

            ',' => tokens.push(Comma),
            '=' => tokens.push(Equals), 
            '&' => tokens.push(And),
            '!' => tokens.push(Bang),
            '|' => tokens.push(Bar),
            '"' => tokens.push(DoubleQuote), 
            '\''=> tokens.push(SingleQuote), 
            '<' => tokens.push(Angle { open: true }),
            '>' => tokens.push(Angle { open: false }),
            '(' => tokens.push(Paren { open: true }), 
            ')' => tokens.push(Paren { open: false }), 
            '{' => tokens.push(Brace { open: true }), 
            '}' => tokens.push(Brace { open: false }), 
            '[' => tokens.push(Bracket { open: true }), 
            ']' => tokens.push(Bracket { open: false }), 
            '.' => tokens.push(Dot),

            ':' => tokens.push(Colon),
            ';' => tokens.push(Endline),
            '\n'=> (), //tokens.push(Token::Endline), 

            ' ' => (),
            _ => tokens.push(Unassigned(c.to_string()))
        }
        next = true;
    }

    let mut new_tkns = vec![tokens[0].clone()];
    for (i, token) in tokens.clone().iter().enumerate() {
        if i == 0 { continue; }

        match (new_tkns[new_tkns.len()-1].clone(), token) {
            (Equals, Equals) => {
                new_tkns.pop();
                new_tkns.push(EqualityOp);
            },
            (Bang, Equals) => {
                new_tkns.pop();
                new_tkns.push(NotEqual);
            },
            (Angle{open}, Equals) => {
                new_tkns.pop();
                if open {
                    new_tkns.push(LessEqual);
                } else { 
                    new_tkns.push(GreaterEqual);
                }
            },
            (Bar, Bar) => {
                new_tkns.pop();
                new_tkns.push(BinOr);
            },
            (And, And) => {
                new_tkns.pop();
                new_tkns.push(BinAnd);
            },
            _ => {
                new_tkns.push(token.clone());
            }
        }
    }
    
    return new_tkns;
}

fn lex_char_arr(chars: &mut Chars, mut c: char) -> (char, Vec<Token>) {
    let mut str = vec![Token::Bracket { open: true }];
    while c != '"' {
        str.push(Token::Char(c.to_string()));
        str.push(Token::Comma);
        c = if let Some(c) = chars.next() { c } else { error("Unclosed \" while parsing str"); break; }
    }
    str.push(Token::Bracket { open: false });

    return (c, str);
}
fn lex_identifier(chars: &mut Chars, mut c: char) -> (char, Token) { 
    let mut id = String::new();
    while !ID_BREAK.contains(&c) {
        id.push(c);
        c = if let Some(c) = chars.next() { c } else { break; };
    }
    
    return (c, Token::Identifier(id));
}
fn lex_number(chars: &mut Chars, mut c: char) -> (char, Token) {
    let mut num = String::new();
    while is_num(&c) {
        num.push(c);
        c = if let Some(c) = chars.next() { c } else { break; };
    }

    return (c, Token::Number(num));
}


fn is_alpha(c: &char) -> bool {
    let c = c.to_lowercase()
        .to_string()
        .chars()
        .next()
        .unwrap();
    return ('a'..='z').contains(&c);
}
fn is_num(c: &char) -> bool { ('0'..='9').contains(c) }













