use std::collections::HashMap;

use crate::{lexer::Token, error};






#[derive(Debug, Clone)]
pub enum AstNode {
    VarDeclaration(String),
    VarAssignment { id: String, data: Box<AstNode>, declare: bool },
    AdvanceAssignment { id: Box<AstNode>, data: Box<AstNode>, },
    FunctionDeclaration { 
        id: String, args: Vec<(AstNode, AstNode)>, rtrn_typ: Option<Box<AstNode>>, body: Box<AstNode> }, 
    StructDeclaration { id: String, properties: Vec<(AstNode, AstNode)> }, 

    Block(Vec<AstNode>),
    IfChain { condition: Box<AstNode>, body: Box<AstNode>, else_do: Option<Box<AstNode>> },
    BinaryExpr { left: Box<AstNode>, right: Box<AstNode>, op: String },
    ConditionChain { left: Box<AstNode>, right: Box<AstNode>, op: String },
    Condition { left: Box<AstNode>, right: Box<AstNode>, op: String },
    UnaryExpr  { sign: String, value: Box<AstNode> },
    Identifier(String),
    Literal(TypeKind, String),

    ArrayLiteral { type_id: Option<Box<AstNode>>, size: Option<Box<AstNode>>, list: Vec<AstNode> },
    ObjectLiteral { type_id: Option<String>, members: HashMap<String, AstNode> }, 
    MemberCall { parent: Box<AstNode>, member: Box<AstNode> },
    ArrayCall { id: Box<AstNode>, index: Option<Box<AstNode>> },

    FunctionCall { id: Box<AstNode>, params: Vec<AstNode> }, 
    WhileLoop { condition: Box<AstNode>, body: Box<AstNode> },
    Break,
    Continue,
    WriteExpr { data: Box<AstNode> },
    Return(Box<AstNode>),

    Void,
}
#[derive(Debug, Clone)]
pub enum TypeKind {
    Number,
    Char,
    Bool,
}

pub struct ProgramAST {
    pub nodes    :   Vec<AstNode>,
    pub tkns     :   Vec<Token>,
    pub at       :   usize
}

impl ProgramAST {
    pub fn parse(tkns: Vec<Token>) -> ProgramAST {
        let mut ast = ProgramAST {
            nodes: vec![],
            tkns,
            at: 0
        };
        
        let mut node = ast.parse_keyword();
        ast.nodes.push(node);
        while ast.has_next() {
            ast.next();
            //println!("{:?}", ast.at());
            node = ast.parse_keyword();
            //println!("{:#?}", node);
            ast.nodes.push(node.clone());
        }

        return ast;
    }

    fn has_next(&self) -> bool { self.at < self.tkns.len()-1 }
    fn has_last(&self) -> bool { self.at > 0 }
    fn next(&mut self) -> &Token { 
        self.at += 1;
        return self.tkns.get(self.at).unwrap_or_else(|| panic!("Cant get next token")); }
    fn last(&mut self) -> &Token {
        self.at -= 1;
        return self.tkns.get(self.at).unwrap_or_else(|| panic!("Cant get last token")); }
    fn at(&self) -> &Token {
        return self.tkns.get(self.at).unwrap_or_else(|| panic!("Cant get curr token")); }

    fn get_next(&self) -> &Token {
        return self.tkns.get(self.at + 1).unwrap_or_else(|| panic!("Failed to get next token")); }
    fn get_last(&self) -> &Token {
        return self.tkns.get(self.at - 1).unwrap_or_else(|| panic!("Failed to get last token")); }

    fn check_next(&mut self, msg: &str) {
        if !self.has_next() { error(msg); }
        self.at += 1; 
    }
    fn expect(&mut self, tkn: Token, msg: &str) {
        self.check_next(msg);
        if self.at() != &tkn { error(msg); }
    }
    
}

impl ProgramAST {
    fn parse_keyword(&mut self) -> AstNode {
        use Token::*;
        
        match self.at() {
            Identifier(id) if &id[..] == "write" => todo!(),
            Colon => {
                self.expect(Identifier("".to_string()), "Expected identifier");
                let id = self.parse_primary();
                let id_str = if let AstNode::Identifier(name) = id { name } else {
                    error("Expected identifier 1");
                    String::new()
                };

                if !self.has_next() || self.get_next() != &Equals {
                    return AstNode::VarDeclaration(id_str); } else { self.next(); }
                self.check_next("Expected value after Equals");

                let value = self.parse_array();

                return AstNode::VarAssignment { id: id_str, data: Box::new(value), declare: true };
            },
            Identifier(id) if &id[..] == "fn" => {
                self.expect(Identifier(String::new()), "Expected identifier");
                let id = if let AstNode::Identifier(name) = self.parse_primary() { name }
                else { error("Expected identifier"); String::new() };

                self.expect(Paren{open: true}, "Expected OpeningParen");
                self.check_next("Expected parameters");

                let mut args = vec![];
                while self.at() != &(Paren{open: false}) {
                    let typ = self.parse_keyword();
                    self.expect(Identifier("".to_string()), "Expected identifier after type");
                    let arg = self.parse_primary();
                    self.check_next("Expected Comma or ClosingParen");
                    if self.at() == &Comma { 
                        self.check_next("Expected parameter decloration or Closing Paren"); }
                    args.push((typ, arg));
                }
                self.check_next("Expected \': RETURN TYPE\' or function body");
                let mut return_type = None;
                if self.at() == &Colon {
                    self.check_next("Expected return type");
                    return_type = Some(Box::new(self.parse_primary()));
                    self.expect(Brace{open: true}, "Expected function body");
                }

                let body = self.parse_block();

                //println!("{:?}", self.at());
                return AstNode::FunctionDeclaration {
                    id, args, rtrn_typ: return_type, body: Box::new(body) };
            },
            Identifier(id) if &id[..] == "if" => {
                self.check_next("Expected condidition after if");
                let condition = self.parse_assignment();

                if let AstNode::ConditionChain {..} | AstNode::Condition {..} = condition
                {} else {
                    error("Expected condition after if"); }

                self.check_next("Expected block after condition");
                let body = self.parse_block();

                //println!("{:#?}", self.at());
                
                if self.has_next() && self.get_next().value() == "else" {
                    self.next();
                    //println!("else detected {:#?}", self.at());
                    let else_expr = self.parse_keyword();
                    return AstNode::IfChain { 
                        condition: Box::new(condition),
                        body: Box::new(body),
                        else_do: Some(Box::new(else_expr)) 
                    };
                } 
                

                return AstNode::IfChain { 
                    condition: Box::new(condition),
                    body: Box::new(body),
                    else_do: None 
                };
            },
            Identifier(id) if &id[..] == "while" => {
                self.check_next("Expected condition after while");
                let condition = self.parse_assignment();

                if let AstNode::ConditionChain {..} | AstNode::Condition {..} = condition
                {} else { error("Expected condition after while"); }

                self.check_next("Expected block after condition");
                let body = self.parse_block();

                return AstNode::WhileLoop { 
                    condition: Box::new(condition), 
                    body: Box::new(body) 
                };
            },
            
            Identifier(id) if &id[..] == "else" => {
                self.check_next("Expected if or block after else");
                if self.at() == &(Brace{open: true}) { 
                    let block = self.parse_block(); 
                    //if self.has_next() { self.next(); }
                    return block;
                }
                if self.at().value() != "if" { error("Expected id or block after else"); }

                return self.parse_keyword();
            },
            Identifier(id) if &id[..] == "struct" => {
                self.check_next("Expected identifier");
                let id = if let Identifier(name) = self.at() { name.to_string() }
                else { error("Expected identifier"); String::new() };
                self.expect(Brace{open: true}, "Expected struct fields");
                
                self.check_next("Expected TYPE IDENTIFIER");
                let mut props = vec![];
                while self.at() != &(Brace{open: false}) {
                    let node1 = self.parse_assignment();
                    self.check_next("Expected identifier");
                    let node2 = self.parse_assignment();

                    props.push((node1, node2));

                    self.check_next("Expected Comma or ClosingBrace");
                    if self.at() == &Comma && self.has_next() { self.next(); continue; }
                }

                return AstNode::StructDeclaration { id, properties: props };
            },
            
            Identifier(id) if &id[..] == "return" => {
                self.check_next("Expected return value\n    HINT: if this was an early return to a void function, then replace this with \'return void\'");

                let data = self.parse_assignment();
                return AstNode::Return(Box::new(data));
            },
            Identifier(id) if &id[..] == "break" => AstNode::Break,
            Identifier(id) if &id[..] == "continue" => AstNode::Continue,

            _ => self.parse_assignment()
        }
    }

    fn parse_block(&mut self) -> AstNode {
        use Token::*;
        if self.at() != &(Brace {open: true}) { return self.parse_assignment(); }
        if !self.has_next() { error("Expected block body"); }

        let mut body = vec![];
        while self.next() != &(Brace {open: false}) {
            //println!("{:?}", self.at());
            let node = self.parse_keyword();
            //println!("{:?}", self.at());
            //println!("{:?}", node);
            body.push(node.clone());
            //if self.has_next() && self.at() == &(Brace{open: false}) { self.next(); }
            if !self.has_next() { error("Expected ClosingBrace"); }
        }

        //println!("{:?}", body);
        return AstNode::Block(body);
    }
    fn parse_assignment(&mut self) -> AstNode {
        let mut left = self.parse_array();

        //println!("{:?}", self.at());
        if self.at() == &Token::Colon { return left; }
        if !self.has_next() { return left; }
        if let AstNode::Identifier(ref name) = left {
            if self.get_next() != &Token::Equals { return left; } else { self.next(); }
            self.check_next("Expected value after Equals");
            let right = self.parse_array();
            left = AstNode::VarAssignment { 
                id: name.to_string(),
                data: Box::new(right),
                declare: false 
            };
        } 
        else if let AstNode::MemberCall {..} | AstNode::ArrayCall {..} = left {
            if self.get_next() != &Token::Equals { return left; } else { self.next(); }
            self.check_next("Expected value after Equals");
            let right = self.parse_array();
            left = AstNode::AdvanceAssignment { 
                id: Box::new(left),
                data: Box::new(right)
            };
        }

        return left;
    }
    fn parse_array(&mut self) -> AstNode {
        use Token::*;
        if self.at() != &(Bracket{open: true}) { return self.parse_object(); }
        self.check_next("Expected data type or array value");

        if self.at() == &(Bracket {open: false}) {
            return AstNode::ArrayLiteral {
                type_id: None,
                size: None,
                list: vec![]
            };
        }

        let obj = self.parse_object();
        if let AstNode::ArrayCall{..} | AstNode::Identifier(..) = obj {
            self.check_next("Expected data or ClosingBracket");
            if self.at() == &Endline {
                self.check_next("Expected array size");
                let size = self.parse_object();

                self.expect(Bracket{open: false}, "Expected ClosingBracket");
                return AstNode::ArrayLiteral { 
                    type_id: Some(Box::new(obj)),
                    size: Some(Box::new(size)),
                    list: vec![AstNode::Void]
                };
            } else {
                self.last();
            }
        }
        
        let values = self.parse_csvs();
        if self.at() != &(Bracket{open: false}) { error("Expected ClosingBracket"); }

        //if self.has_next() { self.next(); }
        return AstNode::ArrayLiteral { type_id: None, size: None, list: values };
    }
    fn parse_object(&mut self) -> AstNode {
        use Token::*;
        let id = if self.at() == &(Identifier("".to_string())) {
            let id = self.parse_condition_chain();
            if let AstNode::Identifier(_) = id {
                if !self.has_next() { return id; } 
                if self.next() != &(Brace {open: true}) { self.last(); return id; }
                id
            } else { return id; }
        } else { AstNode::Void };

        if self.at() != &(Brace {open: true}) { return self.parse_condition_chain(); }

        self.check_next("Expected data after OpeningBrace");

        let mut props = HashMap::new();
        while self.at() != &(Brace {open: false}) {
            let id = if let Identifier(name) = self.at() {
                name.clone()
            } else { error("Expected property name"); String::new() };

            self.expect(Colon, "Expected Colon");
            self.check_next("Expected value after Colon");

            let data = self.parse_assignment();
            props.insert(id, data);

            self.check_next("Expected Comma or ClosingBrace");
            if self.at() == &Comma { self.check_next("Expected property name"); }
        }

        if let AstNode::Identifier(name) = id {
            return AstNode::ObjectLiteral { 
                type_id: Some(name),
                members: props 
            };
        }
        return AstNode::ObjectLiteral { type_id: None, members: props };
    }
    
    fn parse_condition_chain(&mut self) -> AstNode {
        let left = self.parse_condition();
        let mut node = AstNode::Void;
        let mut extra_op = false;

        if !self.has_next() { return left } else { self.next(); }
        use Token::*;

        while self.at() == &BinAnd || self.at() == &BinOr {
            let op = self.at().value();
            self.check_next("Expected a value");
            let right = self.parse_condition();

            if extra_op {
                node = AstNode::ConditionChain {
                    left: Box::new(node),
                    right: Box::new(right),
                    op
                };
            } else {
                node = AstNode::ConditionChain {
                    left: Box::new(left.clone()),
                    right: Box::new(right),
                    op
                };
            }
            extra_op = true;
            if self.has_next() { self.next(); }
        }

        if let AstNode::Void = node { self.last(); return left; }
        return node;
    }
    fn parse_condition(&mut self) -> AstNode {
        let left = self.parse_additive();

        if !self.has_next() { return left; } else { self.next(); }
        use Token::*;
        
        let condition_tokens = [
            EqualityOp,
            NotEqual,
            GreaterEqual,
            LessEqual,
            Angle { open: true },
            Angle { open: false },
        ];
        if !condition_tokens.contains(self.at()) { self.last(); return left; }
        let op = self.at().value();

        self.check_next(&format!("Expected value after {}", op));
        let right = self.parse_additive();

        return AstNode::Condition { left: Box::new(left), right: Box::new(right), op };
    }
    
    fn parse_additive(&mut self) -> AstNode {
        let left = self.parse_multiplicative();
        let mut node = AstNode::Void;
        let mut extra_op = false;

        if !self.has_next() { return left; } else { self.next(); }
        while self.at().value() == "+" || self.at().value() == "-" {
            let op = self.at().value();
            self.check_next("Expected a value");
            let right = self.parse_multiplicative();

            if extra_op {
                node = AstNode::BinaryExpr { 
                    left: Box::new(node),
                    right: Box::new(right),
                    op
                };
            } else {
                node = AstNode::BinaryExpr { 
                    left: Box::new(left.clone()),
                    right: Box::new(right),
                    op
                };
            }
            extra_op = true;
            if self.has_next() && (self.at().value() == "+" || self.at().value() == "-" ) { self.next(); }
        }

        if let AstNode::Void = node { self.last(); return left; }
        return node;
    }
    fn parse_multiplicative(&mut self) -> AstNode {
        let left = self.parse_unary();
        let mut node = AstNode::Void;
        let mut extra_op = false;

        if !self.has_next() { return left; } else { self.next(); }
        while self.at().value() == "*" || self.at().value() == "/" {
            let op = self.at().value();
            self.check_next("Expected a value");
            let right = self.parse_unary();

            if extra_op {
                node = AstNode::BinaryExpr { 
                    left: Box::new(node),
                    right: Box::new(right),
                    op
                };
            } else {
                node = AstNode::BinaryExpr { 
                    left: Box::new(left.clone()),
                    right: Box::new(right),
                    op
                };
            }
            extra_op = true;
            if self.has_next() && (self.at().value() == "*" || self.at().value() == "/" ) { self.next(); }
        }

        if let AstNode::Void = node { self.last(); return left; }
        return node;
    }
    fn parse_unary(&mut self) -> AstNode {
        if !["-"].contains(&&self.at().value()[..]) || !self.has_next() { return self.parse_call(); }
        let sign = self.at().value();
        self.next();
        return AstNode::UnaryExpr { sign, value: Box::new(self.parse_call()) };
    }
    
    fn parse_call(&mut self) -> AstNode {
        use Token::*;
        let id = self.parse_array_call();
        if self.at() != &(Identifier(String::new())) { return id; }
        if !self.has_next() || self.get_next() != &(Paren{open: true}) { return id; } else { self.next(); }

        self.check_next("Expected arguments or ClosingParen");
        if self.at() == &(Paren{open: false}) {
            return AstNode::FunctionCall { id: Box::new(id), params: vec![] }; }

        let args = self.parse_csvs();
        if self.at() != &(Paren{open: false}) {
            self.expect(Paren{open: false}, "Exepcted ClosingParen"); }
        return AstNode::FunctionCall { id: Box::new(id), params: args };
    }
    fn parse_array_call(&mut self) -> AstNode {
        let id = self.parse_member();
        //println!("{:?}", id);
        if !self.has_next() || self.get_next() != &(Token::Bracket{open: true}) { return id; }
        self.next();
        //println!("{:?} passed, next is {:?}", id, self.get_next());
    
        self.check_next("Expected ClosingBracket or index");
        if self.at() == &(Token::Bracket{open: false}) {
            //println!("TYPE");
            return AstNode::ArrayCall{id: Box::new(id), index: None};
        }

        let index = self.parse_assignment();
        self.expect(Token::Bracket{open: false}, "Expected ClosingBracket");
        
        return AstNode::ArrayCall { id: Box::new(id), index: Some(Box::new(index)) };
    }
    fn parse_member(&mut self) -> AstNode {
        use Token::*;

        if self.at() != &Identifier("".to_string()) || !self.has_next() { return self.parse_float(); }
        let parent = self.parse_float();

        if !self.has_next() || self.get_next() != &Dot { return parent; }
        self.next();
        self.expect(Identifier(String::new()), "Expected member identifier after Dot");

        return AstNode::MemberCall{ parent: Box::new(parent), member: Box::new(self.parse_member()) };
    }
    fn parse_float(&mut self) -> AstNode {
        use Token::*;
        let mut found_dot = false;
        if self.at() == &Dot { 
            self.expect(Number("".to_string()), "Expected number after decimal"); found_dot = true; }
        if self.at() != &(Number("".to_string())) { return self.parse_primary(); }
        let mut num = self.at().value();

        if found_dot { return AstNode::Literal(TypeKind::Number, format!(".{}", num)); }
        if !self.has_next() || self.get_next() != &Dot { return self.parse_primary(); }
        self.next();
        num += ".";
        if self.has_next() && self.get_next() == &(Number("".to_string())) {
            num += &self.next().value(); } 
        return AstNode::Literal(TypeKind::Number, num);
    }

    fn parse_primary(&mut self) -> AstNode {
        use Token::*;
        use TypeKind as typ;

        match self.at().clone() {
            Number(value) => AstNode::Literal(typ::Number, value.to_string()),
            Char(value) => AstNode::Literal(typ::Char, value.to_string()),
            Identifier(id) if &id[..] == "true" => AstNode::Literal(typ::Bool, id.to_string()),
            Identifier(id) if &id[..] == "false" => AstNode::Literal(typ::Bool, id.to_string()),
            
            Identifier(name) => {
                let id = AstNode::Identifier(name.clone());
                return id;
            },

            _ => { error(&format!("Unidentified token: {:?}", self.at())); AstNode::Void }
        }
    }



    
    fn parse_csvs(&mut self) -> Vec<AstNode> {
        use Token::*;
        let mut val = self.parse_assignment();
        let mut csvs = vec![val];
        if !self.has_next() || self.get_next() != &Comma { return csvs; }
        
        while self.next() == &Comma {
            if self.next() == &(Bracket{open: false}) || self.at() == &(Paren{open: false}) { 
                return csvs; }
            val = self.parse_assignment();
            csvs.push(val);

            if !self.has_next() { return csvs; }
        }

        return csvs;
    }
}




































