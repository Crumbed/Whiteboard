





use std::collections::{HashMap, LinkedList};
use std::env;
use std::slice::Iter;
use std::sync::{Mutex, Arc};

use crate::error;
use crate::parser::{
    AstNode, 
    TypeKind,
    ProgramAST
};

use crate::runtime::{RuntimeValue, ScopeKind, Var};
use crate::runtime::cast;
use crate::runtime::DataType;
use crate::runtime::Env;

#[derive(Debug)]
pub struct Program {
    pub last_val    :   RuntimeValue,
    pub last_node   :   AstNode, 
    pub nodes       :   LinkedList<LinkedList<AstNode>>,
    pub env_stack   :   Vec<Arc<Mutex<Env>>>,
    pub env         :   Arc<Mutex<Env>>
}


impl Program {
    pub fn new(env: Env) -> Self {
        let env = Arc::new(Mutex::new(env));
        
        Program { 
            last_val: RuntimeValue::Void,
            last_node: AstNode::Void,
            nodes: LinkedList::new(),
            env: Arc::clone(&env), 
            env_stack: vec![env],
        }
    }
    pub fn eval_ast(&mut self, ast: ProgramAST) -> RuntimeValue {
        use RuntimeValue::*;
        
        self.nodes.push_front(ast.nodes   
            .iter()
            .cloned()
            .collect());
        let mut at = self.nodes.front_mut().unwrap().pop_front();
        
        while at.is_some() {
            let node = at.unwrap();
            //println!("{}", index);
            //println!("{}", self.env.lock().unwrap().contains_var("fn:test"));
            println!("{:#?}", node);
            let mut rt_val = self.eval_rt_value(node.clone());
            self.last_val = rt_val.clone();
            //println!("{:?}\n", self.env.lock().unwrap().kind);


            match rt_val.clone() {
                Return(data) => {
                    let par_count = self.env
                        .lock()
                        .unwrap()
                        .end_scope(&rt_val, 0);

                    //println!("{:#?}", self.env.lock().unwrap());
                    //println!("{}", par_count);
                    if par_count == -1 { error("Invalid return expression"); }
                    else if par_count == 0 {
                        self.nodes.front_mut().unwrap().clear();
                        self.last_val = Return(data.clone());
                        rt_val = *data;
                    } else {
                        for _ in 0..=par_count {
                            self.nodes.pop_front();
                        }
                        self.nodes.front_mut().unwrap().clear();

                        self.last_val = Return(data.clone());
                        rt_val = *data;
                    }
                },
                Break => {
                    let par_count = self.env
                        .lock()
                        .unwrap()
                        .end_scope(&rt_val, 0);

                    if par_count == -1 { error("Invalid break expression"); }
                    else if par_count == 0 {
                        self.nodes.front_mut().unwrap().clear();
                    } else {
                        for _ in 0..=par_count {
                            self.nodes.pop_front();
                        }
                        self.nodes.front_mut().unwrap().clear();
                    }
                },
                Continue => {
                    let kind = self.env
                        .lock()
                        .unwrap()
                        .kind.clone();

                    let (condition, body) = if let ScopeKind::Loop{condition, body} = kind {
                        (condition, body) } else { panic!("how..") };
                    
                    let cond = self.eval_rt_value(condition);
                    if cond == Bool(true) {
                        self.nodes.pop_front();
                        self.nodes.push_front(body
                                             .iter()
                                             .cloned()
                                             .collect());
                    }
                }
                _ => (),
            }
            
            //println!("{:#?}", rt_val);
            //println!("{}", index);
            if let RuntimeValue::Call { ref kind, ref nodes } = rt_val {
                let mut nodes = nodes.clone();
                //println!("call type: {:?}", kind);
                match kind.clone() {
                    ScopeKind::Program => todo!(),
                    ScopeKind::Module => todo!(),
                    ScopeKind::Function{args, passed,..} => {
                        let mut env = Env::new(
                            Some(Arc::clone(&self.env_stack[0])),
                            kind.clone()
                        );

                        for (i, node) in passed.clone().iter().enumerate() {
                            let value = if let AstNode::Identifier(id) = node.clone() {
                                let ptr = self.env
                                    .lock()
                                    .unwrap()
                                    .get_var_ref(&id);

                                RuntimeValue::VarReferance(ptr)
                            } else {
                                self.eval_rt_value(node.clone())
                            };

                            if value.get_type() != args[i].0 { error(&format!(
                                "function expected type {} but found {}", args[i].0, value.get_type()));}

                            env.declare_var(&args[i].1, &value);
                        }

                        let env = Mutex::new(env);
                        self.env_stack.push(
                            Arc::new(env)
                        );
                        self.env = Arc::clone(&self.env_stack[self.env_stack.len()-1]);
                    },
                    ScopeKind::IfBlock => {
                        let env = Arc::new(Mutex::new(
                            Env::new(
                                Some(Arc::clone(&self.env)), 
                                kind.clone())
                        ));
                        self.env_stack.push(
                            env
                        );
                        self.env = Arc::clone(&self.env_stack[self.env_stack.len() - 1]);
                    },
                    ScopeKind::Loop { condition,.. } => {
                        let cond = self.eval_rt_value(condition);
                        if cond == Bool(true) {
                            let env = Arc::new(Mutex::new(Env::new(
                                Some(Arc::clone(&self.env_stack[0])),
                                kind.clone()
                            )));

                            self.env_stack.push(env);
                            self.env = Arc::clone(&self.env_stack[self.env_stack.len() - 1]);
                        } else {
                            nodes.clear();
                        }
                    }
                }

                self.nodes.push_front(nodes
                    .iter()
                    .cloned()
                    .collect());
            }


            if self.nodes.is_empty() { break; }
            if self.nodes.front().unwrap().is_empty() {
                let kind = self.env
                    .lock()
                    .unwrap()
                    .kind.clone();

                if let ScopeKind::Loop{condition, body} = kind {
                    let cond = self.eval_rt_value(condition);
                    if cond == Bool(true) {
                        self.nodes.pop_front();
                        self.nodes.push_front(body
                                              .iter()
                                              .cloned()
                                              .collect());

                        at = self.nodes.front_mut().unwrap().pop_front();
                        continue;
                    }
                }
                
                self.env_stack.pop();
                if self.env_stack.is_empty() { break; }
                self.env = Arc::clone(&self.env_stack[self.env_stack.len() - 1]);
                //println!("{:?}", self.env.lock().unwrap().contains_var("x"));
                //println!("{:?}", self.env.lock().unwrap().contains_var("$RETURN"));
                //println!("last {:?}", self.last_val);
                if self.env.lock().unwrap().contains_var("$RETURN") {
                    if let Return(_) = self.last_val {
                        self.env
                            .lock()
                            .unwrap()
                            .assign_var("$RETURN", &rt_val);
                    } else {
                        let typ = self.env.lock().unwrap().get_var("$RETURN").get_type();

                        if typ != DataType::Void { 
                            error(&format!("expected return typ {} but found void", typ)); }
                    }
                }
                self.nodes.pop_front();
                if self.nodes.is_empty() { break; }
            }
            at = self.nodes.front_mut().unwrap().pop_front();
            //println!("{}", node_counts[self.env_stack.len()-1]);
        }

        return self.last_val.clone();
    }

    pub fn eval_rt_value(
        &mut self, 
        node: AstNode,
    ) -> RuntimeValue {
        use RuntimeValue::*;
        
        match node.clone() {
            AstNode::Void => RuntimeValue::Void,
            // remember to add explicit typing here.
            AstNode::VarDeclaration(id) => {
                self.last_node = node;
                let value = RuntimeValue::Void;
                self.env
                    .lock()
                    .unwrap()
                    .declare_var(&id, &RuntimeValue::Void); 
                return value;
            },

            AstNode::VarAssignment { id, data, declare } => {
                let typ_spec = self.last_val.clone();
                let mut data = self.eval_rt_value(*data);
                if let TypeSpecifier(new_typ) = typ_spec {
                    let typ = data.get_type();
                    if typ != new_typ { data = match new_typ { 
                        DataType::Int if typ.can_cast(&new_typ) => cast(data, new_typ),
                        DataType::Float if typ.can_cast(&new_typ) => cast(data, new_typ),
                        DataType::Bool if typ.can_cast(&new_typ) => cast(data, new_typ),
                        DataType::Char if typ.can_cast(&new_typ) => cast(data, new_typ),
                        DataType::Object(_) if typ.can_cast(&new_typ) => cast(data, new_typ),
                        DataType::Array(_) if typ.can_cast(&new_typ) => cast(data, new_typ),
                        _ => { error("Invalid cast"); return Void; },
                    }}
                }

                if let Call { kind, nodes } = data.clone() {
                    if let ScopeKind::Function {rtrn_t,..} = kind {
                        let nodes = self.nodes.front_mut().unwrap();
                        nodes.push_front(AstNode::VarAssignment { 
                            id,
                            data: Box::new(AstNode::Identifier("$RETURN".to_string())),
                            declare
                        });
                        return data;
                    }
                }

                if declare {
                    self.env
                        .lock()
                        .unwrap()
                        .declare_var(&id, &data);
                } else {
                    self.env
                        .lock()
                        .unwrap()
                        .assign_var(&id, &data); 
                }

                return data;
            }, 
            AstNode::AdvanceAssignment { id, data } => {
                let id = *id;
                let data = self.eval_rt_value(*data);
                        p_val = (p_par, Some(mem));
                    },
                    
                }
                
                todo!()
            },
            

            AstNode::StructDeclaration { id, properties } => {
                self.last_node = node;
                self.eval_struct_declaration(&id, properties)
            },

            // LEFT OFF HERE
            AstNode::FunctionDeclaration { id, args, rtrn_typ, body } => {
                let body = if let AstNode::Block(arr) = *body { arr } else { panic!("HOW"); };

                let mut new_args = vec![];
                for (typ, id) in args {
                    let typ = self.eval_rt_value(typ);
                    let t = if let TypeSpecifier(_) = typ {
                        typ.get_type()
                    } else if let Struct{id,..} = typ {
                        DataType::Object(id)
                    } else { error(&format!("Invalid type {:?}", typ)); DataType::Void };

                    let id = if let AstNode::Identifier(name) = id { name } else {
                        error(&format!("Expected identifier but found {:?}", id));
                        String::new()
                    };

                    new_args.push((t, id));
                }

                let rtrn_t = if rtrn_typ.is_some() {
                    let rtrn_typ = self.eval_rt_value(*rtrn_typ.unwrap());
                    if let TypeSpecifier(_) = rtrn_typ {
                        rtrn_typ.get_type()
                    } else if let Struct{id,..} = rtrn_typ {
                        DataType::Object(id)
                    } else { error(&format!("Invalid type {:?}", rtrn_typ.get_type())); DataType::Void }
                } else { DataType::Void };

                let data = RuntimeValue::Function { 
                    id: id.clone(), 
                    args: new_args, 
                    rtrn_t,
                    body
                };

                self.last_node = node;
                //println!("{:?}", data);
                self.env
                    .lock()
                    .unwrap()
                    .declare_var(&format!("fn:{id}"), &data);
                return data;
            },


            AstNode::BinaryExpr {..} => {
                self.last_node = node.clone();
                return self.eval_binary_expr(node);
            },

            AstNode::UnaryExpr {sign, value} => {
                let value = self.eval_rt_value(*value);
                match &sign[..] {
                    "-" => {
                        match value {
                            Int(v) => Int(-v),
                            Float(v) => Float(-v),
                            _ => {
                                error(&format!("Unary operator - cannot be applied to {:?}", value));
                                Void
                            }
                        }
                    },
                    "+" => {
                        match value {
                            Int(v) => Int(v.abs()),
                            Float(v) => Float(v.abs()),
                            _ => {
                                error(&format!("Unary operator + cannot be applied to {:?}", value));
                                Void
                            }
                        }
                    },
                    _ => { error(&format!("Invalid unary operator {}", sign)); Void }
                }
            },

            AstNode::ConditionChain { left, right, op } => {
                let l = self.eval_rt_value(*left);
                let r = self.eval_rt_value(*right);
                let (l, r) = if let (Bool(b1), Bool(b2)) = (l, r) {
                    (b1, b2)
                } else { panic!("bad") };

                let val = match &op[..] {
                    "&&" => l && r,
                    "||" => l || r,
                    _ => panic!("wtf how.")
                };

                return Bool(val);
            },
            
            AstNode::Condition { left, right, op } => {
                let l = self.eval_rt_value(*left);
                let r = self.eval_rt_value(*right);

                Bool(self.eval_condition(l, r, &op))
            },


            AstNode::Literal(kind, v) => {
                match kind {
                    TypeKind::Number => {
                        self.last_node = node;
                        if v.contains(".") {
                            let v: f64 = match v.parse() {
                                Ok(value) => value,
                                Err(_) => {
                                    error(&format!("Number, {v}, is outside acceptable range."));
                                    return Void;
                                } 
                                    
                            };
                            return Float(v);
                        }
                        let v: i32 = match v.parse() {
                            Ok(value) => value, 
                            Err(_) => {
                                error(&format!("Number, {v}, is outside acceptable range."));
                                return Void;
                            } 
                        };
                        return Int(v);
                    },
                    TypeKind::Char => {
                        self.last_node = node;
                        let v: char = match v.chars().next() {
                            Some(v) => v, 
                            None => return RuntimeValue::Void
                        };

                        return Char(v); 
                    },
                    TypeKind::Bool => {

                        self.last_node = node;
                        let v: bool = match v.parse() {
                            Ok(v) => v,
                            Err(_) => {
                                error(&format!("{v} is not a boolean value."));
                                return Void;
                            } 
                        };

                        return Bool(v); 
                    }

                }
            },

            AstNode::ObjectLiteral { type_id, members } => {
                self.last_node = node;
                self.eval_object_expr(type_id, members)
            },

            AstNode::ArrayLiteral { type_id, size, list } => {
                let d_type: DataType;
                if type_id.is_some() {
                    let type_id = type_id.unwrap();
                    d_type = match &type_id[..] {
                        "int" => DataType::Int, 
                        "float" => DataType::Float, 
                        "char" => DataType::Char, 
                        "bool" => DataType::Bool, 
                        "Object" => DataType::Object("Object".to_string()), 
                        _ => {
                            if !self.env
                                .lock()
                                .unwrap()
                                .contains_var(&format!("s:{}", type_id)) {
                                
                                error(&format!("{} is not a valid type", type_id));
                            }

                            DataType::Object(type_id.to_string())
                        },
                    }
                } else { d_type = DataType::Void; }

                self.last_node = node;
                return self.eval_array_expr(d_type, size, list);
            }, 


            AstNode::Identifier(id) => { 
                self.last_node = node;
                self.eval_identifier(&id)
            },
            AstNode::MemberCall { parent, member } => {
                let (p_par, mem) = unsafe { self.eval_member(*parent, *member) };
                let par = unsafe { &*p_par };

                if let Object {properties,..} = par {
                    match mem {
                        AstNode::Identifier(id) if properties.contains_key(&id) => {
                            return properties.get(&id).unwrap().clone();
                        },

                        _ => { error("Invalid member call"); return Void; }
                    }
                }
    
                error("Invalid member call");
                return Void; 
            },
            AstNode::ArrayCall { id, index } => {
                if index.is_some() { return self.eval_array_call(*id, *index.unwrap()); }

                let id = if let AstNode::Identifier(name) = *id { name } else {
                    error("Expected data type");
                    String::new()
                };
                self.last_node = node;
                match &id[..] {
                    "int" => TypeSpecifier(DataType::Array(Box::new(DataType::Int))),
                    "float" => TypeSpecifier(DataType::Array(Box::new(DataType::Float))),
                    "bool" => TypeSpecifier(DataType::Array(Box::new(DataType::Bool))),
                    "char" => TypeSpecifier(DataType::Array(Box::new(DataType::Char))),
                    "Object" => TypeSpecifier(
                        DataType::Array(Box::new(DataType::Object(String::from("Object"))))),
                    _ => {
                        if !self.env
                            .lock()
                            .unwrap()
                            .contains_var(&format!("s:{}", id)) { error(&format!("Invalid type {}", id)); }

                        TypeSpecifier(
                            DataType::Array(Box::new(DataType::Object(id)))
                        )
                    }
                }
            },

            AstNode::IfChain { condition, body, else_do } => {
                let cond = self.eval_rt_value(*condition);
                let b = if let RuntimeValue::Bool(b) = cond { b } else { panic!("AGAIN.. HOW"); };

                if b {
                    if let AstNode::Block(nodes) = *body {
                        return Call { kind: ScopeKind::IfBlock, nodes };
                    }
                    error("Expected block");
                    return Void;
                }

                if else_do.is_some() {
                    let else_do = *else_do.unwrap();
                    if let AstNode::Block(nodes) = else_do.clone() {
                        return Call { kind: ScopeKind::IfBlock, nodes };
                    } else if let AstNode::IfChain {..} = else_do.clone() {
                        let rt = self.eval_rt_value(else_do.clone());
                        return rt;
                    }
                    error("Expected block");
                    return Void;
                }
                return RuntimeValue::Void;
            },
            AstNode::FunctionCall { id, params } => {
                let id = if let AstNode::Identifier(name) = *id { name } else {
                    error("Expected function identifier");
                    String::new()
                };

                // predefined funcs
                match &id[..] {
                    "len" => {
                        if params.len() != 1 { error(&format!(
                            "len expected 1 argument but found {}", params.len())); }

                        let rt = self.eval_rt_value(params[0].clone());
                        if let RuntimeValue::Array {length,..} = rt {
                            return Int(length as i32);
                        } else {
                            error(&format!("len expected type Array but found {}", rt.get_type()));
                        };
                    },
                    _ => ()
                }
    
                
                let func = self.env
                    .lock()
                    .unwrap()
                    .get_var(&format!("fn:{}", id));

                let (args, rtrn_t, body) = if let Function {args, rtrn_t, body,..} = func {
                    (args, rtrn_t, body) } else {
                    error("FUNC wasnt a function, this shouldnt happen");
                    (vec![], DataType::Void, vec![]) };

                if params.len() != args.len() { error(&format!(
                    "{} takes in {} arguments but {} were given", id, args.len(), params.len())); }

                self.env
                    .lock()
                    .unwrap()
                    .vars
                    .insert("$RETURN".to_string(), Arc::new(Mutex::new(Var {
                        id: "$RETURN".to_string(),
                        d_type: rtrn_t.clone(),
                        value: TypeSpecifier(rtrn_t.clone()),
                        constant: false
                    })));
                return Call { kind: ScopeKind::Function {rtrn_t, args, passed: params}, nodes: body };
            },
            AstNode::WhileLoop { condition, body } => {
                let body = if let AstNode::Block(arr) = *body { arr } else { panic!("HOW"); };

                return Call {kind: ScopeKind::Loop{condition: *condition, body: body.clone()}, nodes: body};
            }
            
            AstNode::WriteExpr {..} => todo!(),
            AstNode::Block(_) => { error("Block should be contained"); Void },
            AstNode::Break => RuntimeValue::Break,
            AstNode::Continue => RuntimeValue::Continue,
            AstNode::Return(data) => RuntimeValue::Return(Box::new(self.eval_rt_value(*data))),
        }
    }

    unsafe fn eval_member(&mut self, parent: AstNode, member: AstNode) -> (*mut RuntimeValue, AstNode) {
        use RuntimeValue::*;
        let mut p_par = if let AstNode::Identifier(id) = parent.clone() {
            let arc_var = self.env
                .lock()
                .unwrap()
                .get_var_ref(&id);
            let mut var = arc_var.lock().unwrap();

            &mut var.value as *mut RuntimeValue
        } else {
            &mut self.eval_rt_value(parent) as *mut RuntimeValue };

        let par = &mut *p_par;
        let mut mem = member;

        while let AstNode::MemberCall { parent, member } = mem {
            let props: &mut HashMap<String, RuntimeValue> = if let Object {properties,..} = par {
                properties
            } else { error("Only objects can have members"); return 
                (&mut Void as *mut RuntimeValue, AstNode::Void); };

            match *parent {
                AstNode::Identifier(m_id) => if props.contains_key(&m_id) {
                    p_par = props.get_mut(&m_id).unwrap() as *mut RuntimeValue;
                },

                _ => { error("Invalid member call"); return 
                    (&mut Void as *mut RuntimeValue, AstNode::Void); }
            }

            mem = *member;
        }

        if let Object {..} = *p_par {
            return (p_par, mem);
        } else {
            error("Only objects can have members");
            return (&mut Void as *mut RuntimeValue, AstNode::Void); 
        };
    }
    fn eval_condition(&mut self, l: RuntimeValue, r: RuntimeValue, op: &str) -> bool {
        use RuntimeValue::*;
        
        let rt = match op {
            "==" | "!=" => match (l.clone(), r.clone()) {
                (Int(i1), Int(i2)) => i1 == i2,
                (Float(f1), Float(f2)) => f1 == f2,
                (Char(c1), Char(c2)) => c1 == c2,
                (Bool(b1), Bool(b2)) => b1 == b2,
                (Object{name: n1, properties: p1}, Object{name: n2, properties: p2}) => {
                    if n1 != n2 { 
                        false 
                    } else {
                        p1 == p2
                    }
                },
                (Array{arr: a1,..}, Array{arr: a2,..}) => a1 == a2,
                
                _ if l.get_type() == r.get_type() => {
                    if let VarReferance(ptr) = l {
                        let var = ptr.lock().unwrap();
                        return self.eval_condition(var.value.clone(), r, op);
                    } else 
                    if let VarReferance(ptr) = r {
                        let var = ptr.lock().unwrap();
                        return self.eval_condition(l, var.value.clone(), op);
                    }
                    return false;
                },

                _ => {
                    error(&format!(
                        "Cannot get equality of {} and {}", l.get_type(), r.get_type()));
                    false
                }
            },

            ">" => match (l.clone(), r.clone()) {
                (Int(i1), Int(i2)) => i1 > i2,
                (Float(f1), Float(f2)) => f1 > f2,
                (Char(c1), Char(c2)) => c1 > c2,
                (Array{length: l1,..}, Array{length: l2,..}) => l1 > l2,

                _ if l.get_type() == r.get_type() => {
                    if let VarReferance(ptr) = l {
                        let var = ptr.lock().unwrap();
                        return self.eval_condition(var.value.clone(), r, op);
                    } else 
                    if let VarReferance(ptr) = r {
                        let var = ptr.lock().unwrap();
                        return self.eval_condition(l, var.value.clone(), op);
                    }
                    return false;
                },

                _ => {
                    error(&format!(
                        "Cannot get size comparison of {} and {}", l.get_type(), r.get_type()));
                    false
                }

            },

            ">=" => match (l.clone(), r.clone()) {
                (Int(i1), Int(i2)) => i1 >= i2,
                (Float(f1), Float(f2)) => f1 >= f2,
                (Char(c1), Char(c2)) => c1 >= c2,
                (Array{length: l1,..}, Array{length: l2,..}) => l1 >= l2,
                
                _ if l.get_type() == r.get_type() => {
                    if let VarReferance(ptr) = l {
                        let var = ptr.lock().unwrap();
                        return self.eval_condition(var.value.clone(), r, op);
                    } else 
                    if let VarReferance(ptr) = r {
                        let var = ptr.lock().unwrap();
                        return self.eval_condition(l, var.value.clone(), op);
                    }
                    return false;
                },

                _ => {
                    error(&format!(
                        "Cannot get size comparison of {} and {}", l.get_type(), r.get_type()));
                    false
                }

            },

            "<" => match (l.clone(), r.clone()) {
                (Int(i1), Int(i2)) => i1 < i2,
                (Float(f1), Float(f2)) => f1 < f2,
                (Char(c1), Char(c2)) => c1 < c2,
                (Array{length: l1,..}, Array{length: l2,..}) => l1 < l2,
                
                _ if l.get_type() == r.get_type() => {
                    if let VarReferance(ptr) = l {
                        let var = ptr.lock().unwrap();
                        return self.eval_condition(var.value.clone(), r, op);
                    } else 
                    if let VarReferance(ptr) = r {
                        let var = ptr.lock().unwrap();
                        return self.eval_condition(l, var.value.clone(), op);
                    }
                    return false;
                },

                _ => {
                    error(&format!(
                        "Cannot get size comparison of {} and {}", l.get_type(), r.get_type()));
                    false
                }

            },

            "<=" => match (l.clone(), r.clone()) {
                (Int(i1), Int(i2)) => i1 <= i2,
                (Float(f1), Float(f2)) => f1 <= f2,
                (Char(c1), Char(c2)) => c1 <= c2,
                (Array{length: l1,..}, Array{length: l2,..}) => l1 <= l2,
                
                _ if l.get_type() == r.get_type() => {
                    if let VarReferance(ptr) = l {
                        let var = ptr.lock().unwrap();
                        return self.eval_condition(var.value.clone(), r, op);
                    } else 
                    if let VarReferance(ptr) = r {
                        let var = ptr.lock().unwrap();
                        return self.eval_condition(l, var.value.clone(), op);
                    }
                    return false;
                },

                _ => {
                    error(&format!(
                        "Cannot get size comparison of {} and {}", l.get_type(), r.get_type()));
                    false
                }
            },

            _ => panic!("how did this happen")
        };

        if op == "!=" { return !rt; }
        return rt;

    }

    fn eval_array_call(&mut self, node: AstNode, index: AstNode) -> RuntimeValue {
        let var = self.eval_rt_value(node);
        if let RuntimeValue::Array {length, arr, ..} = var {
            let index = self.eval_rt_value(index);

            if let RuntimeValue::Int(i) = index {
                if i < 0 && i >= length as i32 { error(
                    &format!("Index out of bounds, length was {} but index was {}", length, i)); }

                return arr[i as usize].clone();
            }
            error("Array can only be indexed by type int");
        }
        error("Invalid array");
        return RuntimeValue::Void;
    }
    fn eval_identifier(&mut self, id: &str) -> RuntimeValue { 
        return self.env
            .lock()
            .unwrap()
            .get_var(id); 
    }
    fn eval_array_expr(
        &mut self,
        typ: DataType,
        size: Option<Box<AstNode>>,
        list: Vec<AstNode>
    ) -> RuntimeValue {
        if typ != DataType::Void {
            let size = self.eval_rt_value(*size.unwrap());

            let size_int = if let RuntimeValue::Int(v) = size { v } else {
                error("Array size exepcted int type");
                0
            };

            if size_int < 0 { error("Array size cannot be negative"); }

            return RuntimeValue::Array {
                data_type: typ,
                length: size_int as usize,
                arr: Vec::with_capacity(size_int as usize)
            };
        }

        let mut arr = vec![];
        let mut typ = DataType::Void;
        for node in list {
            let rt = self.eval_rt_value(node);
            if typ == DataType::Void { typ = rt.get_type(); }
            if rt.get_type() != typ { 
                error(&format!("Expected type {} but found {}", typ, rt.get_type())); }

            arr.push(rt);
        }

        return RuntimeValue::Array { data_type: typ, length: arr.len(), arr }
    }
    // Implemment creation of struct objects
    fn eval_object_expr(
        &mut self,
        type_id: Option<String>,
        properties: HashMap<String, AstNode>
    ) -> RuntimeValue {
        if type_id.is_none() {
            let name = String::from("Object");
            let mut props = HashMap::new();
            for (k, v) in properties {
                props.insert(k.to_string(), self.eval_rt_value(v.clone())); }

            return RuntimeValue::Object { name, properties: props };
        }

        let type_id = type_id.unwrap();
        let typ = self.env
            .lock()
            .unwrap()
            .get_var(&type_id);

        if let RuntimeValue::Struct { id, properties: props } = typ {
            let mut obj_props = HashMap::new();
            if props.len() != properties.len() { error(&format!(
                "{} expected {} properties but found {}", id, props.len(), properties.len())); }
            for (prop_id, data) in properties {
                let rt = self.eval_rt_value(data);

                if !props.contains(&(rt.get_type(), prop_id.clone())) { error(&format!(
                    "{} does not have property {} of type {}", id, prop_id, rt.get_type())); }

                obj_props.insert(prop_id, rt);
            }

            return RuntimeValue::Object {
                name: id,
                properties: obj_props
            }
        }

        error(&format!("Invalid object type {}", type_id));
        return RuntimeValue::Void;
    }
    fn eval_binary_expr(&mut self, node: AstNode) -> RuntimeValue {
        if let AstNode::BinaryExpr { left, right, op } = node {
            let lhs = self.eval_rt_value(*left);
            let rhs = self.eval_rt_value(*right);

            if lhs.get_type() != rhs.get_type() { 
                error(&format!("Cannot perform {} on type {} and {}", 
                               op, lhs.get_type(), rhs.get_type())); }

            match lhs.get_type() {
                DataType::Int => {
                    let l = unsafe { lhs.get_int_or_float().int };
                    let r = unsafe { rhs.get_int_or_float().int };

                    match &op[..] {
                        "+" => RuntimeValue::Int(l + r),
                        "-" => RuntimeValue::Int(l - r),
                        "*" => RuntimeValue::Int(l * r),
                        "/" => RuntimeValue::Int(l / r),
                        _ => { error("Invalid operator"); RuntimeValue::Void }
                    }
                },
                DataType::Float => {
                    let l = unsafe { lhs.get_int_or_float().float };
                    let r = unsafe { rhs.get_int_or_float().float };
                    
                    match &op[..] {
                        "+" => RuntimeValue::Float(l + r),
                        "-" => RuntimeValue::Float(l - r),
                        "*" => RuntimeValue::Float(l * r),
                        "/" => RuntimeValue::Float(l / r),
                        _ => { error("Invalid operator"); RuntimeValue::Void }
                    }
                },
                _ => {
                    error(&format!("Cannot perform {} on type {}", op, lhs.get_type()));
                    RuntimeValue::Void
                }
            }
        } else {
            error("Invalid binary expression");
            return RuntimeValue::Void;
        }
    }
    fn eval_struct_declaration(
        &mut self,
        id: &str,
        props: Vec<(AstNode, AstNode)>
    ) -> RuntimeValue {
        let mut s_props = vec![];
        for (t, prop_id) in props {
            let typ = self.eval_rt_value(t);
            match (typ, prop_id) {
                (RuntimeValue::TypeSpecifier(t), AstNode::Identifier(name)) => 
                    s_props.push((t, name)),
                (RuntimeValue::Struct {id,..}, AstNode::Identifier(name)) => 
                    s_props.push((DataType::Object(id), name)),
                    
                _ => error("Expected TYPE IDENTIFIER")
            }
        }

        let rt = RuntimeValue::Struct { 
            id: id.to_string(),
            properties: s_props,
        };

        self.env
            .lock()
            .unwrap()
            .declare_var(&format!("{}", id), &rt);
        return rt;
    }

}
