





use std::collections::{HashMap, LinkedList};
use std::env;
use std::slice::Iter;

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
    pub nodes       :   LinkedList<AstNode>,
    pub env_stack   :   Vec<Env>,
    pub env         :   Env
}

pub static mut VAR_LITERALS: Option<HashMap<String, u64>> = None; 

impl Program {
    pub fn new(env: Env) -> Self {
        Program { 
            last_val: RuntimeValue::Void,
            last_node: AstNode::Void,
            nodes: LinkedList::new(),
            env_stack: vec![],
            env, 
        }
    }
    pub fn eval_ast(&mut self, ast: ProgramAST) -> RuntimeValue {
        self.nodes = ast.nodes
            .iter()
            .cloned()
            .collect();
        let mut node_counts = vec![0];
        let mut at = self.nodes.pop_front();
        while at.is_some() {
            let node = at.unwrap();
            let rt_val = self.eval_rt_value(node.clone());
            let index: usize;
            
            if let RuntimeValue::Call { kind, nodes } = rt_val.clone() {
                match kind.clone() {
                    ScopeKind::Program => todo!(),
                    ScopeKind::Module => todo!(),
                    ScopeKind::Function{args, passed,..} => {
                        let mut env = Env::new(Some(&mut self.env_stack[0]), kind);
                        for (i, node) in passed.clone().iter().enumerate() {
                            if let AstNode::Identifier(id) = node {
                                let ptr = self.env.get_var_ptr(id);
                                unsafe {
                                    let var_p = ptr as *const Var as *mut Var;
                                    let var = &mut *var_p;
                                    if var.d_type != args[i].0 { error(&format!(
                                        "function expected type {} but found {}",args[i].0,var.d_type));}
                                    
                                    VAR_LITERALS
                                        .unwrap()
                                        .insert(id.to_string(), ptr);
                                }
                                continue;
                            }

                            let rt = self.eval_rt_value(node.clone());
                            if rt.get_type() != args[i].0 { error(&format!(
                                "function expected type {} but found {}", args[i].0, rt.get_type()));}

                            env.declare_var(&args[i].1, &rt);
                        }
                        self.env_stack.push(self.env.clone());
                        index = self.env_stack.len()-1;
                        self.env = env;
                    },
                    ScopeKind::IfBlock => {
                        self.env_stack.push(self.env.clone());
                        index = self.env_stack.len() - 1;
                        self.env = Env::new(Some(&mut self.env_stack[index]), kind)
                    },
                }
                
                node_counts.push(0);
                for new_node in nodes.iter().rev() {
                    self.nodes.push_front(new_node.to_owned());
                    node_counts[index] += 1;
                }
            }
            
            at = self.nodes.pop_front();
            self.last_val = rt_val;
            if self.env_stack.len() > 0 {
                node_counts[self.env_stack.len()-1] -= 1; 
                if node_counts[self.env_stack.len()-1] == 0 {
                    self.env = self.env_stack.pop().unwrap();
                    node_counts.pop();
                }
            }
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
                self.env.declare_var(&id, &RuntimeValue::Void); 
                return value;
            },

            AstNode::VarAssignment { id, data, declare } => {
                let typ_spec = self.eval_rt_value(self.last_node.clone());
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

                if declare {
                    self.env.declare_var(&id, &data);
                } else {
                    self.env.assign_var(&id, &data); 
                }
                
                return data;
            }, 
            AstNode::AdvanceAssignment { id, data } => todo!(),

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
                    let t = if let TypeSpecifier(_) | Struct {..} = typ {
                        typ.get_type()
                    } else { error(&format!("Invalid type {:?}", typ)); DataType::Void };

                    let id = if let AstNode::Identifier(name) = id { name } else {
                        error(&format!("Expected identifier but found {:?}", id));
                        String::new()
                    };

                    new_args.push((t, id));
                }

                let rtrn_t = if rtrn_typ.is_some() {
                    let rtrn_typ = self.eval_rt_value(*rtrn_typ.unwrap());
                    if let TypeSpecifier(_) | Struct {..} = rtrn_typ {
                        rtrn_typ.get_type()
                    } else { error(&format!("Invalid type {:?}", rtrn_typ.get_type())); DataType::Void }
                } else { DataType::Void };

                let data = RuntimeValue::Function { 
                    id: id.clone(), 
                    args: new_args, 
                    rtrn_t,
                    body
                };

                self.last_node = node;
                self.env.declare_var(&format!("fn:{id}"), &data);
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

                let rt = match &op[..] {
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

                        _ => {
                            error(&format!(
                            "Cannot get size comparison of {} and {}", l.get_type(), r.get_type()));
                            false
                        }
                    },

                    _ => panic!("how did this happen")
                };

                if op == "!=" { return Bool(!rt); }
                return Bool(rt);
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
                            if !self.env.contains_var(&format!("s:{}", type_id)) {
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
                let parent = self.eval_rt_value(*parent);

                if let Object { name, properties } = parent {
                    match *member {
                        AstNode::Identifier(m_name) => {
                            if !properties.contains_key(&m_name) {
                                error(&format!("{} does not have member {}", name, m_name)); }

                            return properties.get(&m_name).unwrap().clone();
                        },
                        AstNode::ArrayCall{id, index} => todo!(),
                        AstNode::FunctionCall{id, params} => todo!(),
                        _ => { error("Invalid member call"); return Void; }
                    }
                } else {
                    error("Only objects can have members");
                    return Void;
                }
            }
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
                        if !self.env.contains_var(&format!("s:{}", id)) {
                            error(&format!("Invalid type {}", id)); }

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
                    if let AstNode::Block(nodes) = *else_do.unwrap() {
                        return Call { kind: ScopeKind::IfBlock, nodes };
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
                let func = self.env.get_var(&format!("fn:{}", id));

                let (args, rtrn_t, body) = if let Function {args, rtrn_t, body,..} = func {
                    (args, rtrn_t, body) } else {
                    error("FUNC wasnt a function, this shouldnt happen");
                    (vec![], DataType::Void, vec![]) };

                if params.len() != args.len() { error(&format!(
                    "{} takes in {} arguments but {} were given", id, args.len(), params.len())); }

                return Call { kind: ScopeKind::Function {rtrn_t, args, passed: params}, nodes: body };
            },
            AstNode::Return(data) => RuntimeValue::Return(Box::new(self.eval_rt_value(*data))),
            
            AstNode::WriteExpr {..} => todo!(),
            AstNode::Block(_) => { error("Block should be contained"); Void },
        }
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
    fn eval_identifier(&mut self, id: &str) -> RuntimeValue { return self.env.get_var(id); }
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
        let name = String::from("Object");
        if type_id.is_some() { todo!(); }

        let mut props = HashMap::new();
        for (k, v) in properties {
            props.insert(k.to_string(), self.eval_rt_value(v.clone())); }

        return RuntimeValue::Object { name, properties: props };
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
                (RuntimeValue::TypeSpecifier(T), AstNode::Identifier(name)) => 
                    s_props.push((T, name)),
                _ => error("Expected TYPE IDENTIFIER")
            }
        }

        let rt = RuntimeValue::Struct { 
            id: id.to_string(),
            properties: s_props,
        };

        self.env.declare_var(&format!("s:{}", id), &rt);
        return rt;
    }

}
