





use std::collections::HashMap;

use crate::parser::{
    AstNode, 
    TypeKind,
    ProgramAST
};

use crate::runtime::RuntimeValue;
use crate::runtime::cast;
use crate::runtime::DataType;
use crate::runtime::Env;

#[derive(Debug)]
pub struct Program {
    pub last_val    :   RuntimeValue,
    pub last_node   :   AstNode, 
    pub env         :   Env
}

impl Program {
    pub fn eval_ast(ast: ProgramAST) -> RuntimeValue {
        let mut env = Env::new(None, ScopeKind::Program);
        let mut prgm = Program {
            last_val: RuntimeValue::Void,
            last_node: AstNode::Void,
            env
        };
        
        for node in ast.nodes.clone() {
            let rt_val = prgm.eval_rt_value(node);
            prgm.last_val = rt_val;
        }

        return prgm.last_val.clone();
    }

    pub fn eval_rt_value(
        &mut self, 
        node: AstNode,
    ) -> RuntimeValue {
        use RuntimeValue::*;
        
        match node.clone() {
            // remember to add explicit typing here.
            AstNode::VarDeclaration(id) => {
                self.last_node = node;
                let value = RuntimeValue::Void;
                self.env.declare_var(&id, &RuntimeValue::Void); 
                return value;
            },

            AstNode::VarAssignment { id, data, declare } => {
                let typ_spec = self.eval_rt_value(self.last_node);
                let data = self.eval_rt_value(*data);
                if let TypeSpecifier(new_typ) = typ_spec {
                    let typ = data.get_type();
                    if typ != new_typ { match new_typ { 
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
                self.eval_struct_declaration(&id, &properties)
            },

            // LEFT OFF HERE
            AstNode::FunctionDeclaration { id, args, body } => {
                let body = if let AstNode::Block(arr) = *body { arr } else { panic!("HOW"); };
                let data = RuntimeValue::Function { 
                    id: id.clone(), 
                    args, 
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

            AstNode::UnaryExpr {..} => todo!(),

            AstNode::ConditionChain { left, right, op } => {
                let l = self.eval_rt_value(*left);
                let r = self.eval_rt_value(*right);
                let (l, r) = if let (RuntimeValue::Bool(b1), RuntimeValue::Bool(b2)) = (l, r) {
                    (b1, b2)
                } else { panic!("bad") };

                let val = match &op[..] {
                    "&&" => l && r,
                    "||" => l || r,
                    _ => panic!("wtf how.")
                };

                return RuntimeValue::Bool(val);
            },
            
            AstNode::Condition { left, right, op } => {
                let l = self.eval_rt_value(*left);
                let r = self.eval_rt_value(*right);

                use RuntimeValue::*;
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
                            Program::<T>::throw_error(&format!(
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
                            Program::<T>::throw_error(&format!(
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
                            Program::<T>::throw_error(&format!(
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
                            Program::<T>::throw_error(&format!(
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
                            Program::<T>::throw_error(&format!(
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
                            let v: f32 = match v.parse() {
                                Ok(value) => value,
                                Err(_) => return Self::throw_error("Number, {v}, is outside acceptable range.")
                            };
                            return RuntimeValue::Float(v);
                        }
                        let v: i32 = match v.parse() {
                            Ok(value) => value, 
                            Err(_) => return Self::throw_error("Number, {v}, is outside acceptable range.") 
                        };
                        return RuntimeValue::Int(v);
                    },
                    TypeKind::Char => {
                        self.last_node = node;
                        let v: char = match v.chars().next() {
                            Some(v) => v, 
                            None => return RuntimeValue::Void
                        };

                        return RuntimeValue::Char(v); 
                    },
                    TypeKind::Bool => {

                        self.last_node = node;
                        let v: bool = match v.parse() {
                            Ok(v) => v,
                            Err(_) => return Self::throw_error("{v}, is not a valid boolean value.") 
                        };

                        return RuntimeValue::Bool(v); 
                    }

                }
            },

            AstNode::ObjectLiteral(properties) => self.eval_object_expr(properties),

            AstNode::ArrayLiteral { type_id, size, list } => {
                let d_type: DataType;
                if !type_id.is_empty() {
                    d_type = match &type_id[..] {
                        "int" => DataType::Int, 
                        "float" => DataType::Float, 
                        "char" => DataType::Char, 
                        "bool" => DataType::Bool, 
                        "Object" => DataType::Object("Object".to_string()), 
                        _ => {
                            if !(self.env.contains_var(&type_id) && self.env.get_var(&type_id).get_type() == DataType::Struct) {
                                return Program::<T>::throw_error(&format!("{} is not a valid type", type_id));
                            }

                            DataType::Object(type_id.to_string())
                        },
                    }
                } else { d_type = DataType::Void; }

                return self.eval_array_expr(d_type, *size, list);
            }, 


            AstNode::Identifier(id) => { 
                self.last_node = node;
                return self.eval_identifier(&id);
            },
            AstNode::MemberCall(id) => self.eval_member_call(&id),
            AstNode::ArrayCall { id, index } => {
                if (*index).is_some() { return self.eval_array_call(&id, (*index).unwrap()); }

                self.last_node = node;
                return RuntimeValue::Type;
            },

            AstNode::IfChain { condition, body, else_do } => {
                let cond = self.eval_rt_value(*condition);
                let b = if let RuntimeValue::Bool(b) = cond { b } else { panic!("AGAIN.. HOW"); };

                if b {
                    return self.eval_block(*body);
                }

                if (*else_do).is_some() { return self.eval_rt_value((*else_do).unwrap()); }
                return RuntimeValue::Void;
            },
            
            AstNode::Block(_) => self.eval_block(node),
            AstNode::WriteExpr {..} => todo!(),
            AstNode::FunctionCall {..} => todo!(),
            AstNode::Return(data) => RuntimeValue::Return(Box::new(self.eval_rt_value(*data))),
        }
    }

    4

}
