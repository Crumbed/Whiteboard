use std::{fmt::Display, collections::HashMap, sync::{Mutex, Arc}};

use crate::{error, parser::AstNode, interpreter::Program};






pub union IntOrFloat {
    pub int: i32,
    pub float: f64
}

#[allow(non_camel_case_types)]
type Type_Id = (DataType, String);

#[derive(Debug, Clone)]
pub enum RuntimeValue {
    Int(i32),
    Float(f64),
    Bool(bool), 
    Char(char), 
    Object { name: String, properties: HashMap<String, RuntimeValue> }, 
    Array { data_type: DataType, length: usize, arr: Vec<RuntimeValue> },

    Function { id: String, args: Vec<Type_Id>, rtrn_t: DataType, body: Vec<AstNode> }, 
    Struct { id: String, properties: Vec<Type_Id> },
    Void,
    TypeSpecifier(DataType),
    Return(Box<RuntimeValue>), 
    Break,
    Continue,
    Call { kind: ScopeKind, nodes: Vec<AstNode> },
    VarReferance(Arc<Mutex<Var>>),
}

#[derive(Debug, Clone)]
pub enum DataType {
    Int,
    Float,
    Bool,
    Char,
    Struct,
    Object(String),
    Array(Box<DataType>),
    Function(String),
    Ref(Box<DataType>),
    Void,
}

impl DataType {
    pub fn can_cast(&self, other: &DataType) -> bool {
        use DataType::*;

        match self {
            Int => match other {
                Int => true,
                Float => true,
                Char => true,
                _ => false,
            },
            Float => match other {
                Int => true,
                Float => true,
                _ => false,
            },
            Bool if other == &Bool => true,
            Char => match other {
                Int => true,
                Char => true,
                _ => false,
            },

            _ if self == other => true,
            _ => false
        }
    }
}
impl PartialEq for DataType {
    fn eq(&self, other: &Self) -> bool {
        use DataType::*;

        match (self, other) {
            (Int, Int) => true,
            (Float, Float) => true,
            (Bool, Bool) => true,
            (Char, Char) => true,
            (Object(t1), Object(t2)) if t1 == t2 => true,
            (Array(t1), Array(t2)) if t1 == t2 => true,
            (Function(t1), Function(t2)) if t1 == t2 => true,
            (Ref(t1), Ref(t2)) if t1 == t2 => true,
            (Void, Void) => true,
            _ => false,
        }
    }
}
impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use DataType::*;
        
        match self {
            Int => write!(f, "int"),
            Float => write!(f, "float"),
            Bool => write!(f, "bool"),
            Char => write!(f, "char"),
            Struct => write!(f, "struct"),
            Object(name) => write!(f, "{}", name),
            Array(t) => write!(f, "{}", *t),
            Function(name) => write!(f, "{}", name),
            Ref(t) => write!(f, "&{}", t),
            Void => write!(f, "void"),
        }
    }
}

impl RuntimeValue {
    pub fn get_type(&self) -> DataType {
        use RuntimeValue::*;
        
        match self {
            Int(_) => DataType::Int,
            Float(_) => DataType::Float,
            Bool(_) => DataType::Bool,
            Char(_) => DataType::Char,
            Object { name, ..} => DataType::Object(name.to_string()),
            Array { data_type, ..} => DataType::Array(Box::new(data_type.clone())),
            Function { id, ..} => DataType::Function(id.to_string()),
            Struct { id, ..} => DataType::Struct,
            Break | Continue | Void => DataType::Void,
            TypeSpecifier(data_type) => data_type.clone(),
            Return(node) => (*node).get_type(),
            Call {..} => DataType::Void,
            VarReferance(ptr) => {
                let var = ptr.lock().unwrap();
                var.d_type.clone()
            },
        }
    } 
    pub fn get_int_or_float(&self) -> IntOrFloat {
        match self {
            RuntimeValue::Int(v) => IntOrFloat { int: *v },
            RuntimeValue::Float(v) => IntOrFloat { float: *v },
            RuntimeValue::VarReferance(ptr) => {
                let val = &ptr
                    .lock()
                    .unwrap()
                    .value;

                return val.get_int_or_float();
            }
            _ => { error("Not int or float"); IntOrFloat{int:0} }
        }
    }
}

impl Display for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use RuntimeValue::*;

        match self {
            Int(i) => write!(f, "{}", i),
            Float(i) => write!(f, "{}", i),
            Bool(i) => write!(f, "{}", i),
            Char(i) => write!(f, "{}", i),
            Object { name, properties } => {
                let mut props = String::new();
                properties.keys()
                    .for_each(|k| {
                        let value = properties.get(k).unwrap();
                        props.push_str(&format!("    {}: {},\n", k, value));
                    });
                write!(f, "{name} {}\n{}{}", "{", props, "}")
            },
            Struct { id, properties } => {
                let mut props = String::new();
                properties.iter().for_each(|prop| {
                    props.push_str(&format!("    {} {},\n", prop.0, prop.1));
                });
                return write!(f, "{id} {}\n{}{}", "{", props, "}");
            },
            Array { arr, ..} => write!(f, "{:?}", arr),
            Break | Continue | Void => write!(f, "void"),
            Function { id, ..} => write!(f, "id: {id}"),
            TypeSpecifier(d_type) => write!(f, "{}", d_type),
            Return(value) => write!(f, "{}", *value),
            Call{..} => write!(f, "void"),
            VarReferance(ptr) => {
                let var = ptr.lock().unwrap();
                write!(f, "{}", var.value)
            },
        }
    }
}

impl PartialEq for RuntimeValue {
    fn eq(&self, other: &Self) -> bool {
        use RuntimeValue::*;

        match (self, other) {
            (Int(v1), Int(v2)) => v1 == v2,
            (Float(v1), Float(v2)) => v1 == v2,
            (Char(v1), Char(v2)) => v1 == v2,
            (Bool(v1), Bool(v2)) => v1 == v2,
            (Object{name: n1,..}, Object{name: n2,..}) => n1 == n2,
            (Array{arr: a1, ..}, Array {arr: a2, ..}) => a1 == a2, 
            (Function{id: n1, ..}, Function{id: n2, ..}) => n1 == n2,
            (Struct{id: n1, ..}, Struct{id: n2, ..}) => n1 == n2,
            (Void, Void) => true,
            (TypeSpecifier(t1), TypeSpecifier(t2)) => t1 == t2,
            (VarReferance(ptr1), VarReferance(ptr2)) => unsafe {
                let var1 = ptr1.lock().unwrap();
                let var2 = ptr2.lock().unwrap();
                return var1.value == var2.value;
            }
            _ => false,
        }
    }
}



#[derive(Debug, Clone)]
pub struct Var {
    pub id        :    String,
    pub d_type    :    DataType,
    pub value     :    RuntimeValue,
    pub constant  :    bool
}

#[derive(Debug, Clone)]
pub enum ScopeKind {
    Program,
    Module,
    Function { rtrn_t: DataType, args: Vec<Type_Id>, passed: Vec<AstNode> },
    IfBlock,
    Loop { condition: AstNode, body: Vec<AstNode> }
}
impl PartialEq for ScopeKind {
    fn eq(&self, other: &Self) -> bool {
        use ScopeKind::*;

        match (self, other) {
            (Program, Program) => true,
            (Module, Module) => true,
            (Function{..}, Function{..}) => true,
            (IfBlock, IfBlock) => true,
            (Loop{..}, Loop{..}) => true,
            _ => false,
        }
    }
}


#[derive(Debug)]
pub struct Env {
    pub kind    :    ScopeKind,
    pub vars    :    HashMap<String, Arc<Mutex<Var>>>,
    parent_p    :    Option<Arc<Mutex<Env>>> // optional, referance counted pointer, to a mutable Env
}
impl Env {
    pub fn new (parent_p: Option<Arc<Mutex<Env>>>, kind: ScopeKind) -> Self {
        return Env {
            kind,
            vars: HashMap::new(),
            parent_p
        };
    }
}


impl Env {
    pub fn declare_var(&mut self, var_id: &str, var_data: &RuntimeValue) {
        if self.vars.contains_key(var_id) { error(&format!(
            "Cannot declare {}, as it already exists", var_id )); }
        if ["int", "float", "bool", "char", "Object",].contains(&var_id) { error(&format!(
            "cannot declare {} as it is a reserved type specifier", var_id)); }

        let d_type = if let RuntimeValue::VarReferance(_) = var_data {
            DataType::Ref(Box::new(var_data.get_type()))
        } else {
            var_data.get_type()
        };

        let var = Var {
            id: var_id.to_string(),
            d_type,
            value: var_data.clone(),
            constant: false,
        };

        self.vars.insert(var_id.to_string(), Arc::new(Mutex::new(var)));
        //println!("{:#?}", self.vars);
    }
    pub fn assign_var(&mut self, var_id: &str, var_data: &RuntimeValue) {
        if self.vars.contains_key(var_id) {
            let mut var = self.vars
                .get(var_id)
                .unwrap()
                .lock()
                .unwrap();


            let var_type = if let DataType::Ref(typ) = &var.d_type { *typ.clone() }
            else { var.d_type.clone() };
            
            if var_type != var_data.get_type() { error(&format!(
                "Incompatible types {} and {}, {} expected type {}", 
                var_type, var_data.get_type(), var_id, var_type
            )); }

            if let RuntimeValue::VarReferance(ref ptr) = var.value {
                let mut var = ptr
                    .lock()
                    .unwrap();

                var.value = var_data.clone();
            } else {
                var.value = var_data.clone();
            }
            
            return;
        }
        if ["int", "float", "bool", "char", "Object"].contains(&var_id) { error(&format!(
            "cannot assign {} as it is a reserved type specifier", var_id)); }

        if self.parent_p.is_none() { error(&format!("Could not resolve {} as it does not exist", var_id));}
        let ptr = self.parent_p.as_ref().unwrap();

        let mut env = ptr
            .lock()
            .unwrap();
        
        env.assign_var(var_id, var_data);
    }
    pub fn get_var(&mut self, var_id: &str) -> RuntimeValue {
        if self.vars.contains_key(var_id) { 
            return self.vars
                .get(var_id)
                .unwrap()
                .lock()
                .unwrap().value
                .clone(); }

        match var_id {
            "int" => return RuntimeValue::TypeSpecifier(DataType::Int),
            "float" => return RuntimeValue::TypeSpecifier(DataType::Float),
            "char" => return RuntimeValue::TypeSpecifier(DataType::Char),
            "bool" => return RuntimeValue::TypeSpecifier(DataType::Bool),
            "Object" => return RuntimeValue::TypeSpecifier(DataType::Object("Object".to_string())),
            _ => (),
        }

        let mut par_p = self.parent_p.clone();
        while par_p.is_some() {
            let ptr = par_p.unwrap();
            let env = ptr
                .lock()
                .unwrap();

            if env.vars.contains_key(var_id) {
                return env.vars
                    .get(var_id)
                    .unwrap()
                    .lock()
                    .unwrap().value
                    .clone();
            }
            par_p = env.parent_p.clone();
        }

        error(&format!("Could not resolve {} as it does not exist", var_id));
        return RuntimeValue::Void;
    }
    pub fn contains_var(&self, var_id: &str) -> bool {
        if self.vars.contains_key(var_id) { return true; }
        if ["int", "float", "bool", "char", "Object"].contains(&var_id) { return true; }

        // changed from recursive calls to iterative because of stack overflow errors
        let mut par_p = self.parent_p.clone();
        while par_p.is_some() {
            let ptr = par_p.unwrap();
            let env = ptr
                .lock()
                .unwrap();

            if env.vars.contains_key(var_id) { return true; }
            par_p = env.parent_p.clone();
        }

        return false;
    }

    pub fn get_var_ref(&mut self, var_id: &str) -> Arc<Mutex<Var>> {
        if self.vars.contains_key(var_id) { return Arc::clone(&self.vars[var_id]); }

        if self.parent_p.is_none() { error(&format!("Could not resolve {} as it does not exist", var_id)); }

        let ptr = self.parent_p.as_ref().unwrap();
        let mut env = ptr
            .lock()
            .unwrap();

        return env.get_var_ref(var_id);
    }

    // returns the count of parent scopes it had to traverse to end, 0 = current scope, -1 = invalid end
    pub fn end_scope(&mut self, end_val: &RuntimeValue, base_count: i32) -> i32 {
        match &self.kind {
            ScopeKind::Function{rtrn_t,..} => match end_val {
                RuntimeValue::Return(data) if &data.get_type() == rtrn_t => base_count,
                RuntimeValue::Return(data) => {
                    error(&format!("function expected return type {} but found {}", 
                                   rtrn_t, data.get_type()));
                    -1
                },
                _ if self.parent_p.is_some() => {
                    let mut par = self.parent_p
                        .as_ref()
                        .unwrap()
                        .lock()
                        .unwrap();

                    par.end_scope(end_val, base_count + 1)
                },

                _ => -1
            },
            ScopeKind::Program => if let RuntimeValue::Return(data) = end_val { 
                if **data == RuntimeValue::Void { base_count } 
                else { -1 }
            } 
            else { -1 },
            ScopeKind::IfBlock => 
                if let RuntimeValue::Return(_) | RuntimeValue::Break = end_val {
                let mut par = self.parent_p
                    .as_ref()
                    .unwrap_or_else(|| panic!("Invalid return expression"))
                    .lock()
                    .unwrap();

                par.end_scope(end_val, base_count + 1)
            } else { -1 },
            ScopeKind::Loop {..} => match end_val {
                RuntimeValue::Return(_) => {
                    let mut par = self.parent_p
                        .as_ref()
                        .unwrap_or_else(|| panic!("Invalid return expression"))
                        .lock()
                        .unwrap();
    
                    par.end_scope(end_val, base_count + 1)
                },
                RuntimeValue::Break => base_count,
                _ => -1
            },
            _ => -1
        }
    }
}


pub fn cast(data: RuntimeValue, typ: DataType) -> RuntimeValue {
    use RuntimeValue::*;
    
    match (data.clone(), typ.clone()) {
        (Int(_), DataType::Int) => data,
        (Float(_), DataType::Float) => data,
        (Char(_), DataType::Char) => data,
        (Bool(_), DataType::Bool) => data,
        (Object{..}, DataType::Object(_)) if data.get_type() == typ => data,
        (Array{data_type,..}, DataType::Array(_)) if data_type == typ => data,

        (Int(i), DataType::Float) => Float(i as f64),
        (Int(i), DataType::Char) if i >= 0 => Char(i as u8 as char),

        (Float(f), DataType::Int) => Int(f as i32),
        (Char(c), DataType::Int) => Int(c as i32),

        _ => { error("Invalid cast"); return RuntimeValue::Void; }
    }
}

































