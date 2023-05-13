use std::{fmt::Display, collections::HashMap};

use crate::{error, parser::AstNode, interpreter::{Program, VAR_LITERALS}};






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
    Call { kind: ScopeKind, nodes: Vec<AstNode> },
    VarReferance(u64),
}

#[derive(Debug, Clone)]
pub enum DataType {
    Int,
    Float,
    Bool,
    Char,
    Object(String),
    Array(Box<DataType>),
    Function(String),
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
            Object(name) => write!(f, "{}", name),
            Array(t) => write!(f, "{}", *t),
            Function(name) => write!(f, "{}", name),
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
            Struct { id, ..} => DataType::Object(id.to_string()),
            Void => DataType::Void,
            TypeSpecifier(data_type) => data_type.clone(),
            Return(node) => (*node).get_type(),
            Call {..} => DataType::Void,
            VarReferance(ptr) => unsafe {
                let var_p = *ptr as *const Var;
                let var = &*var_p;
                var.d_type.clone()
            },
        }
    } 
    pub fn get_int_or_float(&self) -> IntOrFloat {
        match self {
            RuntimeValue::Int(v) => IntOrFloat { int: *v },
            RuntimeValue::Float(v) => IntOrFloat { float: *v },
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
            Void => write!(f, "void"),
            Function { id, ..} => write!(f, "id: {id}"),
            TypeSpecifier(d_type) => write!(f, "{}", d_type),
            Return(value) => write!(f, "{}", *value),
            Call{..} => write!(f, "void"),
            VarReferance(ptr) => unsafe {
                let var_p = *ptr as *const Var;
                let var = &*var_p;
                write!(f, "{}", var.value)
            }
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
                let var1_p = *ptr1 as *const Var;
                let var2_p = *ptr2 as *const Var;
                let var1 = &*var1_p;
                let var2 = &*var2_p;
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
}
impl PartialEq for ScopeKind {
    fn eq(&self, other: &Self) -> bool {
        use ScopeKind::*;

        match (self, other) {
            (Program, Program) => true,
            (Module, Module) => true,
            (Function{..}, Function{..}) => true,
            (IfBlock, IfBlock) => true,
            _ => false,
        }
    }
}


#[derive(Debug, Clone)]
pub struct Env {
    kind    :    ScopeKind,
    vars    :    HashMap<String, Var>,
    parent_p:    u64
}
impl Env {
    pub fn new (parent: Option<&mut Env>, kind: ScopeKind) -> Self {
        let parent_p = match parent {
            Some(env) => env as *const Env as u64,
            None => 0 as u64
        };
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
        if ["int", "float", "bool", "char", "Object"].contains(&var_id) { error(&format!(
            "cannot declare {} as it is a reserved type specifier", var_id)); }

        let var = Var {
            id: var_id.to_string(),
            d_type: var_data.get_type(),
            value: var_data.clone(),
            constant: false,
        };

        self.vars.insert(var_id.to_string(), var);
    }
    pub fn assign_var(&mut self, var_id: &str, var_data: &RuntimeValue) {
        if self.vars.contains_key(var_id) {
            let mut var = self.vars.get(var_id).unwrap().clone();

            if var.d_type != var_data.get_type() { error(&format!(
                "Incompatible types {} and {}, {} expected type {}", 
                var.d_type, var_data.get_type(), var_id, var.d_type
            )); }
            var.value = var_data.clone();
            self.vars.insert(var_id.to_string(), var);

            return;
        }
        if let ScopeKind::Function {..} = self.kind { unsafe {
            if VAR_LITERALS.unwrap().contains_key(var_id) {
                let ptr = VAR_LITERALS
                    .unwrap()
                    .get(var_id)
                    .unwrap().clone();
                let var_p = ptr as *mut Var;
                let var = &mut *var_p;

                if var.d_type != var_data.get_type() { error(&format!(
                    "Incompatible types {} and {}, {} expected type {}",
                    var.d_type, var_data.get_type(), var_id, var.d_type
                )); }
                var.value = var_data.clone();
                return;
            }
        }}
        if ["int", "float", "bool", "char", "Object"].contains(&var_id) { error(&format!(
            "cannot assign {} as it is a reserved type specifier", var_id)); }

        if self.parent_p == 0 { error(&format!("Could not resolve {} as it does not exist", var_id));}
        unsafe {
            let env_p = self.parent_p as *const Env as *mut Env;
            let env = &mut *env_p;

            env.assign_var(var_id, var_data);
        }
    }
    pub fn get_var(&mut self, var_id: &str) -> RuntimeValue {
        if self.vars.contains_key(var_id) { return self.vars.get(var_id).unwrap().value.clone(); }
        if let ScopeKind::Function {..} = self.kind { unsafe {
            if VAR_LITERALS.unwrap().contains_key(var_id) {
                let ptr = *VAR_LITERALS
                    .unwrap()
                    .get(var_id)
                    .unwrap();
                let var_p = ptr as *const Var;
                let var = &*var_p;
                return var.value.clone();
            }
        }}
            

        match var_id {
            "int" => return RuntimeValue::TypeSpecifier(DataType::Int),
            "float" => return RuntimeValue::TypeSpecifier(DataType::Float),
            "char" => return RuntimeValue::TypeSpecifier(DataType::Char),
            "bool" => return RuntimeValue::TypeSpecifier(DataType::Bool),
            "Object" => return RuntimeValue::TypeSpecifier(DataType::Object("Object".to_string())),
            _ => (),
        }

        if self.parent_p == 0 { error(&format!("Could not resolve {} as it does not exist", var_id));}
        unsafe {
            let env_p = self.parent_p as *const Env as *mut Env;
            let env = &mut *env_p;

            return env.get_var(var_id).clone();
        }
    }
    pub fn contains_var(&self, var_id: &str) -> bool {
        if self.vars.contains_key(var_id) { return true; }
        if ["int", "float", "bool", "char", "Object"].contains(&var_id) { return true; }

        // changed from recursive calls to iterative because of stack overflow errors
        let mut parent_p = self.parent_p;
        unsafe {
            while parent_p != 0 {
                let env_p = self.parent_p as *const Env as *mut Env;
                let env = &mut *env_p;

                if env.vars.contains_key(var_id) { return true; }
                if ["int", "float", "bool", "char", "Object"].contains(&var_id) { return true; }
                parent_p = env.parent_p;
            }

            return false;
        }
    }

    pub fn get_var_ptr(&mut self, var_id: &str) -> u64 {
        if self.vars.contains_key(var_id) { 
            return self.vars.get(var_id).unwrap() as *const Var as u64;  }

        if self.parent_p == 0 { error(&format!("Could not resolve {} as it does not exist", var_id)); }
        unsafe {
            let env_p = self.parent_p as *mut Env;
            let env = &mut *env_p;

            return env.get_var_ptr(var_id);
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

































