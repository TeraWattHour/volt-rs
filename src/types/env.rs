use std::cell::RefCell;
use std::collections::HashMap;
use crate::types::Type;

#[derive(Debug)]
pub struct TypeEnv<'a> {
    types: HashMap<String, Type>,
    functions: HashMap<String, Type>,

    returns: RefCell<Vec<Type>>,
    returns_directly: RefCell<bool>,

    isolated_returns: bool,
    parent: Option<&'a TypeEnv<'a>>,
}

impl<'a> TypeEnv<'a> {
    pub fn new() -> Self {
        TypeEnv {
            types: HashMap::new(),
            functions: HashMap::new(),
            returns: RefCell::new(Vec::new()),
            returns_directly: RefCell::new(false),
            isolated_returns: true,
            parent: None,
        }
    }

    pub fn child(&mut self) -> TypeEnv {
        TypeEnv {
            types: HashMap::new(),
            functions: HashMap::new(),
            returns: RefCell::new(Vec::new()),
            isolated_returns: false,
            returns_directly: RefCell::new(false),
            parent: Some(self),
        }
    }

     pub fn child_with_isolated_returns(&mut self) -> TypeEnv {
        TypeEnv {
            types: HashMap::new(),
            functions: HashMap::new(),
            returns: RefCell::new(Vec::new()),
            isolated_returns: true,
            returns_directly: RefCell::new(false),
            parent: Some(self),
        }
     }

    pub fn add_type(&mut self, name: &str, typ: Type) {
        self.types.insert(name.to_string(), typ);
    }

    pub fn add_function(&mut self, name: &str, typ: Type) {
        self.functions.insert(name.to_string(), typ);
    }

    pub fn add_return(&self, typ: Type) {
        self.returns.borrow_mut().push(typ.clone());
        *self.returns_directly.borrow_mut() = true;

        let mut base = self;

        if base.isolated_returns {
            return;
        }

        if let Some(ref p) = base.parent {
            p.add_return(typ.clone());
        }
    }

    pub fn compatible_returns(&self, with: Type) -> bool {
        let borrowed = self.returns.borrow();
        !borrowed.is_empty() && borrowed.iter().all(|x| x == &with)
    }

    pub fn get_type(&self, name: &str) -> Type {
        if let Some(typ) = self.types.get(name) {
            typ.clone()
        } else if let Some(ref parent) = self.parent {
            parent.get_type(name)
        } else {
            Type::Unknown
        }
    }

    fn isolated_returns(&self) -> bool {
        self.isolated_returns
    }

    pub fn returns_directly(&self) -> bool {
        *self.returns_directly.borrow()
    }
}
