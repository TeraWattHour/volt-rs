use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::types::Type;

pub struct Context {
    functions: Rc<RefCell<HashMap<String, Type>>>,
    variables: Rc<RefCell<HashMap<String, (bool, Type)>>>,
    temporaries: RefCell<usize>
}

impl Context {
    pub fn global() -> Self {
        Self {
            functions: Rc::new(RefCell::new(HashMap::new())),
            variables: Rc::new(RefCell::new(HashMap::new())),
            temporaries: RefCell::new(0),
        }
    }

    pub fn inherit_separate(from: &Context) -> Self {
        Self {
            functions: from.functions.clone(),
            variables: from.variables.clone(),
            temporaries: RefCell::new(0)
        }
    }

    pub fn temporary_count(&self) -> usize {
        *self.temporaries.borrow()
    }

    pub fn add_temporary(&self) -> usize {
        let mut count = self.temporaries.borrow_mut();
        *count += 1;
        *count
    }

    pub fn add_function(&self, name: String, typ: Type) {
        self.functions.borrow_mut().insert(name, typ);
    }

    pub fn get_function(&self, name: &str) -> Option<Type> {
        self.functions.borrow().get(name).cloned()
    }

    pub fn add_variable(&self, name: String, is_const: bool, typ: Type) {
        self.variables.borrow_mut().insert(name, (is_const, typ));
    }

    pub fn get(&self, name: &str) -> Option<Type> {
        self.variables.borrow().get(name).cloned().map(|(_, t)| t).or_else(|| self.functions.borrow().get(name).cloned())
    }
}