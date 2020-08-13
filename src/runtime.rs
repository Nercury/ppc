use std::collections::HashMap;
use std::collections::hash_map::Entry;

pub struct Stack {
    named_values: HashMap<String, Vec<Varying>>,
}

impl Stack {
    pub fn new() -> Stack {
        Stack {
            named_values: HashMap::new(),
        }
    }

    pub fn push_named_value(&mut self, name: &str, value: Varying) {
        match self.named_values.entry(name.into()) {
            Entry::Occupied(mut stack) => { stack.get_mut().push(value); },
            Entry::Vacant(e) => { e.insert(vec![value]); },
        }
    }

    pub fn update_named_value(&mut self, name: &str, value: Varying) {
        let named_value = self.named_values.get_mut(name).expect(&format!("{} should have been pushed and hashmap entry should exist", name));
        *named_value.last_mut().expect(&format!("{} should have been pushed", name)) = value;
    }

    pub fn pop_named_value(&mut self, name: &str) {
        let named_value = self.named_values.get_mut(name).expect(&format!("{} should have been pushed and hashmap entry should exist", name));
        named_value.pop().expect(&format!("{} should have been pushed", name));
    }

    pub fn get_named(&self, name: &str) -> Option<&Varying> {
        self.named_values
            .get(name)
            .and_then(|vec| vec.last())
    }
}

#[derive(Copy, Clone)]
pub enum Varying {
    Integer(i64),
}

impl Varying {
    pub fn to_string(&self) -> String {
        match self {
            Varying::Integer(v) => format!("{}", v),
        }
    }
}