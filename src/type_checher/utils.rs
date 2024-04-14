use std::fmt::Debug;

use super::*;

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub struct ActivationRecord<T>(HashMap<Identifier, T>)
    where T: Clone + PartialEq + Eq + Debug;

impl<T> ActivationRecord<T>
    where T: Clone + PartialEq + Eq + Debug
{
    pub fn new() -> Self {
        Self::default()
    }

    pub fn new_identifier(&mut self, id: &Identifier, value: T) {
        self.0.insert(id.clone(), value);
    }

    pub fn search_identifier(&self, id: &Identifier) -> Option<T> {
        self.0.get(&id).cloned()
    }

    pub fn pop_identifier(&mut self, id: &Identifier) -> Option<T> {
        self.0.remove(&id)
    }
}

impl<T> Default for ActivationRecord<T>
    where T: Clone + PartialEq + Eq + Debug
{
    fn default() -> Self {
        Self(Default::default())
    }
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub struct EnvBlock<T>
    where T: Clone + PartialEq + Eq + Debug
{
    stack: Vec<ActivationRecord<T>>,
}

impl<T> EnvBlock<T>
    where T: Clone + PartialEq + Eq + Debug
{
    pub fn new(record: ActivationRecord<T>) -> Self {
        Self { stack: vec![record] }
    }

    pub fn new_identifier(&mut self, id: &Identifier, value: T) {
        self.stack.last_mut().unwrap().new_identifier(id, value);
    }

    pub fn search_identifier(&self, id: &Identifier) -> Option<T> {
        self.stack.iter().rev().fold(None, |acc, act_record| {
            acc.or_else(|| act_record.search_identifier(id))
        })
    }

    pub fn pop_identifier(&mut self, id: &Identifier) -> Option<T> {
        self.stack.iter_mut().rev().fold(None, |acc, act_record| {
            acc.or_else(|| act_record.pop_identifier(id))
        })
    }

    pub fn attach(&mut self, record: ActivationRecord<T>) {
        self.stack.push(record);
    }

    pub fn push(&mut self) {
        self.stack.push(ActivationRecord::default());
    }

    pub fn pop(&mut self) {
        self.stack.pop();
    }

    pub fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }

    pub fn get_stack_size(&self) -> usize {
        self.stack.len()
    }
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq, Eq)]
pub struct Environment<T>
    where T: Clone + PartialEq + Eq + Debug
{
    handlers: Vec<EnvBlock<T>>
}

impl<T> Environment<T>
    where T: Clone + PartialEq + Eq + Debug
{
    pub fn new() -> Self {
        Self::default()
    }

    pub fn reset(&mut self) {
        self.handlers.clear();
    }

    pub fn new_identifier(&mut self, id: &Identifier, value: T) {
        self.handlers.last_mut().unwrap().new_identifier(id, value);
    }
    
    pub fn new_identifier_str(&mut self, id: &str, value: T) {
        self.handlers.last_mut().unwrap().new_identifier(&id.into(), value);
    }

    pub fn search_identifier(&self, id: &Identifier) -> Option<T> {
        self.handlers.iter().rev()
            .fold(None, |acc, envblock| {
                acc.or_else(|| envblock.search_identifier(id))
            })
    }

    pub fn pop_identifier(&mut self, id: &Identifier) -> Option<T> {
        self.handlers.iter_mut().rev()
            .fold(None, |acc, envblock| {
                acc.or_else(|| envblock.pop_identifier(id))
            })
    }

    pub fn attach_block(&mut self, record: ActivationRecord<T>) {
        self.handlers.last_mut().unwrap().attach(record);
    }

    pub fn push_block(&mut self) {
        self.handlers.last_mut().unwrap().push();
    }

    pub fn pop_block(&mut self) {
        self.handlers.last_mut().unwrap().pop();
    }

    pub fn push_handler(&mut self, record: ActivationRecord<T>) {
        self.handlers.push(EnvBlock::new(record))
    }

    pub fn pop_handler(&mut self) -> EnvBlock<T> {
        self.handlers.pop().unwrap()
    }

    pub fn restore_environment(&mut self, previous_environment: Vec<EnvBlock<T>>) {
        self.handlers.extend(previous_environment.into_iter().rev())
    }
}

impl<T> Default for Environment<T>
    where T: Clone + PartialEq + Eq + Debug
{
    fn default() -> Self {
        Self { handlers: Default::default() }
    }
}