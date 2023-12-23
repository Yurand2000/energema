use super::*;

impl Declarations {
    pub fn new(defs: Vec<Declaration>, native_fns: Vec<(Identifier, NativeFun)>) -> Self {
        let mut declarations = Self {
            functions: HashMap::new(),
            handlers: HashMap::new(),
            effects: HashMap::new(),
            native_functions: HashMap::new(),
        };

        defs.into_iter().for_each(|definition| {
            match definition {
                Declaration::Function(def) => { declarations.functions.insert(def.name.clone(), def.into()); },
                Declaration::Handler(def) => { declarations.handlers.insert(def.name.clone(), def.into()); },
                Declaration::Effect(def) => { declarations.effects.insert(def.name.clone(), def); },
            }
        });

        for (id, native_fn) in native_fns.into_iter() {
            declarations.native_functions.insert(id, native_fn);
        }

        declarations
    }
}