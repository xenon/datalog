use std::collections::HashMap;

use crate::datalog::{Constant, IdentOrString, Literal, LiteralName, Term};

/// A substitution maps variable names to Terms
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Substitution {
    bindings: HashMap<String, Term>,
}

impl Substitution {
    /// Create an empty substitution
    pub fn empty() -> Self {
        Substitution {
            bindings: HashMap::new(),
        }
    }

    /// Create a substitution from a single binding
    pub fn singleton(var: String, term: Term) -> Self {
        let mut bindings = HashMap::new();
        bindings.insert(var, term);
        Substitution { bindings }
    }

    /// Check if the substitution contains a variable
    pub fn contains(&self, var: &str) -> bool {
        self.bindings.contains_key(var)
    }

    /// Get the binding for a variable
    pub fn get(&self, var: &str) -> Option<&Term> {
        self.bindings.get(var)
    }

    /// Extend the substitution with a new binding
    /// Returns None if the variable is already bound to a different term
    pub fn extend(&self, var: String, term: Term) -> Option<Substitution> {
        if let Some(existing) = self.bindings.get(&var) {
            // Variable already bound - check if it's the same term
            if existing == &term {
                Some(self.clone())
            } else {
                None // Conflict
            }
        } else {
            let mut new_sub = self.clone();
            new_sub.bindings.insert(var, term);
            Some(new_sub)
        }
    }

    /// Apply substitution to a term
    pub fn apply_term(&self, term: &Term) -> Term {
        match term {
            Term::Variable(v) => self
                .bindings
                .get(v)
                .cloned()
                .unwrap_or_else(|| term.clone()),
            Term::Constant(_) => term.clone(),
        }
    }

    /// Apply substitution to a literal
    pub fn apply_literal(&self, lit: &Literal) -> Literal {
        Literal {
            name: self.apply_literal_name(&lit.name),
            terms: lit.terms.iter().map(|t| self.apply_term(t)).collect(),
            negated: lit.negated,
        }
    }

    /// Apply substitution to a literal name (handles Variable variant)
    fn apply_literal_name(&self, name: &LiteralName) -> LiteralName {
        match name {
            LiteralName::IdentOrString(_) => name.clone(),
            LiteralName::Variable(var, _func) => {
                // If the variable is bound to a constant, use that as the name
                if let Some(Term::Constant(Constant::IdentOrString(ios))) = self.bindings.get(var) {
                    LiteralName::IdentOrString(ios.clone())
                } else {
                    name.clone()
                }
            }
        }
    }

    /// Compose two substitutions: apply other after self
    /// Returns None if there's a conflict
    pub fn compose(&self, other: &Substitution) -> Option<Substitution> {
        let mut result = self.clone();

        // First, apply other to all bindings in self
        for term in result.bindings.values_mut() {
            *term = other.apply_term(term);
        }

        // Then, add bindings from other that aren't in self
        for (var, term) in &other.bindings {
            if let Some(existing) = result.bindings.get(var) {
                if existing != term {
                    return None; // Conflict
                }
            } else {
                result.bindings.insert(var.clone(), term.clone());
            }
        }

        Some(result)
    }

    /// Get all bindings as an iterator
    pub fn iter(&self) -> impl Iterator<Item = (&String, &Term)> {
        self.bindings.iter()
    }
}

/// Check if a term is ground (contains no variables)
pub fn is_ground_term(term: &Term) -> bool {
    matches!(term, Term::Constant(_))
}

/// Check if a literal is ground (all terms are constants)
pub fn is_ground_literal(lit: &Literal) -> bool {
    lit.terms.iter().all(is_ground_term)
}

/// Extract all variables from a literal
pub fn literal_variables(lit: &Literal) -> Vec<String> {
    let mut vars = Vec::new();
    for term in &lit.terms {
        if let Term::Variable(v) = term {
            if !vars.contains(v) {
                vars.push(v.clone());
            }
        }
    }
    // Also check literal name if it's a variable form
    if let LiteralName::Variable(v, _) = &lit.name {
        if !vars.contains(v) {
            vars.push(v.clone());
        }
    }
    vars
}

/// Get the predicate identifier from a LiteralName
pub fn predicate_name(name: &LiteralName) -> String {
    match name {
        LiteralName::IdentOrString(ios) => ident_or_string_to_string(ios),
        LiteralName::Variable(_, ios) => ident_or_string_to_string(ios),
    }
}

/// Convert IdentOrString to String
pub fn ident_or_string_to_string(ios: &IdentOrString) -> String {
    match ios {
        IdentOrString::Ident(s) => s.clone(),
        IdentOrString::String(s) => s.clone(),
    }
}
