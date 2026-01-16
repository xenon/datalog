use std::collections::{HashMap, HashSet};

use crate::datalog::{Clause, Constant, IdentOrString, Literal, LiteralName, Statement, Term};

use super::substitution::predicate_name;

/// A ground fact represented for efficient storage
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GroundAtom {
    pub predicate: String,
    pub terms: Vec<Constant>,
}

impl GroundAtom {
    /// Convert from a ground Literal
    pub fn from_literal(lit: &Literal) -> Option<Self> {
        if lit.negated {
            return None; // Don't store negated facts
        }

        let terms: Option<Vec<_>> = lit
            .terms
            .iter()
            .map(|t| match t {
                Term::Constant(c) => Some(c.clone()),
                Term::Variable(_) => None,
            })
            .collect();

        terms.map(|terms| GroundAtom {
            predicate: predicate_name(&lit.name),
            terms,
        })
    }

    /// Convert back to a Literal
    pub fn to_literal(&self) -> Literal {
        Literal {
            name: LiteralName::IdentOrString(IdentOrString::Ident(self.predicate.clone())),
            terms: self
                .terms
                .iter()
                .map(|c| Term::Constant(c.clone()))
                .collect(),
            negated: false,
        }
    }
}

/// The fact database, indexed by predicate name
#[derive(Clone, Debug, Default)]
pub struct Database {
    /// Facts indexed by predicate name
    facts: HashMap<String, HashSet<GroundAtom>>,

    /// Rules (clauses with non-empty bodies)
    rules: Vec<Clause>,
}

impl Database {
    pub fn new() -> Self {
        Database::default()
    }

    /// Load statements from parsed program
    pub fn load_program(&mut self, statements: &[Statement]) {
        for stmt in statements {
            if let Statement::Assertion(clause) = stmt {
                self.add_clause(clause);
            }
        }
    }

    /// Add a clause (fact or rule)
    pub fn add_clause(&mut self, clause: &Clause) {
        if clause.body.is_empty() {
            // Fact: add directly
            self.add_fact_from_literal(&clause.head);
        } else {
            // Rule: store for later evaluation
            self.rules.push(clause.clone());
        }
    }

    /// Add a ground fact
    pub fn add_fact(&mut self, atom: GroundAtom) -> bool {
        let pred = atom.predicate.clone();
        self.facts.entry(pred).or_default().insert(atom)
    }

    /// Add fact from a ground literal
    pub fn add_fact_from_literal(&mut self, lit: &Literal) -> bool {
        if let Some(atom) = GroundAtom::from_literal(lit) {
            self.add_fact(atom)
        } else {
            false
        }
    }

    /// Check if a ground atom exists in the database
    pub fn contains(&self, atom: &GroundAtom) -> bool {
        self.facts
            .get(&atom.predicate)
            .map(|set| set.contains(atom))
            .unwrap_or(false)
    }

    /// Check if a ground literal holds (considering negation)
    pub fn literal_holds(&self, lit: &Literal) -> bool {
        if let Some(atom) = GroundAtom::from_literal(&Literal {
            negated: false,
            ..lit.clone()
        }) {
            let positive_holds = self.contains(&atom);
            if lit.negated {
                !positive_holds // Negation-as-failure
            } else {
                positive_holds
            }
        } else {
            false
        }
    }

    /// Get all facts for a predicate
    pub fn get_facts(&self, predicate: &str) -> impl Iterator<Item = &GroundAtom> {
        self.facts
            .get(predicate)
            .into_iter()
            .flat_map(|set| set.iter())
    }

    /// Get all facts
    pub fn all_facts(&self) -> impl Iterator<Item = &GroundAtom> {
        self.facts.values().flat_map(|set| set.iter())
    }

    /// Get the rules
    pub fn rules(&self) -> &[Clause] {
        &self.rules
    }

    /// Get number of facts
    pub fn fact_count(&self) -> usize {
        self.facts.values().map(|s| s.len()).sum()
    }
}
