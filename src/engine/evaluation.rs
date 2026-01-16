use std::collections::HashSet;

use crate::datalog::{Clause, Literal};

use super::{
    database::{Database, GroundAtom},
    stratification::StratifiedProgram,
    substitution::{is_ground_literal, predicate_name, Substitution},
    unification::match_literal_to_fact,
};

/// Result of evaluation
#[derive(Debug)]
pub struct EvaluationResult {
    pub facts_derived: usize,
    pub iterations: usize,
}

/// Bottom-up evaluator with semi-naive optimization
pub struct Evaluator<'a> {
    db: &'a mut Database,
}

impl<'a> Evaluator<'a> {
    pub fn new(db: &'a mut Database) -> Self {
        Evaluator { db }
    }

    /// Evaluate a stratified program
    pub fn evaluate(&mut self, program: &StratifiedProgram) -> EvaluationResult {
        let mut total_derived = 0;
        let mut total_iterations = 0;

        for stratum_rules in &program.strata {
            let result = self.evaluate_stratum(stratum_rules);
            total_derived += result.facts_derived;
            total_iterations += result.iterations;
        }

        EvaluationResult {
            facts_derived: total_derived,
            iterations: total_iterations,
        }
    }

    /// Evaluate a single stratum using semi-naive evaluation
    fn evaluate_stratum(&mut self, rules: &[Clause]) -> EvaluationResult {
        if rules.is_empty() {
            return EvaluationResult {
                facts_derived: 0,
                iterations: 0,
            };
        }

        let mut iterations = 0;
        let mut total_derived = 0;

        // Delta: facts derived in the previous iteration
        // Initially, all current facts are "new"
        let mut delta: HashSet<GroundAtom> = self.db.all_facts().cloned().collect();

        loop {
            iterations += 1;
            let mut new_facts: HashSet<GroundAtom> = HashSet::new();

            // For each rule, try to derive new facts
            for rule in rules {
                // Semi-naive: at least one body literal must match a delta fact
                let derived = self.apply_rule_semi_naive(rule, &delta);

                for atom in derived {
                    if !self.db.contains(&atom) && !new_facts.contains(&atom) {
                        new_facts.insert(atom);
                    }
                }
            }

            if new_facts.is_empty() {
                break; // Fixpoint reached
            }

            // Add new facts to database
            for atom in &new_facts {
                self.db.add_fact(atom.clone());
            }

            total_derived += new_facts.len();
            delta = new_facts;
        }

        EvaluationResult {
            facts_derived: total_derived,
            iterations,
        }
    }

    /// Apply a rule using semi-naive optimization
    fn apply_rule_semi_naive(&self, rule: &Clause, delta: &HashSet<GroundAtom>) -> Vec<GroundAtom> {
        let mut results = Vec::new();

        if rule.body.is_empty() {
            // This shouldn't happen for rules, but handle it
            if let Some(atom) = GroundAtom::from_literal(&rule.head) {
                results.push(atom);
            }
            return results;
        }

        // Try each body literal as the "delta" literal
        for delta_idx in 0..rule.body.len() {
            let delta_lit = &rule.body[delta_idx];

            // Skip negated literals as delta - they don't produce bindings
            if delta_lit.negated {
                continue;
            }

            let delta_pred = predicate_name(&delta_lit.name);

            // For each delta fact matching this literal
            for delta_atom in delta {
                if delta_atom.predicate != delta_pred {
                    continue;
                }

                let delta_fact = delta_atom.to_literal();
                if let Some(subst) = match_literal_to_fact(delta_lit, &delta_fact) {
                    // Try to satisfy remaining body literals
                    let derived = self.complete_rule_body(rule, delta_idx, subst);
                    results.extend(derived);
                }
            }
        }

        results
    }

    /// Complete a rule body given partial substitution and which literal was matched
    fn complete_rule_body(
        &self,
        rule: &Clause,
        matched_idx: usize,
        initial_subst: Substitution,
    ) -> Vec<GroundAtom> {
        // Build list of body literals to satisfy (excluding already matched one)
        let remaining: Vec<(usize, &Literal)> = rule
            .body
            .iter()
            .enumerate()
            .filter(|(i, _)| *i != matched_idx)
            .collect();

        // Recursively satisfy remaining literals
        let substitutions = self.satisfy_literals(&remaining, initial_subst);

        // Generate head facts from successful substitutions
        let mut results = Vec::new();
        for subst in substitutions {
            let grounded_head = subst.apply_literal(&rule.head);
            if is_ground_literal(&grounded_head) {
                if let Some(atom) = GroundAtom::from_literal(&grounded_head) {
                    results.push(atom);
                }
            }
        }

        results
    }

    /// Satisfy a list of literals, returning all valid substitutions
    fn satisfy_literals(
        &self,
        literals: &[(usize, &Literal)],
        subst: Substitution,
    ) -> Vec<Substitution> {
        if literals.is_empty() {
            return vec![subst];
        }

        let (_, lit) = literals[0];
        let rest = &literals[1..];

        // Apply current substitution to the literal
        let grounded_lit = subst.apply_literal(lit);

        if lit.negated {
            // Negation-as-failure: check that the positive literal does NOT hold
            if is_ground_literal(&grounded_lit) {
                // Create positive version to check
                let positive_lit = Literal {
                    negated: false,
                    ..grounded_lit
                };
                if !self.db.literal_holds(&positive_lit) {
                    // Negation succeeds (positive form doesn't hold) - continue with rest
                    self.satisfy_literals(rest, subst)
                } else {
                    // Negation fails (positive form holds)
                    vec![]
                }
            } else {
                // Non-ground negated literal - this is a safety violation
                // For now, we skip it (could also error)
                vec![]
            }
        } else {
            // Positive literal: find all matching facts
            let pred = predicate_name(&grounded_lit.name);
            let mut results = Vec::new();

            for atom in self.db.get_facts(&pred) {
                let fact = atom.to_literal();
                if let Some(new_subst) = match_literal_to_fact(&grounded_lit, &fact) {
                    if let Some(combined) = subst.compose(&new_subst) {
                        let sub_results = self.satisfy_literals(rest, combined);
                        results.extend(sub_results);
                    }
                }
            }

            results
        }
    }
}

/// Convenience function to run full evaluation
pub fn evaluate_program(
    db: &mut Database,
) -> Result<EvaluationResult, super::stratification::StratificationError> {
    let rules = db.rules().to_vec();
    let program = StratifiedProgram::from_rules(rules)?;
    let mut evaluator = Evaluator::new(db);
    Ok(evaluator.evaluate(&program))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::datalog::Statement;
    use crate::parser::peg_parser;

    fn parse_and_evaluate(input: &str) -> Database {
        let stmts = peg_parser::program(input).expect("parse failed");
        let mut db = Database::new();

        for stmt in &stmts {
            if let Statement::Assertion(clause) = stmt {
                db.add_clause(clause);
            }
        }

        evaluate_program(&mut db).expect("evaluation failed");
        db
    }

    #[test]
    fn test_simple_facts() {
        let db = parse_and_evaluate("parent(tom, bob). parent(bob, jim).");
        assert_eq!(db.fact_count(), 2);
    }

    #[test]
    fn test_simple_rule() {
        let input = r#"
            edge(a, b).
            edge(b, c).
            path(X, Y) :- edge(X, Y).
        "#;

        let db = parse_and_evaluate(input);
        // 2 edge facts + 2 path facts derived
        assert_eq!(db.fact_count(), 4);
    }

    #[test]
    fn test_transitive_closure() {
        let input = r#"
            edge(a, b).
            edge(b, c).
            edge(c, d).
            path(X, Y) :- edge(X, Y).
            path(X, Z) :- edge(X, Y), path(Y, Z).
        "#;

        let db = parse_and_evaluate(input);
        // edge: 3 facts
        // path: path(a,b), path(b,c), path(c,d), path(a,c), path(a,d), path(b,d) = 6 facts
        // Total: 9
        assert_eq!(db.fact_count(), 9);
    }

    #[test]
    fn test_ancestor() {
        let input = r#"
            parent(tom, bob).
            parent(bob, jim).
            parent(jim, ann).
            ancestor(X, Y) :- parent(X, Y).
            ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
        "#;

        let db = parse_and_evaluate(input);
        // parent: 3
        // ancestor: (tom,bob), (bob,jim), (jim,ann), (tom,jim), (tom,ann), (bob,ann) = 6
        // Total: 9
        assert_eq!(db.fact_count(), 9);
    }

    #[test]
    fn test_negation_simple() {
        let input = r#"
            bird(tweety).
            bird(opus).
            penguin(opus).
            flies(X) :- bird(X), ~penguin(X).
        "#;

        let db = parse_and_evaluate(input);
        // bird: 2, penguin: 1, flies: 1 (only tweety)
        assert_eq!(db.fact_count(), 4);

        // Check that flies(tweety) exists but not flies(opus)
        use crate::datalog::{Constant, IdentOrString, Literal, LiteralName, Term};
        use crate::engine::QueryEngine;

        let query_engine = QueryEngine::new(&db);

        let flies_tweety = Literal {
            name: LiteralName::IdentOrString(IdentOrString::Ident("flies".into())),
            terms: vec![Term::Constant(Constant::IdentOrString(
                IdentOrString::Ident("tweety".into()),
            ))],
            negated: false,
        };

        let flies_opus = Literal {
            name: LiteralName::IdentOrString(IdentOrString::Ident("flies".into())),
            terms: vec![Term::Constant(Constant::IdentOrString(
                IdentOrString::Ident("opus".into()),
            ))],
            negated: false,
        };

        assert!(query_engine.query(&flies_tweety).success);
        assert!(!query_engine.query(&flies_opus).success);
    }

    #[test]
    fn test_cyclic_graph_terminates() {
        // Even with a cycle in the data, evaluation should terminate
        let input = r#"
            edge(a, b).
            edge(b, c).
            edge(c, a).
            reachable(X, Y) :- edge(X, Y).
            reachable(X, Z) :- reachable(X, Y), edge(Y, Z).
        "#;

        let db = parse_and_evaluate(input);
        // Should terminate and derive all reachable pairs
        // edge: 3
        // reachable: 9 (all pairs in a cycle of 3)
        assert_eq!(db.fact_count(), 12);
    }

    #[test]
    fn test_multiple_body_literals() {
        let input = r#"
            person(alice).
            person(bob).
            person(charlie).
            likes(alice, bob).
            likes(bob, charlie).
            mutual_friends(X, Z) :- person(X), person(Z), likes(X, Y), likes(Y, Z).
        "#;

        let db = parse_and_evaluate(input);
        // person: 3, likes: 2, mutual_friends: 1 (alice-charlie via bob)
        assert_eq!(db.fact_count(), 6);
    }

    #[test]
    fn test_integers_and_booleans() {
        let input = r#"
            age(bob, 42).
            age(alice, 35).
            has_age(X) :- age(X, Y).
        "#;

        let db = parse_and_evaluate(input);
        // age: 2, has_age: 2
        assert_eq!(db.fact_count(), 4);
    }

    #[test]
    fn test_empty_program() {
        let db = parse_and_evaluate("");
        assert_eq!(db.fact_count(), 0);
    }

    #[test]
    fn test_facts_only() {
        let input = "fact1. fact2. fact3(a, b).";
        let db = parse_and_evaluate(input);
        assert_eq!(db.fact_count(), 3);
    }
}
