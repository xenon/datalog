use crate::datalog::{Literal, Term};

use super::substitution::{predicate_name, Substitution};

/// Unify two terms, returning a substitution if successful
pub fn unify_terms(t1: &Term, t2: &Term) -> Option<Substitution> {
    match (t1, t2) {
        // Two constants: must be equal
        (Term::Constant(c1), Term::Constant(c2)) => {
            if c1 == c2 {
                Some(Substitution::empty())
            } else {
                None
            }
        }

        // Variable and constant: bind variable to constant
        (Term::Variable(v), Term::Constant(_)) => {
            Some(Substitution::singleton(v.clone(), t2.clone()))
        }
        (Term::Constant(_), Term::Variable(v)) => {
            Some(Substitution::singleton(v.clone(), t1.clone()))
        }

        // Two variables: bind one to the other
        (Term::Variable(v1), Term::Variable(v2)) => {
            if v1 == v2 {
                Some(Substitution::empty())
            } else {
                // Bind v1 to v2 (arbitrary choice)
                Some(Substitution::singleton(v1.clone(), t2.clone()))
            }
        }
    }
}

/// Unify two literals, returning a substitution if successful
/// Literals must have the same predicate name, arity, and negation status
pub fn unify_literals(l1: &Literal, l2: &Literal) -> Option<Substitution> {
    // Check predicate name match
    if predicate_name(&l1.name) != predicate_name(&l2.name) {
        return None;
    }

    // Check arity match
    if l1.terms.len() != l2.terms.len() {
        return None;
    }

    // Check negation match
    if l1.negated != l2.negated {
        return None;
    }

    // Unify all term pairs
    let mut subst = Substitution::empty();
    for (t1, t2) in l1.terms.iter().zip(l2.terms.iter()) {
        // Apply current substitution before unifying
        let t1_applied = subst.apply_term(t1);
        let t2_applied = subst.apply_term(t2);

        if let Some(new_subst) = unify_terms(&t1_applied, &t2_applied) {
            subst = subst.compose(&new_subst)?;
        } else {
            return None;
        }
    }

    Some(subst)
}

/// Match a literal against a ground fact, returning bindings for variables
/// The literal can contain variables; the fact must be ground
pub fn match_literal_to_fact(literal: &Literal, fact: &Literal) -> Option<Substitution> {
    // Negation must match
    if literal.negated != fact.negated {
        return None;
    }

    // Names must match
    if predicate_name(&literal.name) != predicate_name(&fact.name) {
        return None;
    }

    // Arity must match
    if literal.terms.len() != fact.terms.len() {
        return None;
    }

    let mut subst = Substitution::empty();
    for (lit_term, fact_term) in literal.terms.iter().zip(fact.terms.iter()) {
        match lit_term {
            // Literal has constant - must match fact's constant
            Term::Constant(c1) => {
                if let Term::Constant(c2) = fact_term {
                    if c1 != c2 {
                        return None;
                    }
                } else {
                    return None; // Fact should be ground
                }
            }
            // Literal has variable - bind to fact's constant
            Term::Variable(v) => {
                // Apply current subst to see if already bound
                let current_binding = subst.apply_term(lit_term);
                if let Term::Variable(_) = current_binding {
                    // Not yet bound, create binding
                    subst = subst.extend(v.clone(), fact_term.clone())?;
                } else {
                    // Already bound, check consistency
                    if current_binding != *fact_term {
                        return None;
                    }
                }
            }
        }
    }

    Some(subst)
}
