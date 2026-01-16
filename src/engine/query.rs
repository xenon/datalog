use crate::datalog::Literal;

use super::{
    database::Database,
    substitution::{is_ground_literal, literal_variables, predicate_name, Substitution},
    unification::match_literal_to_fact,
};

/// Result of a query
#[derive(Debug)]
pub struct QueryResult {
    /// Variable bindings for each answer
    pub answers: Vec<Substitution>,
    /// Whether the query succeeded (has at least one answer)
    pub success: bool,
}

impl QueryResult {
    pub fn empty() -> Self {
        QueryResult {
            answers: Vec::new(),
            success: false,
        }
    }

    pub fn with_answers(answers: Vec<Substitution>) -> Self {
        let success = !answers.is_empty();
        QueryResult { answers, success }
    }
}

/// Query executor
pub struct QueryEngine<'a> {
    db: &'a Database,
}

impl<'a> QueryEngine<'a> {
    pub fn new(db: &'a Database) -> Self {
        QueryEngine { db }
    }

    /// Execute a query, returning all matching substitutions
    pub fn query(&self, literal: &Literal) -> QueryResult {
        if literal.negated {
            // Negated query: check if the positive form has no matches
            let positive = Literal {
                negated: false,
                ..literal.clone()
            };

            if is_ground_literal(&positive) {
                // Ground negated query
                if self.db.literal_holds(literal) {
                    QueryResult::with_answers(vec![Substitution::empty()])
                } else {
                    QueryResult::empty()
                }
            } else {
                // Non-ground negated query - check if positive has any answers
                let positive_result = self.query(&positive);
                if positive_result.success {
                    QueryResult::empty()
                } else {
                    QueryResult::with_answers(vec![Substitution::empty()])
                }
            }
        } else {
            // Positive query
            self.query_positive(literal)
        }
    }

    /// Query for positive literal
    fn query_positive(&self, literal: &Literal) -> QueryResult {
        let pred = predicate_name(&literal.name);
        let mut answers = Vec::new();

        for atom in self.db.get_facts(&pred) {
            let fact = atom.to_literal();
            if let Some(subst) = match_literal_to_fact(literal, &fact) {
                answers.push(subst);
            }
        }

        QueryResult::with_answers(answers)
    }
}

/// Format query results for display
pub fn format_query_result(query: &Literal, result: &QueryResult) -> String {
    if !result.success {
        return "No.".to_string();
    }

    let vars = literal_variables(query);

    if vars.is_empty() {
        // Ground query - just yes/no
        return "Yes.".to_string();
    }

    let mut output = String::new();

    for (i, subst) in result.answers.iter().enumerate() {
        if i > 0 {
            output.push('\n');
        }

        let bindings: Vec<String> = vars
            .iter()
            .filter_map(|v| subst.get(v).map(|t| format!("{} = {}", v, t)))
            .collect();

        output.push_str(&bindings.join(", "));
    }

    if output.is_empty() {
        "Yes.".to_string()
    } else {
        output
    }
}
