pub mod database;
pub mod evaluation;
pub mod query;
pub mod stratification;
pub mod substitution;
pub mod unification;

// Re-export commonly used types
pub use database::{Database, GroundAtom};
pub use evaluation::{evaluate_program, EvaluationResult, Evaluator};
pub use query::{format_query_result, QueryEngine, QueryResult};
pub use stratification::{StratificationError, StratifiedProgram};
pub use substitution::Substitution;
