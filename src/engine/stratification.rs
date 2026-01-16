use std::collections::{HashMap, HashSet};

use crate::datalog::Clause;

use super::substitution::predicate_name;

/// Dependency type between predicates
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DependencyType {
    Positive,
    Negative,
}

/// Dependency graph for stratification analysis
#[derive(Debug, Default)]
pub struct DependencyGraph {
    /// Map from predicate to its dependencies: (dependency_pred, dep_type)
    edges: HashMap<String, Vec<(String, DependencyType)>>,
    predicates: HashSet<String>,
}

impl DependencyGraph {
    pub fn new() -> Self {
        DependencyGraph::default()
    }

    /// Build dependency graph from rules
    pub fn from_rules(rules: &[Clause]) -> Self {
        let mut graph = DependencyGraph::new();

        for clause in rules {
            let head_pred = predicate_name(&clause.head.name);
            graph.predicates.insert(head_pred.clone());

            for body_lit in &clause.body {
                let body_pred = predicate_name(&body_lit.name);
                graph.predicates.insert(body_pred.clone());

                let dep_type = if body_lit.negated {
                    DependencyType::Negative
                } else {
                    DependencyType::Positive
                };

                graph
                    .edges
                    .entry(head_pred.clone())
                    .or_default()
                    .push((body_pred, dep_type));
            }
        }

        graph
    }

    /// Compute stratification - returns ordered strata or None if unstratifiable
    /// Each stratum is a set of predicates that can be evaluated together
    pub fn compute_strata(&self) -> Option<Vec<HashSet<String>>> {
        // Compute strongly connected components
        let sccs = self.compute_sccs();

        // Check for negative cycles within any SCC
        for scc in &sccs {
            if self.has_negative_edge_within(scc) {
                return None; // Unstratifiable: negative cycle
            }
        }

        // Topologically sort SCCs based on dependencies
        self.topological_sort_sccs(&sccs)
    }

    /// Check if there's a negative edge within a set of predicates
    fn has_negative_edge_within(&self, preds: &HashSet<String>) -> bool {
        for pred in preds {
            if let Some(deps) = self.edges.get(pred) {
                for (dep, dtype) in deps {
                    if *dtype == DependencyType::Negative && preds.contains(dep) {
                        return true;
                    }
                }
            }
        }
        false
    }

    /// Compute strongly connected components using Tarjan's algorithm
    fn compute_sccs(&self) -> Vec<HashSet<String>> {
        struct TarjanState {
            index: usize,
            indices: HashMap<String, usize>,
            lowlinks: HashMap<String, usize>,
            on_stack: HashSet<String>,
            stack: Vec<String>,
            sccs: Vec<HashSet<String>>,
        }

        fn strongconnect(v: &str, graph: &DependencyGraph, state: &mut TarjanState) {
            state.indices.insert(v.to_string(), state.index);
            state.lowlinks.insert(v.to_string(), state.index);
            state.index += 1;
            state.stack.push(v.to_string());
            state.on_stack.insert(v.to_string());

            if let Some(deps) = graph.edges.get(v) {
                for (w, _) in deps {
                    if !state.indices.contains_key(w) {
                        strongconnect(w, graph, state);
                        let low_w = *state.lowlinks.get(w).unwrap();
                        let low_v = state.lowlinks.get_mut(v).unwrap();
                        *low_v = (*low_v).min(low_w);
                    } else if state.on_stack.contains(w) {
                        let idx_w = *state.indices.get(w).unwrap();
                        let low_v = state.lowlinks.get_mut(v).unwrap();
                        *low_v = (*low_v).min(idx_w);
                    }
                }
            }

            if state.lowlinks.get(v) == state.indices.get(v) {
                let mut scc = HashSet::new();
                loop {
                    let w = state.stack.pop().unwrap();
                    state.on_stack.remove(&w);
                    scc.insert(w.clone());
                    if w == v {
                        break;
                    }
                }
                state.sccs.push(scc);
            }
        }

        let mut state = TarjanState {
            index: 0,
            indices: HashMap::new(),
            lowlinks: HashMap::new(),
            on_stack: HashSet::new(),
            stack: Vec::new(),
            sccs: Vec::new(),
        };

        for pred in &self.predicates {
            if !state.indices.contains_key(pred) {
                strongconnect(pred, self, &mut state);
            }
        }

        state.sccs
    }

    /// Topologically sort SCCs
    fn topological_sort_sccs(&self, sccs: &[HashSet<String>]) -> Option<Vec<HashSet<String>>> {
        // Build SCC dependency graph
        let scc_index: HashMap<&str, usize> = sccs
            .iter()
            .enumerate()
            .flat_map(|(i, scc)| scc.iter().map(move |p| (p.as_str(), i)))
            .collect();

        let mut scc_deps: HashMap<usize, HashSet<usize>> = HashMap::new();

        for (from_pred, deps) in &self.edges {
            if let Some(&from_scc) = scc_index.get(from_pred.as_str()) {
                for (to_pred, _) in deps {
                    if let Some(&to_scc) = scc_index.get(to_pred.as_str()) {
                        if from_scc != to_scc {
                            scc_deps.entry(from_scc).or_default().insert(to_scc);
                        }
                    }
                }
            }
        }

        // Kahn's algorithm for topological sort
        let n = sccs.len();
        let mut in_degree: Vec<usize> = vec![0; n];

        for deps in scc_deps.values() {
            for &dep in deps {
                in_degree[dep] += 1;
            }
        }

        let mut queue: Vec<usize> = in_degree
            .iter()
            .enumerate()
            .filter(|(_, &d)| d == 0)
            .map(|(i, _)| i)
            .collect();

        let mut result = Vec::new();

        while let Some(scc_idx) = queue.pop() {
            result.push(sccs[scc_idx].clone());

            if let Some(deps) = scc_deps.get(&scc_idx) {
                for &dep_idx in deps {
                    in_degree[dep_idx] -= 1;
                    if in_degree[dep_idx] == 0 {
                        queue.push(dep_idx);
                    }
                }
            }
        }

        // Reverse because we want dependencies first
        result.reverse();

        if result.len() == n {
            Some(result)
        } else {
            None // Cycle detected
        }
    }
}

/// A stratified program ready for evaluation
#[derive(Debug)]
pub struct StratifiedProgram {
    /// Rules grouped by stratum
    pub strata: Vec<Vec<Clause>>,
}

impl StratifiedProgram {
    /// Create a stratified program from rules
    pub fn from_rules(rules: Vec<Clause>) -> Result<Self, StratificationError> {
        if rules.is_empty() {
            return Ok(StratifiedProgram { strata: vec![] });
        }

        let graph = DependencyGraph::from_rules(&rules);

        let strata_preds = graph
            .compute_strata()
            .ok_or(StratificationError::NegativeCycle)?;

        // Group rules by stratum
        let mut strata: Vec<Vec<Clause>> = vec![Vec::new(); strata_preds.len()];

        // Create predicate to stratum index mapping
        let pred_stratum: HashMap<String, usize> = strata_preds
            .iter()
            .enumerate()
            .flat_map(|(i, preds)| preds.iter().map(move |p| (p.clone(), i)))
            .collect();

        for rule in rules {
            let head_pred = predicate_name(&rule.head.name);
            if let Some(&stratum) = pred_stratum.get(&head_pred) {
                strata[stratum].push(rule);
            }
        }

        Ok(StratifiedProgram { strata })
    }
}

#[derive(Debug, Clone)]
pub enum StratificationError {
    NegativeCycle,
}

impl std::fmt::Display for StratificationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StratificationError::NegativeCycle => {
                write!(f, "Program is not stratifiable: contains negative cycle")
            }
        }
    }
}

impl std::error::Error for StratificationError {}
