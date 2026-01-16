use std::fmt::{Display, Formatter, Result};

#[derive(Clone, Debug)]
pub enum Statement {
    Assertion(Clause),
    Query(Literal),
}

#[derive(Clone, Debug)]
pub struct Clause {
    pub head: Literal,
    pub body: Vec<Literal>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Literal {
    pub name: LiteralName,
    pub terms: Vec<Term>,
    pub negated: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum LiteralName {
    IdentOrString(IdentOrString),
    Variable(String, IdentOrString),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Term {
    Constant(Constant),
    Variable(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Constant {
    IdentOrString(IdentOrString),
    Integer(i64),
    Bool(bool),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum IdentOrString {
    Ident(String),
    String(String),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Statement::Assertion(c) => write!(f, "{c}."),
            Statement::Query(l) => write!(f, "{l}?"),
        }
    }
}

impl Display for Clause {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.head)?;
        if !self.body.is_empty() {
            write!(f, " :- ")?;
            for literal in self.body.iter().take(self.body.len() - 1) {
                write!(f, "{literal}, ")?;
            }
            write!(f, "{}", self.body[self.body.len() - 1])?;
        }
        Ok(())
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if self.negated {
            write!(f, "~")?;
        }
        write!(f, "{}", self.name)?;
        if !self.terms.is_empty() {
            write!(f, "(")?;
            for term in self.terms.iter().take(self.terms.len() - 1) {
                write!(f, "{term}, ")?;
            }
            write!(f, "{})", self.terms[self.terms.len() - 1])?;
        }
        Ok(())
    }
}

impl Display for LiteralName {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            LiteralName::IdentOrString(i) => write!(f, "{i}"),
            LiteralName::Variable(var, func) => write!(f, "{var} :- {func}"),
        }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Term::Constant(c) => write!(f, "{c}"),
            Term::Variable(v) => write!(f, "{v}"),
        }
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Constant::IdentOrString(i) => write!(f, "{i}"),
            Constant::Integer(i) => write!(f, "{i}"),
            Constant::Bool(b) => write!(f, "{b}"),
        }
    }
}

impl Display for IdentOrString {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            IdentOrString::Ident(i) => write!(f, "{i}"),
            IdentOrString::String(s) => write!(f, "\"{s}\""),
        }
    }
}
