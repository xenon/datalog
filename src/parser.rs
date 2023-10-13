use crate::datalog::{Clause, Constant, IdentOrString, Literal, LiteralName, Statement, Term};

/* BNF Grammar for Datalog described on Racket website:
  https://docs.racket-lang.org/datalog/datalog.html
*/

peg::parser! {
    pub grammar peg_parser() for str {
        pub rule program() -> Vec<Statement>
            = _ stmts:(s:statement() _ { s })* { stmts }

        rule statement() -> Statement
            = s:(single_assertion() / single_query()) { s }

        rule single_assertion() -> Statement
            = c:clause() _ "." { Statement::Assertion(c) }

        rule single_query() -> Statement
            = q:literal() _ "?" { Statement::Query(q) }

        rule clause() -> Clause
            = head:literal() _ clause_symbol() _ body:(literal() ** ("," _)) { Clause { head, body } }
            / head:literal() { Clause {head, body: vec![] } }

        rule literal() -> Literal
            = name:ident_or_string() "(" terms:(term() **  ("," _)) ")" { Literal { name: LiteralName::IdentOrString(name), terms } }
            / name:ident_or_string() { Literal { name : LiteralName::IdentOrString(name), terms: vec![] } }
            / var:variable()  _ clause_symbol() _ f:ident_or_string() "(" terms:(term() ** ("," _)) ")" { Literal { name: LiteralName::Variable(var, f), terms } }

        rule term() -> Term
            = v:variable() { Term::Variable(v) }
            / c:constant() { Term::Constant(c) }

        rule constant() -> Constant
            = i:integer() { Constant::Integer(i) }
            / b:boolean() { Constant::Bool(b) }
            / i:ident_or_string() { Constant::IdentOrString(i) }

        rule variable() -> String
            = v:$(['A'..='Z'] ['A'..='Z' | 'a'..='z' | '0'..='9' | '_']*) { v.to_string() }

        rule ident_or_string() -> IdentOrString
            = i:identifier() { IdentOrString::Ident(i) }
            / s:string() { IdentOrString::String(s) }

        rule identifier() -> String
            = s:$(!(clause_symbol())[^ 'A'..='Z' | '(' |  ')' | '`' | '\'' | '.' | '~' | '?' | '\"' | '%' | ' ' | ','][^ '(' | ')' | '`' | '\'' | '.' | '~' | '?' | '\"' | '%' | ' ' | ',']*) { s.to_string() }

        rule string() -> String
            = s:(['\"'] v:$([^ '\"']*) ['\"'] { v }) { s.to_string() }

        rule integer() -> i64
            = "0" { 0 }
            / n:$(v:['1'..='9']['0'..='9']*) {? n.parse().or(Err("i64")) }
            / "-" n:$(v:['1'..='9']['0'..='9']*) {? n.parse().and_then(|n: i64| Ok(-n)).or(Err("i64")) }

        rule boolean() -> bool
            = "true" { true }
            / "false" { false }

        rule clause_symbol()
            = ":-"
            / "<-"

        rule _()
            = quiet!{(comment() / inline_comment() / [' ' | '\n' | '\t' | '\r'])*}

        rule comment()
            = ("%" [^ '\n']* "\n")

        rule inline_comment()
            = ("/*" (!("*/")[_])* "*/")
    }
}
