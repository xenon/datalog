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
            = head:literal() _ clause_symbol() _ body:(literal() ** (_ "," _)) { Clause { head, body } }
            / head:literal() { Clause {head, body: vec![] } }

        rule literal() -> Literal
            = neg:negation()? name:ident_or_string() _ "(" _ terms:(term() ** (_ "," _)) _ ")" { Literal { name: LiteralName::IdentOrString(name), terms, negated: neg.is_some() } }
            / neg:negation()? name:ident_or_string() { Literal { name : LiteralName::IdentOrString(name), terms: vec![], negated: neg.is_some() } }
            / neg:negation()? var:variable() _ clause_symbol() _ f:ident_or_string() _ "(" _ terms:(term() ** (_ "," _)) _ ")" { Literal { name: LiteralName::Variable(var, f), terms, negated: neg.is_some() } }

        rule negation() -> ()
            = "~" _ { () }

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
            = ("%" [^ '\n']* ("\n" / ![_]))

        rule inline_comment()
            = ("/*" (!("*/")[_])* "*/")
    }
}

#[cfg(test)]
mod tests {
    use super::peg_parser;
    use crate::datalog::*;

    // Helper to check if parsing succeeds
    fn parses(input: &str) -> bool {
        peg_parser::program(input).is_ok()
    }

    // Helper to get parsed statements
    fn parse(input: &str) -> Vec<Statement> {
        peg_parser::program(input).expect("parse failed")
    }

    // ==================== SPACING TESTS ====================
    // These tests verify the spacing fixes

    #[test]
    fn test_space_before_open_paren() {
        // Space between predicate name and opening parenthesis
        assert!(parses("foo (X)."));
        assert!(parses("foo  (X, Y)."));
    }

    #[test]
    fn test_space_after_open_paren() {
        // Space after opening parenthesis
        assert!(parses("foo( X)."));
        assert!(parses("foo(  X, Y)."));
    }

    #[test]
    fn test_space_before_close_paren() {
        // Space before closing parenthesis
        assert!(parses("foo(X )."));
        assert!(parses("foo(X, Y  )."));
    }

    #[test]
    fn test_space_before_comma() {
        // Space before comma in argument list
        assert!(parses("foo(X , Y)."));
        assert!(parses("foo(X  ,Y)."));
    }

    #[test]
    fn test_spaces_around_comma() {
        // Spaces on both sides of comma
        assert!(parses("foo(X , Y)."));
        assert!(parses("foo(X  ,  Y)."));
    }

    #[test]
    fn test_all_spacing_combinations() {
        // Various spacing combinations
        assert!(parses("foo ( X , Y )."));
        assert!(parses("foo(X,Y)."));
        assert!(parses("foo( X , Y )."));
        assert!(parses("predicate ( Arg1 , Arg2 , Arg3 )."));
    }

    #[test]
    fn test_spacing_in_clause_body() {
        // Spacing in rule bodies
        assert!(parses("head(X) :- body1(X) , body2(X)."));
        assert!(parses("head(X) :- body1( X ) , body2( Y )."));
        assert!(parses("head (X) :- body1 (X), body2 (X)."));
    }

    #[test]
    fn test_spacing_around_clause_symbol() {
        assert!(parses("head(X):-body(X)."));
        assert!(parses("head(X) :- body(X)."));
        assert!(parses("head(X)  :-  body(X)."));
        assert!(parses("head(X)<-body(X)."));
        assert!(parses("head(X) <- body(X)."));
    }

    // ==================== NEGATION TESTS ====================

    #[test]
    fn test_negation_in_body() {
        assert!(parses("safe(X) :- person(X), ~dangerous(X)."));
        let stmts = parse("safe(X) :- person(X), ~dangerous(X).");
        if let Statement::Assertion(clause) = &stmts[0] {
            assert!(!clause.body[0].negated);
            assert!(clause.body[1].negated);
        } else {
            panic!("Expected assertion");
        }
    }

    #[test]
    fn test_negation_with_space() {
        assert!(parses("result(X) :- ~ negated(X)."));
    }

    #[test]
    fn test_negation_in_query() {
        assert!(parses("~foo(X)?"));
    }

    #[test]
    fn test_negation_no_args() {
        assert!(parses("result :- ~fact."));
    }

    // ==================== COMMENT TESTS ====================

    #[test]
    fn test_line_comment_with_newline() {
        assert!(parses("foo(X). % this is a comment\n"));
    }

    #[test]
    fn test_line_comment_at_eof() {
        // Comment at end of file without trailing newline
        assert!(parses("foo(X). % comment at eof"));
    }

    #[test]
    fn test_inline_comment() {
        assert!(parses("foo(X). /* inline comment */ bar(Y)."));
    }

    #[test]
    fn test_multiline_comment() {
        assert!(parses("foo(X). /* multi\nline\ncomment */ bar(Y)."));
    }

    // ==================== BASIC PARSING TESTS ====================

    #[test]
    fn test_simple_fact() {
        assert!(parses("parent(tom, bob)."));
    }

    #[test]
    fn test_simple_rule() {
        assert!(parses("grandparent(X, Z) :- parent(X, Y), parent(Y, Z)."));
    }

    #[test]
    fn test_simple_query() {
        assert!(parses("parent(tom, X)?"));
    }

    #[test]
    fn test_fact_without_args() {
        assert!(parses("sunny."));
    }

    #[test]
    fn test_integer_constants() {
        assert!(parses("age(bob, 42)."));
        assert!(parses("temperature(-10)."));
        assert!(parses("zero(0)."));
    }

    #[test]
    fn test_boolean_constants() {
        assert!(parses("flag(true)."));
        assert!(parses("flag(false)."));
    }

    #[test]
    fn test_string_constants() {
        assert!(parses("name(bob, \"Robert\")."));
        assert!(parses("message(\"hello world\")."));
    }

    #[test]
    fn test_multiple_statements() {
        let input = r#"
            parent(tom, bob).
            parent(bob, jim).
            grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
            grandparent(tom, X)?
        "#;
        let stmts = parse(input);
        assert_eq!(stmts.len(), 4);
    }

    #[test]
    fn test_alternative_clause_symbol() {
        assert!(parses("head(X) <- body(X)."));
    }

    #[test]
    fn test_variable_literal_form() {
        // Variable :- predicate form
        assert!(parses("X :- member(1, 2, 3)."));
    }

    // ==================== EDGE CASES ====================

    #[test]
    fn test_underscore_in_variable() {
        assert!(parses("foo(X_var, Y_1)."));
    }

    #[test]
    fn test_underscore_identifier() {
        assert!(parses("_private(X)."));
    }

    #[test]
    fn test_numbers_in_identifier() {
        assert!(parses("rule1(X)."));
        assert!(parses("test123(Y)."));
    }

    #[test]
    fn test_empty_program() {
        assert!(parses(""));
        assert!(parses("   "));
        assert!(parses("\n\n"));
    }

    #[test]
    fn test_whitespace_only_between_statements() {
        assert!(parses("foo(X).\n\n\nbar(Y)."));
        assert!(parses("foo(X).\t\tbar(Y)."));
    }
}
