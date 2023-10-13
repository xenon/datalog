## Datalog Parser
- Uses ``peg`` crate to parse
- Does not actually implement the language
- Could be used as a basis for a Datalog implementation


## Using the parser
- ``datalog <DATALOG_SOURCE_FILE>``
- Give a filename argument containing a path to a datalog file.
- If it parsed correctly, it will reprint the AST.
- Otherwise it will report an error occured and where.
