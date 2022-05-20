# nimbleparse-lsp README

nimbleparse-lsp provides a lsp server for yacc style grammars.
This uses the grmtools versions of lex and yacc.

Instead of generating source code, this lsp builds parse tables in memory,
Ignoring any actions specified in the grammar.

Parsers are specified in a file `nimbleparse.toml` see the repository for details.

## Features

* Build parse tables from a yacc file.
* Parse sources from the generated parser based.
* Print a generic AST
* Railroad diagrams
* Pretty print the parse tables in various levels of detail.
* Diagnostics [wip]

## Extension Settings

## Known Issues


## Release Notes

### 0.0.1

Initial release
