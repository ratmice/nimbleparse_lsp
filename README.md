## Purpose

Primarily intended for the development of grammars.
It seeks to remove any/all extraneous steps between editing a grammar testing it.

It avoids generating source code, compiling the sources, subsequently it will not run actions embedded in the grammar. It will optionally build a generic syntax tree.

Reparsing is performed on the editors buffer, on change. So you do not need to save the changes to reparse. Parser tables are built in memory, so you can both edit the grammar and its input.

Error recovery is optional (enabled by default), but will be slower than disabling it.

## Features

* Parse test input on lex/yacc file change.
* Parse test input on test input change.
* Actions to show the parser state table of a parser.
* Actions to show the generic ast for an input file.

## Editor support

VSCode will automatically discover and register for the test input file extension
Other editors require hard coding of the test input file extension.


| Editor  | supported | caveats |
| ------- | ----------|------------------------------------------------------------- |
| neovim  | :heavy_check_mark: | |
| vscode  | :heavy_check_mark: | |

## Configuration
```
[[parser]]
l_file = "src/foo.l"
y_file = "src/foo.y"
extension = ".foo"

[[tests]]
dir = "tests/pass"
pass = true

[[tests]]
dir = "tests/fail"
pass = false
```

## VSCode Install

After installing prerequisites

```
cargo run --bin xtask -- install
```
for vscode you will need at least `vscode`, `npm`, `vsce`

```
npm install --global vsce
```

## Neovim Install

```
cargo run --bin xtask -- install --server
````
Then follow the configuration in [neovim/](neovim/README.md) 

## Developer options
Append `--console` to the install command, to enable tokio-console support. 

## Projects
[Crimson](https://github.com/ratmice/crimson)
