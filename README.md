## Purpose

Primarily intended for the development of grammars.
It seeks to remove any/all extraneous steps between editing a grammar testing it.
It does not run generate source code, compile the sources, or run actions embedded in the grammar.
While actions get ignored you can optionally build a generic syntax tree.

Reparsing is performed on the editors buffer, on change, so you do not need to save the changes to reparse.


Error recovery is optional (enabled by default), but will be slower than disabling it.

## Features

* Parse test input on lex/yacc file change.
* Parse test input on test input change.

## Editor support

VSCode will automatically discover and register for the test input file extension
Other editors require hard coding of the test input file extension.


| Editor  | supported | caveats |
| ------- | ----------|------------------------------------------------------------- |
| neovim  |           | requires manual config, no dynamically registered extensions |
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
