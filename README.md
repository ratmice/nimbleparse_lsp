## Editor support

Currently the only editor supported is vscode.
Because we deal not with a hard coded file extension,
but an extension which gets discovered from a workspace.
This  requires some amount of support from the editor.


| Editor  | supported | excuse                                                  |
| ------- | ----------|-------------------------------------------------------- |
| neovim  |           | not obvious how to dynamically register file extensions |
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

## Install

After installing prerequisites

```
cargo run --bin xtask -- install
```

## Prerequisites
This is perhaps incomplete, you'll need at least `vscode`, `npm`, `vsce`

```
npm install --global vsce
```
