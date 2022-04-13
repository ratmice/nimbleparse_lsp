## Editor support

Currently the only editor supported is vscode.
Because we deal not with a hard coded file extension,
but an extension which gets discovered from a workspace.
This  requires some amount of support from the editor.


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
