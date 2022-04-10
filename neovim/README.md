## VimPlug
if using [vimplug](https://github.com/junegunn/vim-plug)
```
call plug#begin()
if has('nvim')
        Plug 'neovim/nvim-lspconfig'
endif
call plug#end()
```
then `:PlugUpdate`

## Filetypes
You'll need to manually configure filetypes for the test extension.
Below is an example configuration for the [crimson](https://github.com/ratmice/crimson) project
that nimbleparse_lsp was written for.

Change the crimson extension to whatever extension you are trying to parse.
Add this as `~/.config/nvim/ftdetect/crimson.vim`
```
au BufRead,BufNewFile *.crimson set filetype=crimson
au BufRead,BufNewFile nimbleparse.toml set filetype=nimbleparse_toml
```

## init.vim 
Add the following to init.vim or init.lua without the heredoc.

```
lua << EOF
  vim.lsp.set_log_level 'info'
  local nvim_lsp = require 'lspconfig'
  local configs = require 'lspconfig.configs'
  local util = require 'lspconfig/util'
  local name = 'nimbleparse_lsp'
  local cmd = { name, '--server' }
  configs[name] = {
      default_config = {
        autostart = true,
        cmd = cmd,
        filetypes = { 'crimson', 'lex', 'yacc', 'nimbleparse_toml' },
        root_dir = util.root_pattern('nimbleparse.toml'),
        settings = {},
      },
  }

  nvim_lsp[name].setup {
    cmd = cmd,
  }
EOF
```

## Testing

There isn't much done in the plugin yet besides logging to the log file, so you'll want to `tail -f that`...

* `:set ft?` show filetype.
* `:LspInfo` show LSP status
* `:lua print(vim.lsp.get_log_path())` path of log file...


