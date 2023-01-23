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
Because nimbleparse_lsp works both with lex/yacc files,
as well as the files your that parser parses.

It requires a more complex initialization in the editor client than is typical.

## Setting up LSP
The following assumes that you have set up nvim-lspconfig like the [Suggested Configuration](https://github.com/neovim/nvim-lspconfig#Suggested-configuration).

```
cp nimbleparse.lua .config/nvim/lua/
```

call `require('nimbleparse').setup(on_attach)` passing in your `on_attach` function from nvim-lspconfig, or nil if you don't use one.

## Further configuration

nimbleparse_lsp will generate diagnostics even for multiple files, even which are not open.
Beyond the builtin diagnostic support, there are neovim plugins worth looking into.

However many of them offer different variations of the same functionality.

### Diagnostics

1. [telescope](https://github.com/nvim-telescope/telescope.nvim) :thumbsup:
   The `Telescope diagnostics` can show diagnostics for all files,
   or `Telescope diagnostics bufnr=0` for the current file.
2. [Trouble](https://github.com/folke/trouble.nvim) :thumbsup:
    Shows diagnostics in it separate pane.
3. [NvimTree](https://github.com/kyazdani42/nvim-tree.lua) :thumbsup:
   `Plug 'kyazdani42/nvim-tree.lua`

   When configured to do so, nvim-tree will show diagnostic signs and
   highlight files with the appropriate color,
   When you leave the tree open, it will update a diagnostics change.
   ```
   nnoremap <leader>t <cmd>NvimTreeToggle<cr>
   lua <<EOF
   require('nvim-tree').setup{
     view = {
       signcolumn = "yes",
     },
     diagnostics = {
       enable = true,
       show_on_dirs = true
     }
   }
   EOF
   ```
### Progress indication

3. [fidget](https://github.com/j-hui/fidget.nvim) :thumbsup:
   Progress above the status line.
4. [lsp-status](https://github.com/nvim-lua/lsp-status.nvim)
   Progress and diagnostic counts info on the status line.
5. [lualine-lsp-progress](https://github.com/arkav/lualine-lsp-progress) :thumbsup:
   Nice unobtrusive, easy to configure.

### Notifications

nimbleparse_lsp only emits a few (one?) notification,
when it needs to reload due to nimbleparse.toml changes.
by default nvim doesn't show them though.

6. [nvim-notify](https://github.com/rcarriga/nvim-notify/) :thumbsup:
   ```
	Plug 'rcarriga/nvim-notify/'

	lua <<EOF
	require("notify").setup{}
	vim.notify = require('notify')
	local severity = {
		"error",
		"warn",
		"info",
		"info",
	}

	-- set the following key to the handlers key in nimbleparse_lspconfig above.
	local handlers = {
	        ["window/showMessage"] = function(err, method, params, client_id)
		   vim.notify(method.message, severity[params.type])
	        end,
	  }
	EOF
   ```
   It can also be configured to show [progress](https://github.com/rcarriga/nvim-notify/wiki/Usage-Recipes), but currently that appears to be fairly obtrusive when used with nimbleparse (although debounce_text_changes does not currently appear to be functioning at this time which should help).



## Testing

watch the log file,

* `:set ft?` show filetype.
* `:LspInfo` show LSP status
* `:lua print(vim.lsp.get_log_path())` path of log file...
