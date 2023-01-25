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
**Important** points:

  1. `nimbleparse_on_attach` below will call the `on_attach` function.
  2. **Don't** add nimbleparse_lsp to the loop based `setup` call instead using the script below.
  3. Because `on_attach` is declared local, you'll need to to add it to the same file or heredoc block.

## init.vim 
Add the following to init.vim or init.lua without the heredoc.


```
lua <<EOF

-- Add the following within the same block as your on_attach.

-- Extension to FileType map
-- You may need to modify this, otherwise the following code
-- assumes the filetype == extension without leading dot.
-- because there is not currently any way to query the neovim filetypes.
-- This may go away once there is a fix for
-- https://github.com/neovim/neovim/issues/18241
local extension_lang_map = {
        ["l"] = "lex",
        ["y"] = "yacc",
}

-- This function pulls file extensions out of nimbleparse.toml
-- and registers them in vim and the lsp client config.
local nimbleparse_lsp_attach = function(client, bufnr)
  local strip_prefix = function(p, s)
    return (s:sub(0, #p) == p) and s:sub(#p+1) or s
  end
  local contains = function(l, v)
    for _, value in ipairs(l) do
      if value == v then
        return true
      end
    end
    return false
  end
  vim.api.nvim_command("au BufRead,BufNewFile nimbleparse.toml set filetype=nimbleparse_toml")
  local config = client["config"]
  config["filetypes"] = { "nimbleparse_toml", "yacc", "lex" }
  local filetypes = config["filetypes"]

  for _, folder in pairs(client["workspace_folders"]) do
	  local workspace_path = folder["name"]
	  local handle = io.popen("nimbleparse_lsp --workspace " .. workspace_path)
	  local result = handle:read("*a")
	  handle:close()
	  local workspace = vim.json.decode(result)
	  for _, parser in pairs(workspace["parsers"]) do
		local ext = parser["extension"]
		local no_dot_ext = strip_prefix(".", ext)
		local language = extension_lang_map[no_dot_ext]
                if not language then
                  language = no_dot_ext
                end
		local ft_reg = "au BufRead,BufNewFile *" .. ext .. " set filetype=" .. language 
		if not contains(filetypes, no_dot_ext) then
		  filetypes[#filetypes+1] = language
		end
		vim.api.nvim_command(ft_reg)
	  end
  end
  on_attach(client, bufnr)
end

  vim.lsp.set_log_level 'info'
  local util = require 'lspconfig/util'
  local name = 'nimbleparse_lsp'
  local cmd = { name, '--server' }
  require 'lspconfig.configs'[name] = {
      default_config = {
        autostart = true,
	cmd = cmd,
	-- Don't add filetypes here, not adding them causes it to default to a glob pattern.
	root_dir = util.root_pattern('nimbleparse.toml'),
        settings = {},
      },
 }

  nimbleparse_lspconfig = {
  {
    on_attach = nimbleparse_lsp_attach,
    cmd = cmd,
  }
  require 'lspconfig'[name].setup(nimbleparse_lspconfig)
EOF
```

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
   Edit nimbleparse.lua
   ```
	Plug 'rcarriga/nvim-notify/'
   ```
   Add the following to nimbleparse.lua and add the key `handlers = handler_fns` to the `default_config` table also in `nimbleparse.lua`
   ```lua
	require("notify").setup{}
	vim.notify = require('notify')
	local severity = {
		"error",
		"warn",
		"info",
		"info",
	}

	-- set the following key to the handlers key in nimbleparse_lspconfig above.
	local handler_fns = {
	        ["window/showMessage"] = function(err, method, params, client_id)
		   vim.notify(method.message, severity[params.type])
	        end,
	  }
   ```

   It can also be configured to show [progress](https://github.com/rcarriga/nvim-notify/wiki/Usage-Recipes), but currently that appears to be fairly obtrusive when used with nimbleparse (although debounce_text_changes does not currently appear to be functioning at this time which should help).

## Testing

watch the log file,

* `:set ft?` show filetype.
* `:LspInfo` show LSP status
* `:lua print(vim.lsp.get_log_path())` path of log file...
