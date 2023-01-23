
-- Add the following within the same block as your on_attach.

-- Extension to FileType map
-- You may need to modify this, otherwise the following code
-- assumes the filetype == extension without leading dot.
-- because there is not currently any way to query the neovim filetypes.
-- This may go away once there is a fix for
-- https://github.com/neovim/neovim/issues/18241
function setup(on_attach)
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
    if on_attach then
      on_attach(client, bufnr)
    end
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

  local nimbleparse_lspconfig = {
    on_attach = nimbleparse_lsp_attach,
    cmd = cmd,
  }
  require 'lspconfig'[name].setup(nimbleparse_lspconfig)
end
