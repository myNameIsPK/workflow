if not _G.my then
  _G.my = {}
end

---@type string[] list of dependencies of executables
my.exec_deps = { "git" }
---@type boolean
my.is_termux = vim.env.TERMUX_VERSION and true or false
---@type string[] list of optional dependencies of executables
my.opt_exec_deps = { "rg", "fd", "fzf" }

---@type table global configure options
my.opts = {}
---@type table Config options of LSP
my.opts.lsp = {}
---@type boolean Enable/Disable LSP document highlight
my.opts.lsp.document_highlight = false
---@type boolean Enable/Disable LSP semantic tokens
my.opts.lsp.semantic_tokens = false
---@type boolean Enable/Disable LSP inlay hints
my.opts.lsp.inlay_hints = false

---@type table Config options of colorscheme
my.opts.colorscheme = {}
---@type string name of the Neovim default colorscheme eg. "retrobox", "morning"
my.opts.colorscheme.default = "default"
---@type boolean if true use the plugin colorscheme if posible(overide `default`)
my.opts.colorscheme.use_plugin = false
---@type "dark"|"light"
my.opts.colorscheme.background_default = "light"
---@type boolean Sync background with MY_BACKGROUND env vars
my.opts.colorscheme.background_system_sync = true

---@return "dark"|"light"
my.opts.background_resolve = function(self)
  if not self.colorscheme.background_system_sync then
    return self.colorscheme.background_default
  end
  local bg_env = vim.env.MY_BACKGROUND
  if bg_env == "light" or bg_env == "dark" then
    return bg_env
  end
  return self.colorscheme.background_default
end

local diagnostic_signs = {
  -- { name = "DiagnosticSignError", text = "" },
  -- { name = "DiagnosticSignWarn", text = "" },
  -- { name = "DiagnosticSignHint", text = "" },
  -- { name = "DiagnosticSignInfo", text = "" },
  { name = "DiagnosticSignError", text = "E" },
  { name = "DiagnosticSignWarn", text = "W" },
  { name = "DiagnosticSignHint", text = "H" },
  { name = "DiagnosticSignInfo", text = "I" },
}

-- Define diagnostic_signs the same name as diagnostic_signs_highlight
for _, sign in ipairs(diagnostic_signs) do
  vim.fn.sign_define(sign.name, { texthl = sign.name, text = sign.text, numhl = "" })
end

my.save_and_exec = function()
  vim.cmd "silent! write"
  vim.cmd "source %"
  vim.notify("Save and Exec " .. vim.fn.getreg "%", vim.log.levels.INFO)
end

_G.I = function(object)
  print(vim.inspect(object))
end

_G.R = require("my.plugin.reload").reload

my.map = function(mode, lhs, rhs, opt)
  local options = { noremap = true, silent = false }
  if opt then
    options = vim.tbl_extend("force", options, opt)
  end
  if type(lhs) == "table" then
    for _, key in ipairs(lhs) do
      my.map(mode, key, rhs, opt)
    end
  else
    vim.keymap.set(mode, lhs, rhs, opt)
  end
end
