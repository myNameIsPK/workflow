if not _G.my then
  _G.my = {}
end

---@type string[] list of dependencies of executables
my.exec_deps = { "git" }
---@type string[] list of optional dependencies of executables
my.opt_exec_deps = { "rg", "fd", "fzf" }

---@type table global configure options
my.opts = {
  ---@type table Config options of LSP
  lsp = {
    ---@type boolean Enable/Disable LSP document highlight
    document_highlight = false,
    ---@type boolean Enable/Disable LSP samantic tokens
    samantic_tokens = false,
  },
  ---@type table Config options of colorscheme
  colorscheme = {
    ---@type string name of the Neovim default colorscheme eg. "retrobox", "morning"
    default = "retrobox",
    ---@type boolean if true use the plugin colorscheme if posible(overide `default`)
    use_plugin = false,
    ---@type "dark"|"light"
    background_default = "light",
    ---@type boolean Sync background with MY_BACKGROUND env vars
    background_system_sync = true,
  },
}

---@return "dark"|"light"
my.opts.background_resolve = function(self)
  if self.colorscheme.background_system_sync then
    local bg_env = vim.env.MY_BACKGROUND
    for _, v in ipairs { "light", "dark" } do
      if bg_env == v then
        return bg_env
      end
    end
  end
  return self.colorscheme.background_default
end

local diagnostic_signs = {
  { name = "DiagnosticSignError", text = "" },
  { name = "DiagnosticSignWarn", text = "" },
  { name = "DiagnosticSignHint", text = "" },
  { name = "DiagnosticSignInfo", text = "" },
}

-- Define diagnostic_signs the same name as diagnostic_signs_highlight
for _, sign in ipairs(diagnostic_signs) do
  vim.fn.sign_define(sign.name, { texthl = sign.name, text = sign.text, numhl = "" })
end

my.kind_icons = {
  Text = "",
  Method = "m",
  Function = "",
  Constructor = "",
  Field = "",
  Variable = "",
  Class = "",
  Interface = "",
  Module = "",
  Property = "",
  Unit = "",
  Value = "",
  Enum = "",
  Keyword = "",
  Snippet = "",
  Color = "",
  File = "",
  Reference = "",
  Folder = "",
  EnumMember = "",
  Constant = "",
  Struct = "",
  Event = "",
  Operator = "",
  TypeParameter = "",
}

my.save_and_exec = function()
  vim.cmd "silent! write"
  if vim.bo.filetype == "lua" then
    vim.cmd "luafile %"
  elseif vim.bo.filetype == "vim" then
    vim.cmd "source %"
  end
  vim.notify("Save and Exec " .. vim.fn.getreg "%", vim.log.levels.INFO)
end

_G.I = function(object)
  print(vim.inspect(object))
end

_G.R = require("utils.reload").reload

local is_override, _ = pcall(require, "override_globals")
if is_override then
  vim.notify("Detect `override_globals.lua`: override `my` global vars", vim.log.levels.INFO)
end
