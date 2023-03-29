_G.my = {}

my.opts = {
  document_highlight = false,
}

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
