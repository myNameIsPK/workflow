local status_ok, blankline = pcall(require, "indent_blankline")
if not status_ok then
  return
end

blankline.setup {
  enabled = false,
  show_current_context = true,
  -- show_current_context_start = true,
  use_treesitter = true,

  -- -- for background highlight version
  -- char = "",
  -- char_highlight_list = {
  --   "IndentBlanklineIndent1",
  --   "IndentBlanklineIndent2",
  -- },
  -- space_char_highlight_list = {
  --   "IndentBlanklineIndent1",
  --   "IndentBlanklineIndent2",
  -- },
}

local hi_link = require("utils.highlight").hi_link
local hi = require("utils.highlight").highlight
hi_link("IndentBlanklineChar", "Fg4")
hi_link("IndentBlanklineContextChar", "Fg2")
hi("IndentBlanklineContextStart", { style = "underline" })

hi_link("IndentBlanklineIndent1", "Bg0")
hi_link("IndentBlanklineIndent2", "Bg1")
