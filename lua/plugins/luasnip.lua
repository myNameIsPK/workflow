local ls = require("luasnip")

local s = ls.snippet
local sn = ls.snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_node

ls.config.set_config({
  history = true,
  updateevents = "TextChanged,TextChangedI",
})

-- FIXME: Must type and press tab only, no completion
ls.add_snippets("lua", {
    s({ trig = "[[-", wordTrig = false, hidden = true }, {
      t "--[[",
      t { "", "\t" },
      i(0),
      t { "", "--]]" },
    }),
    s({ trig = "ig", wordTrig = true, hidden = true }, {
      t "-- stylua: ignore",
    }),
  }
)

-- this work only friendly-snippets
require('luasnip.loaders.from_vscode').lazy_load()
require('luasnip.loaders.from_vscode').lazy_load { paths =  { '~/.config/nvim/snippets/vscode', }}

local map = require("utils.mappings").map

map({ "i", "s" }, "<C-j>", function()
  if ls.expand_or_jumpable() then
    ls.expand_or_jump()
  end
end)

map({ "i", "s" }, "<C-k>", function()
  if ls.jumpable(-1) then
    ls.jump(-1)
  end
end)

map("i", "<C-l>", function()
  if ls.choice_active() then
    ls.change_choice(1)
  end
end)
