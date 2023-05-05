local ls = require "luasnip"

local s = ls.snippet
local sn = ls.snippet_node
local isn = ls.indent_snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_node
local r = ls.restore_node
local events = require "luasnip.util.events"
local ai = require "luasnip.nodes.absolute_indexer"
local fmt = require("luasnip.extras.fmt").fmt
local m = require("luasnip.extras").m
local lambda = require("luasnip.extras").l
local rep = require("luasnip.extras").rep

ls.cleanup()

ls.config.set_config {
  history = true,
  updateevents = "TextChanged,TextChangedI",
}

-- stylua: ignore start
ls.add_snippets("lua", {
  s("ig", {
    t "-- stylua: ignore",
  }),

  s("igstart", {
    t "-- stylua: ignore start",
  }),

  s("igend", {
    t "-- stylua: ignore end",
  }),

  s("preq", fmt([[
    local {}_ok, {} = pcall(require, "{}")
    if not {}_ok then
      return
    end
  ]], {
    i(2, "test"),
    rep(2),
    i(1, "module_name"),
    rep(2),
  }))

})

ls.add_snippets("markdown", {

  s("todo", fmt([[
  - [{}] {}
  ]], {
    c(1, {
        t(" "), t("x")
      }),
    i(2, "task..."),
  })),

})

-- stylua: ignore end

-- this work only friendly-snippets
require("luasnip.loaders.from_vscode").lazy_load()
require("luasnip.loaders.from_vscode").lazy_load { paths = { "~/.config/nvim/snippets/vscode" } }

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
