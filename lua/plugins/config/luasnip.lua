local ls_ok, ls = pcall(require, "luasnip")
if not ls_ok then
  return
end

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
local types = require "luasnip.util.types"

ls.cleanup()

ls.config.set_config {
  history = true,
  updateevents = "TextChanged,TextChangedI",
  ext_opts = {
    [types.choiceNode] = {
      active = {
        virt_text = { { "<- Choices", "WarningMsg" } },
      },
    },
  },
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

local snippets = {}

snippets.markdown = {}

snippets.markdown.task = function()
  return sn(nil, fmt("- [{}] {}", {
      c(1, {
        i(nil, " "),
        i(nil, "x")
      }),
      i(2, "task..."),
    }))
end

snippets.markdown.recur_task = function()
  return sn(nil, {
    d(1, snippets.markdown.task, {}),
    t({"",""}), -- newline
    c(2, {
      t({""}),
      d(nil, snippets.markdown.recur_task, {})
    })
  })
end

ls.add_snippets("markdown", {

  s({ trig = "tl", name = "task list" }, d(1, snippets.markdown.recur_task, {})),

})

-- stylua: ignore end

-- this work only friendly-snippets
require("luasnip.loaders.from_vscode").lazy_load()
require("luasnip.loaders.from_vscode").lazy_load { paths = { "~/.config/nvim/snippets/vscode" } }

my.map({ "i", "s" }, "<C-j>", function()
  if ls.expand_or_jumpable() then
    ls.expand_or_jump()
  end
end)

my.map({ "i", "s" }, "<C-k>", function()
  if ls.jumpable(-1) then
    ls.jump(-1)
  end
end)

my.map({ "i", "s" }, "<C-l>", function()
  if ls.choice_active() then
    ls.change_choice(1)
  end
end)

my.map({ "i", "s" }, "<C-h>", function()
  require "luasnip.extras.select_choice"()
end)
