local M = {}

function M.setup()
  local ls = require("luasnip")

  local s = ls.snippet
  -- local sn = ls.snippet_node
  local t = ls.text_node
  local i = ls.insert_node
  -- local f = ls.function_node
  -- local c = ls.choice_node
  -- local d = ls.dynamic_node

  ls.config.set_config({
    history = true,
    -- Update more often, :h events for more info.
    updateevents = "TextChanged,TextChangedI",
  })

  ls.snippets = {
    all = {},
    -- FIXME: Must type and press tab only, no completion
    lua = {
      s({ trig = "[[-", wordTrig = false, hidden = true }, {
        t "--[[",
        t { "", "\t" },
        i(0),
        t { "", "--]]" },
      }),
      s({ trig = "ig", wordTrig = true, hidden = true }, {
        t "-- stylua: ignore",
      }),
    },
  }

  -- FIXME: fix hard code and why relative path not work
  -- this work only friendly-snippets
  -- require('luasnip/loaders/from_vscode').lazy_load()
  -- work both but hardcoded
  require('luasnip/loaders/from_vscode').lazy_load { paths =  {
    '~/.config/nvim/snippets/vscode',
    '~/.local/share/nvim/site/pack/packer/start/friendly-snippets',
  }}

  -- TODO: convert to lua
  -- FIXME: <C-E> not work
  vim.cmd [[
    imap <silent><expr> <Tab> luasnip#expand_or_jumpable() ? '<Plug>luasnip-expand-or-jump' : '<Tab>' 
    inoremap <silent> <S-Tab> <cmd>lua require'luasnip'.jump(-1)<Cr>

    snoremap <silent> <Tab> <cmd>lua require('luasnip').jump(1)<Cr>
    snoremap <silent> <S-Tab> <cmd>lua require('luasnip').jump(-1)<Cr>

    imap <silent><expr> <C-E> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-E>'
    smap <silent><expr> <C-E> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-E>'
  ]]
end

return M
