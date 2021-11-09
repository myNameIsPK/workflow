local M = {}

function M.setup()
  local api = vim.api
  local cmp = require("cmp")

  local function t(str)
    return api.nvim_replace_termcodes(str, true, true, true)
  end

  local function feed(key, mode)
    api.nvim_feedkeys(t(key), mode or '', true)
  end

  local function get_luasnip()
    local ok, luasnip = pcall(require, "luasnip")
    if not ok then
      return nil
    end
    return luasnip
  end

  -- FIXME: tabout not installed
  local function tab(fallback)
    local luasnip = get_luasnip()
    if cmp.visible() then
      cmp.select_next_item()
    elseif luasnip and luasnip.expand_or_jumpable() then
      feed '<Plug>luasnip-expand-or-jump'
    -- elseif api.nvim_get_mode().mode == 'c' then
    --   fallback()
    -- else
    --   feed '<Plug>(Tabout)'
    -- end
    else
      fallback()
    end
  end

  local function shift_tab(fallback)
    local luasnip = get_luasnip()
    if cmp.visible() then
      cmp.select_prev_item()
    elseif luasnip and luasnip.jumpable(-1) then
      feed '<Plug>luasnip-jump-prev'
    -- elseif api.nvim_get_mode().mode == 'c' then
    --   fallback()
    -- else
    --   feed '<Plug>(TaboutBack)'
    -- end
    else
      fallback()
    end
  end

  cmp.setup({
    -- snippet = {
    --   -- REQUIRED - you must specify a snippet engine
    --   expand = function(args)
    --     require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
    --     -- vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
    --     -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
    --     -- require'snippy'.expand_snippet(args.body) -- For `snippy` users.
    --   end,
    -- },
    mapping = {
      ['<Tab>'] = cmp.mapping(tab, { 'i', 'c' }),
      ['<S-Tab>'] = cmp.mapping(shift_tab, { 'i', 'c' }),
      ['<C-d>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
      ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
      ['<C-e>'] = cmp.mapping({
        i = cmp.mapping.abort(),
        c = cmp.mapping.close(),
      }),
      ['<CR>'] = cmp.mapping.confirm {
        behavior = cmp.ConfirmBehavior.Replace,
        select = true,
      },
    },
    documentation = {
      border = 'rounded',
    },
    sources = cmp.config.sources({
        { name = 'nvim_lsp' },
        { name = 'luasnip' },
        { name = 'cmp_tabnine' },
        { name = 'spell' },
        { name = 'path' },
        { name = 'neorg' },
        { name = 'orgmode' },
      }, {
        { name = 'buffer' },
      })
  })

  -- Use buffer source for `/` and `?`.
  local search_sources = {
    sources = cmp.config.sources({
      { name = 'nvim_lsp_document_symbol' },
    }, {
      { name = 'buffer' },
    }),
  }
  cmp.setup.cmdline('/', search_sources)
  cmp.setup.cmdline('?', search_sources)

  -- Use cmdline & path source for ':'.
  cmp.setup.cmdline(':', {
    sources = cmp.config.sources({
      { name = 'path' },
    }, {
      { name = 'cmdline' },
    }),
  })

end

function M.autopairs()
  require('nvim-autopairs').setup({
    disable_filetype = { 'TelescopePrompt', 'vim' },
  })

  local ok, cmp_autopairs = pcall(require, "nvim-autopairs.completion.cmp")
  if not ok then
    return
  end
  local cmp = require('cmp')
  cmp.event:on('confirm_done', cmp_autopairs.on_confirm_done({ map_char = { tex = '' } }))
end

return M
