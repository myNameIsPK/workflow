local M = {}

function M.setup()
  local cmp = require("cmp")
  -- local luasnip = require("luasnip")
  local luasnip_ok, luasnip = pcall(require, "luasnip")
  if not luasnip_ok then
    return
  end

  require("plugins.luasnip").setup()

  cmp.setup {
    snippet = {
      -- REQUIRED - you must specify a snippet engine
      expand = function(args)
        luasnip.lsp_expand(args.body)
      end,
    },
    mapping = {
      ['<C-d>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
      ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
      ['<C-e>'] = cmp.mapping({
        i = cmp.mapping.abort(),
        c = cmp.mapping.close(),
      }),
      ['<C-y>'] = cmp.mapping.confirm {
        behavior = cmp.ConfirmBehavior.Insert,
        select = true,
      },
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
      -- { name = 'cmp_tabnine' },
      -- { name = 'spell' },
      { name = 'path' },
      -- { name = 'neorg' },
      -- { name = 'orgmode' },
    }, {
        { name = 'buffer', keyword_lenght = 5 },
    }),
    formatting = {
      deprecated = true,
      format = function(entry, vim_item)
        vim_item.kind = 'test'
        vim_item.menu = ({
          nvim_lsp = "[LSP]",
          emoji = "[Emoji]",
          path = "[Path]",
          calc = "[Calc]",
          cmp_tabnine = "[Tabnine]",
          vsnip = "[Vsnip]",
          luasnip = "[Luasnip]",
          buffer = "[Buffer]",
          spell = "[Spell]",
          neorg = "[Neorg]",
        })[entry.source.name]
        -- TODO: What is this?
        -- vim_item.dup = ({
        --   buffer = 1,
        --   path = 1,
        --   nvim_lsp = 0,
        -- })[entry.source.name] or 0
        return vim_item
      end,
    },
  }

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
