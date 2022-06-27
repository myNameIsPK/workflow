local cmp_ok, cmp = pcall(require, "cmp")
if not cmp_ok then
  return
end

local luasnip_ok, luasnip = pcall(require, "luasnip")
if not luasnip_ok then
  return
end

-- local check_backspace = function()
--   local col = vim.fn.col "." - 1
--   return col == 0 or vim.fn.getline("."):sub(col, col):match "%s"
-- end

cmp.setup {
  snippet = {
    -- you must specify a snippet engine
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = {
    ["<C-n>"] = cmp.mapping(cmp.mapping.select_next_item(), { "i", "c" }),
    ["<C-p>"] = cmp.mapping(cmp.mapping.select_prev_item(), { "i", "c" }),
    ["<C-d>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }),
    ["<C-f>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" }),
    ["<C-e>"] = cmp.mapping {
      i = cmp.mapping.abort(),
      c = cmp.mapping.close(),
    },
    -- ["<C-y>"] = cmp.config.disable,
    ["<C-y>"] = cmp.mapping.confirm { select = true },
    -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    ["<CR>"] = cmp.mapping.confirm { select = false },
    ["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
    -- if you woant to use <Tab>
    -- ["<Tab>"] = cmp.mapping(function(fallback)
    --   if cmp.visible() then
    --     cmp.select_next_item()
    --   elseif luasnip.expandable() then
    --     luasnip.expand()
    --   elseif luasnip.expand_or_jumpable() then
    --     luasnip.expand_or_jump()
    --   elseif check_backspace() then
    --     fallback()
    --   else
    --     fallback()
    --   end
    -- end, {
    --   "i",
    --   "s",
    -- }),
    -- ["<S-Tab>"] = cmp.mapping(function(fallback)
    --   if cmp.visible() then
    --     cmp.select_prev_item()
    --   elseif luasnip.jumpable(-1) then
    --     luasnip.jump(-1)
    --   else
    --     fallback()
    --   end
    -- end, {
    --   "i",
    --   "s",
    -- }),
  },
  confirm_opts = {
    behavior = cmp.ConfirmBehavior.Replace,
    select = false,
  },
  -- TODO: add more useful source
  sources = cmp.config.sources({ -- Group 1
    { name = "nvim_lsp" },
    -- { name = "nvim_lua" }, -- lua-dev is better
    { name = "luasnip" },
    -- { name = 'cmp_tabnine' },
    { name = "path" },
    { name = "nvim_lsp_signature_help" },
    { name = "neorg" },
    { name = "orgmode" },
  }, { -- Group 2
    { name = "spell" },
    { name = "buffer", keyword_lenght = 5 },
  }),
  formatting = {
    -- fields = { "kind", "abbr", "menu" }, --vscode-like menu
    deprecated = true,
    format = function(entry, vim_item)
      -- kinds from my _G.kind_icons
      vim_item.kind = string.format('%s %s', my.kind_icons[vim_item.kind], vim_item.kind) -- This concatonates the icons with the name of the item kind
      -- vim_item.kind = string.format("%s", my.kind_icons[vim_item.kind])
      vim_item.menu = ({
        nvim_lsp = "[LSP]",
        nvim_lua = "[Nvim_API]",
        emoji = "[Emoji]",
        path = "[Path]",
        cmdline = "[Cmd]",
        calc = "[Calc]",
        cmp_tabnine = "[Tabnine]",
        vsnip = "[Vsnip]",
        luasnip = "[Luasnip]",
        buffer = "[Buffer]",
        spell = "[Spell]",
        neorg = "[Neorg]",
        orgmode = "[Orgmode]",
        nvim_lsp_document_symbol = "[Symbol]",
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
  experimental = {
    ghost_text = false,
    native_menu = false,
  },
}

-- -- TODO: Set configuration for specific filetype.
-- cmp.setup.filetype('gitcommit', {
--   sources = cmp.config.sources({
--     { name = 'cmp_git' }, -- You can specify the `cmp_git` source if you were installed it.
--   }, {
--     { name = 'buffer' },
--   })
-- })

-- Use buffer source for `/` and `?`.
local search_sources = {
  sources = cmp.config.sources {
    { name = "nvim_lsp_document_symbol" },
    { name = "buffer" },
  },
}
cmp.setup.cmdline("/", search_sources)
cmp.setup.cmdline("?", search_sources)

-- Use cmdline & path source for ':'.
local cmd_sources = {
  sources = cmp.config.sources {
    { name = "path" },
    { name = "cmdline" },
  },
}
cmp.setup.cmdline(":", cmd_sources)
