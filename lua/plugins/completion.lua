return {
  { "nvim-telescope/telescope.nvim" },
  { "nvim-telescope/telescope-ui-select.nvim" },

  -- { "ibhagwan/fzf-lua" },

  { "numToStr/Comment.nvim" },

  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      { "L3MON4D3/LuaSnip" },
      { "rafamadriz/friendly-snippets", lazy = false },
      { "saadparwaiz1/cmp_luasnip" },
      -- { "hrsh7th/cmp-nvim-lua" }, -- neodev is better
      { "hrsh7th/cmp-nvim-lsp" },
      { "hrsh7th/cmp-nvim-lsp-document-symbol" },
      { "hrsh7th/cmp-nvim-lsp-signature-help" },
      { "hrsh7th/cmp-cmdline" },
      { "f3fora/cmp-spell" },
      { "hrsh7th/cmp-path" },
      { "hrsh7th/cmp-buffer" },
      -- { 'tzachar/cmp-tabnine' },
    },
  },

  { "windwp/nvim-autopairs" },
}
