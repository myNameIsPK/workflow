return {
  -- {
  --   "akinsho/toggleterm.nvim",
  --   config = function()
  --     require("toggleterm").setup()
  --   end,
  -- },

  {
    "echasnovski/mini.align",
    event = "VeryLazy",
    config = function()
      require("mini.align").setup()
    end,
  },

  {
    "echasnovski/mini.surround",
    event = "VeryLazy",
    config = function()
      require("mini.surround").setup {
        mappings = {
          add = "gza", -- Add surrounding in Normal and Visual modes
          delete = "gzd", -- Delete surrounding
          find = "gzf", -- Find surrounding (to the right)
          find_left = "gzF", -- Find surrounding (to the left)
          highlight = "gzh", -- Highlight surrounding
          replace = "gzr", -- Replace surrounding
          update_n_lines = "gzn", -- Update `n_lines`

          suffix_last = "l", -- Suffix to search with "prev" method
          suffix_next = "n", -- Suffix to search with "next" method
        },
      }
    end,
  },

  {
    "ggandor/leap.nvim",
    dependencies = "tpope/vim-repeat",
    lazy = false,
    init = function()
      require("leap").add_default_mappings()
    end,
  },
}
