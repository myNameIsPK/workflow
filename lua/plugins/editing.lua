return {
  {
    "numToStr/Comment.nvim",
    keys = { "gc", { "gc", mode = "v" } },
    config = function()
      require("Comment").setup()
    end,
  },

  {
    "echasnovski/mini.align",
    keys = { "ga", { "ga", mode = "v" } },
    config = function()
      require("mini.align").setup()
    end,
  },

  {
    "echasnovski/mini.surround",
    keys = { "gz", { "gz", mode = "v" } },
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
    keys = { "s", "S", { "s", mode = "v" }, { "S", mode = "v" } },
    dependencies = "tpope/vim-repeat",
    config = function()
      require("leap").add_default_mappings()
    end,
  },
}
