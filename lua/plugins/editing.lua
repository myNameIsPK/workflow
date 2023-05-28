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
    keys = { "<c-s>", { "<c-s>", mode = "v" } },
    config = function()
      require("mini.surround").setup {
        mappings = {
          add = "<c-s>a", -- Add surrounding in Normal and Visual modes
          delete = "<c-s>d", -- Delete surrounding
          find = "<c-s>f", -- Find surrounding (to the right)
          find_left = "<c-s>F", -- Find surrounding (to the left)
          highlight = "<c-s>h", -- Highlight surrounding
          replace = "<c-s>r", -- Replace surrounding
          update_n_lines = "<c-s>n", -- Update `n_lines`

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
