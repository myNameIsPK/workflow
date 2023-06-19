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
    -- use `gs` because `:h gs` is useless
    keys = { "gs", { "gs", mode = "v" } },
    config = function()
      require("mini.surround").setup {
        mappings = {
          add = "gsa", -- Add surrounding in Normal and Visual modes
          delete = "gsd", -- Delete surrounding
          find = "gsf", -- Find surrounding (to the right)
          find_left = "gsF", -- Find surrounding (to the left)
          highlight = "gsh", -- Highlight surrounding
          replace = "gsr", -- Replace surrounding
          update_n_lines = "gsn", -- Update `n_lines`

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
