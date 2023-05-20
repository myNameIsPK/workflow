return {
  {
    "nvim-telescope/telescope.nvim",
    cmd = "Telescope",
    config = function()
      local ts = require "telescope"
      pcall(ts.load_extension, "fzf")
      pcall(ts.load_extension, "projects")
      pcall(ts.load_extension, "ui-select")
      pcall(ts.load_extension, "zk")

      local actions = require "telescope.actions"
      local action_layout = require "telescope.actions.layout"
      local opts = {}
      opts.extensions = {
        fzf = {
          fuzzy = true,
          override_generic_sorter = true,
          override_file_sorter = true,
          case_mode = "smart_case",
        },
      }
      opts.defaults = require("telescope.themes").get_ivy {
        cycle_layout_list = {
          "vertical",
          "horizontal",
          "center",
          "cursor",
          "flex",
          "bottom_pane",
        },
        border = true,
        winblend = 15,
        borderchars = { "─", "│", "─", "│", "┌", "┐", "┘", "└" },
        dynamic_preview_title = true,
        mappings = {
          i = {
            ["<C-l>"] = action_layout.cycle_layout_next,
            ["<C-h>"] = action_layout.cycle_layout_prev,

            ["<Down>"] = actions.cycle_history_next,
            ["<Up>"] = actions.cycle_history_prev,
          },
          n = {
            ["<C-n>"] = actions.move_selection_next,
            ["<C-p>"] = actions.move_selection_previous,

            ["<C-c>"] = actions.close,
          },
        },
      }

      ts.setup(opts)
    end,
  },

  { "nvim-telescope/telescope-ui-select.nvim", dependencies = { "nvim-telescope/telescope.nvim" } },

  {
    "hrsh7th/nvim-cmp",
    event = { "InsertEnter", "CmdlineEnter" },
    config = function()
      require "plugins.config.cmp"
      require "my.luasnip"
    end,
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

  {
    "windwp/nvim-autopairs",
    event = { "InsertEnter" },
    config = function()
      require "plugins.config.autopairs"
    end,
  },
}
