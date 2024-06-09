return {
  {
    "nvim-telescope/telescope.nvim",
    cmd = "Telescope",
    config = function()
      local ts = require "telescope"
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
        bibtex = {
          depth = 10,
          context = true,
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
        -- borderchars = { "─", "│", "─", "│", "┌", "┐", "┘", "└" },
        borderchars = { "", "", "", "", "", "", "", "" },
        dynamic_preview_title = true,
        color_devicons = true,
        path_display = { len = 2, exclude = { -1, -2 } },
        mappings = {
          i = {
            ["<M-o>"] = action_layout.cycle_layout_next,
            ["<M-n>"] = actions.cycle_history_next,
            ["<M-p>"] = actions.cycle_history_prev,

            ["<Down>"] = actions.move_selection_next,
            ["<Up>"] = actions.move_selection_previous,
          },
          n = {
            ["<M-o>"] = action_layout.cycle_layout_next,
            ["<M-n>"] = actions.cycle_history_next,
            ["<M-p>"] = actions.cycle_history_prev,

            ["<C-n>"] = actions.move_selection_next,
            ["<C-p>"] = actions.move_selection_previous,

            ["<C-c>"] = actions.close,
          },
        },
      }

      ts.setup(opts)
      pcall(ts.load_extension, "fzf")
      pcall(ts.load_extension, "projects")
      pcall(ts.load_extension, "ui-select")
      pcall(ts.load_extension, "zk")
      pcall(ts.load_extension, "bibtex")
    end,
  },

  { "nvim-telescope/telescope-ui-select.nvim", dependencies = { "nvim-telescope/telescope.nvim" } },

  { "nvim-telescope/telescope-bibtex.nvim", dependencies = { "nvim-telescope/telescope.nvim" } },

  {
    "hrsh7th/nvim-cmp",
    event = { "InsertEnter", "CmdlineEnter" },
    config = function()
      require "plugins.config.cmp"
    end,
    dependencies = {
      {
        "L3MON4D3/LuaSnip",
        config = function()
          require "plugins.config.luasnip"
        end,
        dependencies = { "rafamadriz/friendly-snippets" },
      },
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
