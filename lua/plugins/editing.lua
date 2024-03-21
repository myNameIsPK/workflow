return {
  {
    "numToStr/Comment.nvim",
    keys = { "gc", { "gc", mode = "v" } },
    config = function()
      require("Comment").setup {
        padding = true,
        sticky = true,
        ignore = nil,
        toggler = { line = "gcc", block = "gbc" },
        opleader = { line = "gc", block = "gb" },
        extra = { above = "gcO", below = "gco", eol = "gcA" },
        mappings = { basic = true, extra = true },
        pre_hook = require("ts_context_commentstring.integrations.comment_nvim").create_pre_hook(),
        post_hook = nil,
      }
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
    keys = { "<M-s>" },
    config = function()
      vim.keymap.set("n", "<M-s>", "<Nop>")
      require("mini.surround").setup {
        mappings = {
          add = "<M-s>a", -- Add surrounding in Normal and Visual modes
          delete = "<M-s>d", -- Delete surrounding
          find = "<M-s>f", -- Find surrounding (to the right)
          find_left = "<M-s>F", -- Find surrounding (to the left)
          highlight = "<M-s>h", -- Highlight surrounding
          replace = "<M-s>r", -- Replace surrounding
          update_n_lines = "<M-s>n", -- Update `n_lines`

          suffix_last = "l", -- Suffix to sarch with "prev" method
          suffix_next = "n", -- Suffix to search with "next" method
        },
        ---@type 'cover'|'cover_or_next'|'cover_or_prev'|'cover_or_nearest'|'next'|'prev'|'nearest'
        search_method = "cover",
      }
    end,
  },

  {
    "ggandor/leap.nvim",
    keys = { "<M-f>", "<M-F>" },
    dependencies = "tpope/vim-repeat",
    config = function()
      -- require("leap").add_default_mappings()
      -- vim.keymap.set("n", "<leader>sS", "<Plug>(leap-from-window)")
      vim.keymap.set("n", "<M-f>", function()
        local focusable_windows_on_tabpage = vim.tbl_filter(function(win)
          return vim.api.nvim_win_get_config(win).focusable
        end, vim.api.nvim_tabpage_list_wins(0))
        require("leap").leap { target_windows = focusable_windows_on_tabpage }
      end, { desc = "Leap Bidirection/All win" })
      vim.keymap.set("n", "<M-F>", function()
        local current_window = vim.fn.win_getid()
        require("leap").leap { target_windows = { current_window } }
      end, { desc = "Leap Bidirection/Current win" })
    end,
  },

  {
    "monaqa/dial.nvim",
    config = function()
      local augend = require "dial.augend"
      require("dial.config").augends:register_group {
        default = {
          augend.constant.alias.bool,
          augend.integer.alias.decimal,
          augend.integer.alias.hex,
          augend.semver.alias.semver,
          augend.constant.alias.alpha,
          augend.constant.alias.Alpha,
          augend.date.alias["%Y/%m/%d"],
          augend.date.alias["%Y-%m-%d"],
          augend.date.alias["%m/%d"],
          augend.date.alias["%H:%M"],
          augend.constant.new {
            elements = { "and", "or" },
            word = true, -- if false, "sand" is incremented into "sor", "doctor" into "doctand", etc.
            cyclic = true,
          },
        },
      }
    end,
    init = function()
      vim.keymap.set("n", "<C-a>", function()
        require("dial.map").manipulate("increment", "normal")
      end)
      vim.keymap.set("n", "<C-x>", function()
        require("dial.map").manipulate("decrement", "normal")
      end)
      vim.keymap.set("n", "g<C-a>", function()
        require("dial.map").manipulate("increment", "gnormal")
      end)
      vim.keymap.set("n", "g<C-x>", function()
        require("dial.map").manipulate("decrement", "gnormal")
      end)
      vim.keymap.set("v", "<C-a>", function()
        require("dial.map").manipulate("increment", "visual")
      end)
      vim.keymap.set("v", "<C-x>", function()
        require("dial.map").manipulate("decrement", "visual")
      end)
      vim.keymap.set("v", "g<C-a>", function()
        require("dial.map").manipulate("increment", "gvisual")
      end)
      vim.keymap.set("v", "g<C-x>", function()
        require("dial.map").manipulate("decrement", "gvisual")
      end)
    end,
  },
}
