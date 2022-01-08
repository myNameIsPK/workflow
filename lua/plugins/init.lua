local utils = require "utils.plugins"

utils.bootstrap_packer()

require("packer").startup(function(use)
  -- Packer can manage itself as an optional plugin
  use { "wbthomason/packer.nvim" }

  -- Performance
  use { "dstein64/vim-startuptime" }

  -- Telescope Fuzzy Finder
  use {
    "nvim-telescope/telescope.nvim",
    cmd = "Telescope",
    keys = { "<leader>f" },
    module_pattern = "telescope.*",
    requires = {
      "nvim-lua/plenary.nvim",
    },
    config = function()
      require("plugins.telescope").setup()
    end,
  }
  use {
    "ahmedkhalf/project.nvim",
    config = function()
      require("project_nvim").setup()
    end,
  }

  -- LSP
  use {
    "neovim/nvim-lspconfig",
    -- NOTE: Use nvim-lsp-installer to setup better
    -- config = function()
    --   require("plugins.lsp").setup()
    -- end,
    requires = {
      {
        "williamboman/nvim-lsp-installer",
        config = function()
          require("plugins.lsp.installer").setup()
        end,
      },
    },
  }
  use {
    "jose-elias-alvarez/null-ls.nvim",
    config = function()
      require("plugins.lsp.null-ls").setup()
    end,
  }

  -- Completion
  use {
    "hrsh7th/nvim-cmp",
    module = "cmp",
    event = "InsertEnter",
    requires = {
      { "L3MON4D3/LuaSnip" },
      { "rafamadriz/friendly-snippets" },
      { "saadparwaiz1/cmp_luasnip" },
      { "hrsh7th/cmp-nvim-lua" },
      { "hrsh7th/cmp-nvim-lsp" },
      { "hrsh7th/cmp-nvim-lsp-document-symbol", after = "nvim-cmp" },
      { "hrsh7th/cmp-cmdline", after = "nvim-cmp" },
      -- { 'f3fora/cmp-spell', after = 'nvim-cmp' },
      { "hrsh7th/cmp-path", after = "nvim-cmp" },
      { "hrsh7th/cmp-buffer", after = "nvim-cmp" },
      -- { 'tzachar/cmp-tabnine', run = './install.sh', after = 'nvim-cmp' },
    },
    config = function()
      require("plugins.cmp").setup()
    end,
  }

  use {
    "windwp/nvim-autopairs",
    after = "nvim-cmp",
    config = function()
      require("plugins.autopairs").setup()
    end,
  }

  -- Treesitter
  use {
    "nvim-treesitter/nvim-treesitter",
    run = ":TSUpdate",
    -- event = 'BufReadPre',
    event = "BufRead",
    config = function()
      require("plugins.treesitter").setup()
    end,
  }

  -- Colors Scheme
  -- use { "sainnhe/gruvbox-material" }

  -- Git
  use {
    "lewis6991/gitsigns.nvim",
    requires = {
      "nvim-lua/plenary.nvim",
      "kyazdani42/nvim-web-devicons",
    },
    event = {
      "VimEnter",
      "BufRead",
      "BufNewFile",
    },
    config = function()
      -- require("plugins.gitsigns").setup()
      require("gitsigns").setup()
    end,
  }

  -- WhichKey
  use {
    "folke/which-key.nvim",
    config = function()
      require("plugins.whichkey").setup()
    end,
  }

  -- QOL
  use {
    'numToStr/Comment.nvim',
    event = {
      "BufRead",
      "BufNewFile",
    },
    config = function()
      require('Comment').setup()
    end,
  }

  use {
    "folke/todo-comments.nvim",
    requires = "nvim-telescope/telescope.nvim",
    config = function()
      require("plugins.todocomments").setup()
    end,
  }

  use { "norcalli/nvim-colorizer.lua", cmd = "ColorizerToggle" }

  use {
    "akinsho/toggleterm.nvim",
    config = function()
      require("toggleterm").setup()
    end,
  }
end)

local map = require("utils.mappings").map

map("n", "<leader>pc", "<Cmd>PackerCompile<Cr>")
map("n", "<leader>ps", "<Cmd>PackerStatus<Cr>")

local ok, wk = pcall(require, "which-key")
if ok then
  wk.register {
    ["<leader>p"] = {
      name = "+Packer",
      c = "Compile",
      s = "Status",
    },
  }
end
