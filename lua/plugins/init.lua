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
    end
  }
  use {
    "ahmedkhalf/project.nvim",
    config = function()
      require("project_nvim").setup() 
    end
  }

  -- Completion
  use {
    'hrsh7th/nvim-cmp',
    module = 'cmp',
    event = 'InsertEnter',
    requires = {
      -- { 'hrsh7th/cmp-nvim-lsp' },
      -- { 'hrsh7th/cmp-nvim-lsp-document-symbol', after = 'nvim-cmp' },
      { 'hrsh7th/cmp-cmdline', after = 'nvim-cmp' },
      -- { 'f3fora/cmp-spell', after = 'nvim-cmp' },
      { 'hrsh7th/cmp-path', after = 'nvim-cmp' },
      { 'hrsh7th/cmp-buffer', after = 'nvim-cmp' },
      -- { 'saadparwaiz1/cmp_luasnip', after = 'nvim-cmp' },
      -- { 'tzachar/cmp-tabnine', run = './install.sh', after = 'nvim-cmp' },
    },
    config = function()
      require("plugins.cmp").setup()
    end
  }

  -- Treesitter
  use {
    "nvim-treesitter/nvim-treesitter",
    run = ":TSUpdate",
    -- event = 'BufReadPre',
    event = "BufRead",
    config = function()
      require("plugins.treesitter").setup()
    end
  }


  -- Colors Scheme
  use { "sainnhe/gruvbox-material" }

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
    end
  }

  -- QOL
  use {
    "terrortylor/nvim-comment",
    event = {
      "BufRead",
      "BufNewFile"
    },
    config = function()
      require("nvim_comment").setup()
    end,
  }
  -- use {
  --   "windwp/nvim-autopairs",
  --   config = function()
  --     require("nvim-autopairs").setup {}
  --   end,
  -- }
  use { "norcalli/nvim-colorizer.lua",
    cmd = "ColorizerToggle",
  }

end)

local map = require("utils.mappings").map

map("n", "<leader>pc", "<Cmd>PackerCompile<Cr>")
map("n", "<leader>ps", "<Cmd>PackerStatus<Cr>")

local ok, wk = pcall(require, "which-key")
if ok then
  wk.register({
    ["<leader>p"] = {
      name = "+Packer",
      c = "Compile",
      s = "Status",
    }
  })
end
