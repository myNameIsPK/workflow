local M = {}

function M.setup()
  local packer = require "packer"

  packer.startup(function(use)

    -- Change path for "impatient"
    packer.init({
      compile_path = vim.fn.stdpath("config").."/lua/packer_compiled.lua"
    })

    -- Packer can manage itself as an optional plugin
    use { "wbthomason/packer.nvim", opt = true, }

    -- Performance
    use { "lewis6991/impatient.nvim" }

    -- Colors Scheme
    use { "sainnhe/gruvbox-material" }

    -- LSP
    use { "neovim/nvim-lspconfig" }
    -- TODO: change this to nvim-lsp-install
    use { "kabouzeid/nvim-lspinstall" }
    use { "tamago324/nlsp-settings.nvim" } -- use to config LSP using Json
    use { "antoinemadec/FixCursorHold.nvim" } -- TODO: delete in the future
    use { "jose-elias-alvarez/null-ls.nvim" } -- formatters, linters

    -- Treesitter
    use {
      "nvim-treesitter/nvim-treesitter",
      run = ":TSUpdate",
      config = function()
        require("config.treesitter").setup()
      end,
    }

    -- Telescope fuzzy finder
    use { "nvim-lua/plenary.nvim" }
    use {
      "nvim-telescope/telescope.nvim",
      config = function()
        require("config.telescope").setup()
      end,
    }
    use {
      "ahmedkhalf/project.nvim",
      config = function()
        require("project_nvim").setup {}
      end
    }

    -- Completion
    use {
      "hrsh7th/nvim-cmp",
      requires = {
        "L3MON4D3/LuaSnip",
        "saadparwaiz1/cmp_luasnip",
        "rafamadriz/friendly-snippets",
        "hrsh7th/cmp-buffer",
        "hrsh7th/cmp-path",
        "hrsh7th/cmp-nvim-lsp",
        "hrsh7th/cmp-nvim-lua",
        "hrsh7th/cmp-calc",
        "f3fora/cmp-spell",
        -- "hrsh7th/cmp-emoji",
        -- "ray-x/cmp-treesitter",
      },
      config = function()
        require("config.cmp").setup()
      end,
    }

    -- Git
    use {
      'lewis6991/gitsigns.nvim',
      opt = true,
      requires = {
        'nvim-lua/plenary.nvim'
      },
      event = "BufRead",
      config = function()
        require('config.gitsigns').setup()
      end,
    }

    -- QOL
    use {
      "terrortylor/nvim-comment",
      event = "BufRead",
      config = function()
        require("config.nvim_comment").setup()
      end,
    }
    use {
      "windwp/nvim-autopairs",
      config = function()
        require("nvim-autopairs").setup {}
      end,
    }
    use { "norcalli/nvim-colorizer.lua" }
    use {
      "famiu/feline.nvim",
      require = {
        "kyazdani42/nvim-web-devicons"
      },
      config = function ()
        require("config.feline").setup()
        -- require("feline").setup({
        --   preset = "noicon"
        -- })
      end,
    }
    use "kyazdani42/nvim-web-devicons"

    -- Note Taking
    use {
      "nvim-neorg/neorg",
      ft = "norg",
      config = function()
        require("config.neorg").setup()
      end,
      requires = {
        "nvim-lua/plenary.nvim",
        "vhyrro/neorg-telescope",
      }
    }
  end)
end

return M
