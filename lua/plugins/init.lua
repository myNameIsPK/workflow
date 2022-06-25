local utils = require "utils.plugins"

utils.bootstrap_packer()

require("packer").startup {
  config = {
    -- Move to lua dir so impatient.nvim can cache it
    compile_path = vim.fn.stdpath "config" .. "/lua/packer_compiled.lua",
  },
  function(use)
    -- Packer can manage itself as an optional plugin
    use { "wbthomason/packer.nvim" }
    use { "nvim-lua/popup.nvim" }
    use { "nvim-lua/plenary.nvim" }
    use { "kyazdani42/nvim-web-devicons" }
    use { "antoinemadec/FixCursorHold.nvim" }

    -- Performance
    -- use { "dstein64/vim-startuptime" } -- use impatient LuaCacheProfile instead
    use {
      "lewis6991/impatient.nvim",
      config = function()
        require("impatient").enable_profile() -- enable LuaCacheProfile
      end,
    }

    -- Telescope
    use { "nvim-telescope/telescope.nvim" }
    use { "nvim-telescope/telescope-fzf-native.nvim", run = "make" }
    use { "nvim-telescope/telescope-ui-select.nvim" }
    use {
      "ahmedkhalf/project.nvim",
      config = function()
        require("project_nvim").setup()
      end,
    }

    -- LSP
    use {
      "neovim/nvim-lspconfig",
      requires = {
        { "williamboman/nvim-lsp-installer" },
        { "folke/lua-dev.nvim" },
      },
    }
    use { "jose-elias-alvarez/null-ls.nvim" }

    use { "b0o/SchemaStore.nvim" }

    -- Completion
    use {
      "hrsh7th/nvim-cmp",
      -- module = "cmp",
      -- event = "InsertEnter",
      requires = {
        { "L3MON4D3/LuaSnip" },
        { "rafamadriz/friendly-snippets" },
        { "saadparwaiz1/cmp_luasnip" },
        -- { "hrsh7th/cmp-nvim-lua" }, -- lua-dev is better
        { "hrsh7th/cmp-nvim-lsp" },
        { "hrsh7th/cmp-nvim-lsp-document-symbol", after = "nvim-cmp" },
        { "hrsh7th/cmp-nvim-lsp-signature-help", after = "nvim-cmp" },
        { "hrsh7th/cmp-cmdline", after = "nvim-cmp" },
        { "f3fora/cmp-spell", after = "nvim-cmp" },
        { "hrsh7th/cmp-path", after = "nvim-cmp" },
        { "hrsh7th/cmp-buffer", after = "nvim-cmp" },
        -- { 'tzachar/cmp-tabnine', run = './install.sh', after = 'nvim-cmp' },
      },
    }

    use { "windwp/nvim-autopairs", after = "nvim-cmp" }

    -- Treesitter
    use {
      "nvim-treesitter/nvim-treesitter",
      requires = {
        { "nvim-treesitter/playground", after = "nvim-treesitter" },
        { "nvim-treesitter/nvim-treesitter-textobjects", after = "nvim-treesitter" },
        { "JoosepAlviste/nvim-ts-context-commentstring", after = "nvim-treesitter" },
      },
    }

    use { "lewis6991/spellsitter.nvim",
      config = function()
        require("spellsitter").setup()
      end,
    }

    -- DAP
    -- use { "mfussenegger/nvim-dap" }

    -- Colors Scheme
    use { "sainnhe/gruvbox-material" }

    -- Git
    use { "lewis6991/gitsigns.nvim" }
    use { "TimUntersberger/neogit" }
    use { "sindrets/diffview.nvim" }

    -- Notes taking
    -- use { "renerocksai/telekasten.nvim" }

    use { "mickael-menu/zk-nvim" }

    use { "nvim-orgmode/orgmode" }

    use { "nvim-neorg/neorg", requires = "nvim-neorg/neorg-telescope" }

    -- QOL

    use { "folke/which-key.nvim" }

    use { "numToStr/Comment.nvim" }

    use {
      "akinsho/toggleterm.nvim",
      config = function()
        require("toggleterm").setup()
      end,
    }

    use { "goolord/alpha-nvim" }

    use { "machakann/vim-sandwich" }

    use {
      "junegunn/vim-easy-align",
      config = function ()
        vim.cmd [[
          xmap ga <Plug>(EasyAlign)
          nmap ga <Plug>(EasyAlign)
        ]]
      end,
    }

    -- UI
    use {
      "j-hui/fidget.nvim",
      config = function ()
        require("fidget").setup()
      end
    }

    use { "lukas-reineke/indent-blankline.nvim" }

    use { "norcalli/nvim-colorizer.lua", cmd = "ColorizerToggle" }

    use { "jbyuki/nabla.nvim" }

    -- use { "folke/todo-comments.nvim" }

    -- 3rd Party
    -- use {
    --   "glacambre/firenvim",
    --   run = function()
    --     vim.fn["firenvim#install"](1)
    --   end,
    -- }

    use {
      'iamcco/markdown-preview.nvim',
      run = function() vim.fn['mkdp#util#install']() end,
      ft = {'markdown'}
    }

  end,
}
