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

    -- Performance
    -- use { "dstein64/vim-startuptime" } -- use impatient LuaCacheProfile instead
    use {
      "lewis6991/impatient.nvim",
      config = function()
        require("impatient").enable_profile() -- enable LuaCacheProfile
      end,
    }

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
        {
          "williamboman/nvim-lsp-installer",
          config = function()
            require("plugins.lsp").setup()
          end,
        },
        { "folke/lua-dev.nvim" },
      },
    }
    use { "jose-elias-alvarez/null-ls.nvim" }

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
        { "hrsh7th/cmp-cmdline", after = "nvim-cmp" },
        { "f3fora/cmp-spell", after = "nvim-cmp" },
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
      -- run = ":TSUpdate",
      requires = {
        { "nvim-treesitter/playground", after = "nvim-treesitter" },
        { "nvim-treesitter/nvim-treesitter-textobjects", after = "nvim-treesitter" },
        { "JoosepAlviste/nvim-ts-context-commentstring", after = "nvim-treesitter" },
      },
      config = function()
        require("plugins.treesitter").setup()
      end,
    }

    use {
      "lewis6991/spellsitter.nvim",
      config = function()
        require("spellsitter").setup()
      end,
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
        require("plugins.gitsigns").setup()
      end,
    }

    use {
      "TimUntersberger/neogit",
      cmd = "Neogit",
      requires = {
        "nvim-lua/plenary.nvim",
        "sindrets/diffview.nvim",
      },
      config = function()
        require("plugins.neogit").setup()
      end,
    }

    -- Notes taking
    -- use {
    --   "renerocksai/telekasten.nvim",
    --   disable = true,
    --   keys = { "<leader>z" },
    --   config = function()
    --     require("plugins.telekasten").setup()
    --   end,
    -- }

    use {
      "mickael-menu/zk-nvim",
      config = function()
        require("plugins.zk").setup()
      end,
    }

    use {
      "nvim-orgmode/orgmode",
      ft = { "org" },
      config = function()
        require("plugins.orgmode").setup()
      end,
    }
    use {
      "nvim-neorg/neorg",
      -- tag = "latest",
      -- ft = { "norg" },
      -- after = "nvim-treesitter",
      config = function()
        require("plugins.neorg").setup()
      end,
      requires = {
        "nvim_lua/plenary.nvim",
        "nvim-telescope/telescope.nvim",
        "nvim-neorg/neorg-telescope",
      },
    }

    -- QOL

    use {
      "folke/which-key.nvim",
      config = function()
        require("plugins.whichkey").setup()
      end,
    }

    use {
      "numToStr/Comment.nvim",
      event = {
        "BufRead",
        "BufNewFile",
      },
      config = function()
        require("plugins.comment").setup()
      end,
    }

    use {
      "akinsho/toggleterm.nvim",
      config = function()
        require("toggleterm").setup()
      end,
    }

    use {
      "goolord/alpha-nvim",
      config = function()
        require("plugins.alpha").setup()
      end,
    }

    use { "machakann/vim-sandwich" }

    -- 3rd Party
    use {
      "glacambre/firenvim",
      run = function()
        vim.fn["firenvim#install"](1)
      end,
    }

    use {
      'iamcco/markdown-preview.nvim',
      run = function() vim.fn['mkdp#util#install']() end,
      ft = {'markdown'}
    }

    -- Beutiful UI
    use { "norcalli/nvim-colorizer.lua", cmd = "ColorizerToggle" }

    -- use {
    --   "folke/todo-comments.nvim",
    --   requires = "nvim-telescope/telescope.nvim",
    --   config = function()
    --     require("plugins.todocomments").setup()
    --   end,
    -- }
  end,
}
