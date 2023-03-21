require("utils.plugins").bootstrap_lazy()

local plugins = {
  always = {
    -- { "nvim-lua/popup.nvim" },
    { "nvim-lua/plenary.nvim" },

    { "nvim-telescope/telescope.nvim" },
    { "nvim-telescope/telescope-ui-select.nvim" },

    -- { "ibhagwan/fzf-lua" },

    { "numToStr/Comment.nvim" },

    {
      "hrsh7th/nvim-cmp",
      dependencies = {
        { "L3MON4D3/LuaSnip" },
        { "rafamadriz/friendly-snippets" },
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

    { "windwp/nvim-autopairs" },
  },
  perf = {
    { "antoinemadec/FixCursorHold.nvim" },

    -- { "dstein64/vim-startuptime" } -- use impatient LuaCacheProfile instead

    {
      "lewis6991/impatient.nvim",
      lazy = false,
      config = function()
        require("impatient").enable_profile() -- enable LuaCacheProfile
      end,
    },

    { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
  },
  qol = {
    {
      "ahmedkhalf/project.nvim",
      config = function()
        require("project_nvim").setup()
      end,
    },

    { "folke/which-key.nvim" },

    -- {
    --   "anuvyklack/hydra.nvim",
    --   dependencies = "anuvyklack/keymap-layer.nvim", -- needed only for pink hydras
    -- },

    {
      "akinsho/toggleterm.nvim",
      config = function()
        require("toggleterm").setup()
      end,
    },

    {
      "echasnovski/mini.nvim",
      lazy = false,
      config = function()
        require("mini.align").setup()
        require("mini.surround").setup()
      end,
    },

    -- {
    --   "machakann/vim-sandwich",
    --   lazy = false,
    -- },

    -- {
    --   "junegunn/vim-easy-align",
    --   lazy = false,
    --   config = function()
    --     vim.cmd [[
    --       xmap ga <Plug>(EasyAlign)
    --       nmap ga <Plug>(EasyAlign)
    --       ]]
    --   end,
    -- },

    {
      "ggandor/leap.nvim",
      dependencies = "tpope/vim-repeat",
      lazy = false,
      init = function()
        require("leap").add_default_mappings()
      end,
    },
  },
  ui = {
    { "kyazdani42/nvim-web-devicons" },

    {
      "j-hui/fidget.nvim",
      lazy = false,
      config = true,
    },

    -- {
    --   "stevearc/aerial.nvim",
    --   cmd = "AerialToggle",
    --   config = function()
    --     require("aerial").setup {
    --       filter_kind = false,
    --     }
    --   end,
    -- },

    -- { "lukas-reineke/indent-blankline.nvim" },

    { "norcalli/nvim-colorizer.lua", cmd = "ColorizerToggle" },

    -- { "jbyuki/nabla.nvim" },

    -- { "folke/todo-comments.nvim" },

    -- {
    --   "glacambre/firenvim",
    --   build = function()
    --     vim.fn["firenvim#install"](1)
    --   end,
    -- },

    -- { "goolord/alpha-nvim" },

    {
      "iamcco/markdown-preview.nvim",
      build = function()
        vim.fn["mkdp#util#install"]()
      end,
      ft = { "markdown" },
    },
  },
  color = {
    {
      "sainnhe/gruvbox-material",
      lazy = false,
      priority = 1000,
      config = function()
        vim.g.gruvbox_material_visual = "reverse"
        vim.g.gruvbox_material_background = "soft"
        vim.g.gruvbox_material_disable_italic_comment = 0
        vim.g.gruvbox_material_palette = "original"
        vim.cmd "colorscheme gruvbox-material"
      end,
    },
  },
  git = {
    { "lewis6991/gitsigns.nvim" },
    { "TimUntersberger/neogit" },
    -- { "sindrets/diffview.nvim" },
  },
  lsp = {
    {
      "neovim/nvim-lspconfig",
      dependencies = {
        { "williamboman/mason.nvim" },
        { "williamboman/mason-lspconfig.nvim" },
        { "folke/neodev.nvim" },
      },
    },
  },
  code = {
    { "jose-elias-alvarez/null-ls.nvim" },

    -- {
    --   "danymat/neogen",
    --   dependencies = "nvim-treesitter/nvim-treesitter",
    -- },

    -- TODO: neotest or vimtest

    -- { "mfussenegger/nvim-dap" },
  },
  treesitter = {
    {
      "nvim-treesitter/nvim-treesitter",
      dependencies = {
        { "nvim-treesitter/playground" },
        { "nvim-treesitter/nvim-treesitter-textobjects" },
        { "JoosepAlviste/nvim-ts-context-commentstring" },
      },
    },
  },
  lang = {
    -- Json
    { "b0o/SchemaStore.nvim" },
  },
  note = {
    -- Notes taking
    -- { "renerocksai/telekasten.nvim" },

    { "mickael-menu/zk-nvim" },

    -- { "nvim-orgmode/orgmode" },

    -- { "nvim-neorg/neorg", dependencies = "nvim-neorg/neorg-telescope" },
  },
}

local lazy_plugins = {}
for _, plugin in pairs(plugins) do
  table.insert(lazy_plugins, plugin)
end

local lazy_opts = {
  defaults = {
    lazy = true,
  },
  dev = {
    path = "~/Projects",
  },
}

require("lazy").setup(lazy_plugins, lazy_opts)
