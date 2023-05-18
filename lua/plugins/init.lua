local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system {
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  }
end
vim.opt.rtp:prepend(lazypath)

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
        require("mini.surround").setup {
          mappings = {
            add = "<C-s>a", -- Add surrounding in Normal and Visual modes
            delete = "<C-s>d", -- Delete surrounding
            find = "<C-s>f", -- Find surrounding (to the right)
            find_left = "<C-s>F", -- Find surrounding (to the left)
            highlight = "<C-s>h", -- Highlight surrounding
            replace = "<C-s>r", -- Replace surrounding
            update_n_lines = "<C-s>n", -- Update `n_lines`

            suffix_last = "l", -- Suffix to search with "prev" method
            suffix_next = "n", -- Suffix to search with "next" method
          },
        }
      end,
    },

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

    { "norcalli/nvim-colorizer.lua", cmd = "ColorizerToggle" },

    -- { "jbyuki/nabla.nvim" },

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
    (function()
      local plugin_path = vim.fn.finddir("zk-nvim", vim.env.HOME .. "/.local/src/nvim-plugins")
      if plugin_path then
        return { dir = plugin_path }
      end
      return { "mickael-menu/zk-nvim" }
    end)(),
    {
      "jakewvincent/mkdnflow.nvim",
      ft = { "markdown" },
      config = function()
        require "plugins.mkdnflow"
      end,
    },

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
