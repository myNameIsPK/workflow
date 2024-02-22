return {

  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      { "williamboman/mason.nvim" },
      { "williamboman/mason-lspconfig.nvim" },
    },
    config = function()
      local mason_lspconfig = require "mason-lspconfig"

      mason_lspconfig.setup {
        ensure_installed = { "lua_ls" },
      }

      for _, server in ipairs(mason_lspconfig.get_installed_servers()) do
        local opts = {
          flags = { debounce_text_changes = 150 },
        }

        -- Add personal servers configs
        local config_exist, config = pcall(require, "my.lsp.configs." .. server)
        if config_exist then
          local conf_type = type(config)
          local my_server_opts
          if conf_type == "table" then
            my_server_opts = config
          elseif conf_type == "function" then
            my_server_opts = config()
          end
          opts = vim.tbl_deep_extend("force", my_server_opts, opts)
        end

        require("lspconfig")[server].setup(opts)
      end
    end,
  },

  {
    "williamboman/mason.nvim",
    -- lazy = false,
    cmd = "Mason",
    config = function()
      -- `:h mason-setting`
      require("mason").setup()
    end,
  },

  { "williamboman/mason-lspconfig.nvim" },
}
