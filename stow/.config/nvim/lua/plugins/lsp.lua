return {

  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      {
        "williamboman/mason.nvim",
        cmd = "Mason",
        config = function()
          -- `:h mason-setting`
          require("mason").setup()
        end,
      },
      { "williamboman/mason-lspconfig.nvim" },
    },
    config = function()
      local mason_lspconfig = require "mason-lspconfig"

      mason_lspconfig.setup {
        ensure_installed = my.is_termux and {} or { "lua_ls" },
      }

      local server_list = mason_lspconfig.get_installed_servers()
      if vim.fn.executable "lua-language-server" == 1 then
        -- vim.tbl_deep_extend("force", server_list, { "lua_ls" })
        table.insert(server_list, "lua_ls")
      end

      for _, server in ipairs(server_list) do
        local opts = {
          autostart = false,
          -- capabilities = (function()
          --   local cmp_nvim_lsp_ok, cmp_nvim_lsp = pcall(require, "cmp_nvim_lsp")
          --   if cmp_nvim_lsp_ok then
          --     return cmp_nvim_lsp.default_capabilities()
          --   end
          --   return vim.lsp.protocol.make_client_capabilities()
          -- end)(),
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
}
