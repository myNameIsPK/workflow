local M = {}

function M.setup()
  local mason_lspconfig_ok, mason_lspconfig = pcall(require, "mason-lspconfig")
  if not mason_lspconfig_ok then
    return
  end
  mason_lspconfig.setup {
    ensure_installed = { "lua_ls" },
  }

  local handlers = require "my.lsp.handlers"
  for _, server in ipairs(mason_lspconfig.get_installed_servers()) do
    -- local opts = get_server_config(server)
    local opts = {
      on_attach = handlers.on_attach,
      -- capabilities = handlers.capabilities,
      flags = { debounce_text_changes = 150 }, -- this make lsp not reload immediately everytime while you typing the words
      root_dir = handlers.root_dir,
    }

    -- Add personal server configs
    local config_exist, config = pcall(require, "my.lsp.configs." .. server)
    if config_exist then
      local conf_type = type(config)
      local server_opts
      if conf_type == "table" then
        server_opts = config
      elseif conf_type == "function" then
        server_opts = config()
      end
      opts = vim.tbl_deep_extend("force", server_opts, opts)
    end

    -- This is the same as lspconfig's setup function.
    local lspconfig_ok, lspconfig = pcall(require, "lspconfig")
    if not lspconfig_ok then
      return
    end

    lspconfig[server].setup(opts)
  end
end

return M
