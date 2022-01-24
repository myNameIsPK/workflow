local M = {}

function M.setup()
  local status_ok, lsp_installer = pcall(require, "nvim-lsp-installer")
  if not status_ok then
    return
  end

  -- auto install server
  -- local servers = { "sumneko_lua", "jsonls" }
  -- Auto install servers if there is it config
  -- local lsp_installer_servers = require 'nvim-lsp-installer.servers'
  -- for name, _ in pairs(servers) do
  --   ---@type boolean, table|string
  --   local ok, server = lsp_installer_servers.get_server(name)
  --   if ok then
  --     if not server:is_installed() then
  --       server:install()
  --     end
  --   end
  -- end

  lsp_installer.on_server_ready(function(server)
    -- local opts = get_server_config(server)
    local opts = {
      on_attach = require("plugins.lsp.lsp_handlers").on_attach,
      capabilities = require("plugins.lsp.lsp_handlers").capabilities,
      flags = { debounce_text_changes = 150 }, -- this make lsp not reload immediately everytime while you typing the words
      root_dir = function(fname)
        local util = require('lspconfig').util
        return util.root_pattern('.git')(fname)
          or util.root_pattern('tsconfig.base.json')(fname)
          or util.root_pattern('package.json')(fname)
          or util.root_pattern('.eslintrc.js')(fname)
          or util.root_pattern('tsconfig.json')(fname)
      end,
    }

    local config_exist, config = pcall(require, "plugins.lsp.configs." .. server.name)
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
    server:setup(opts)
  end)
end

return M
