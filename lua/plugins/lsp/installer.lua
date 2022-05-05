local M = {}

function M.setup()
  local status_ok, lsp_installer = pcall(require, "nvim-lsp-installer")
  if not status_ok then
    return
  end

  lsp_installer.setup {
    -- ensure_installed = { "sumneko_lua", "jsonls" },
    ensure_installed = {},
    -- Whether servers that are set up (via lspconfig) should be automatically installed if they're not already installed.
    -- Can either be:
    --   - false: Servers are not automatically installed.
    --   - true: All servers set up via lspconfig are automatically installed.
    --   - { exclude: string[] }: All servers set up via lspconfig, except the ones provided in the list, are automatically installed.
    --       Example: automatic_installation = { exclude = { "rust_analyzer", "solargraph" } }
    automatic_installation = false,
    max_concurrent_installers = 4,
    log_level = vim.log.levels.INFO,
    ui = {
      icons = {
        server_installed = "◍",
        server_pending = "◍",
        server_uninstalled = "◍",
      },
      keymaps = {
        toggle_server_expand = "<CR>",
        install_server = "i",
        update_server = "u",
        check_server_version = "c",
        update_all_servers = "U",
        check_outdated_servers = "C",
        uninstall_server = "X",
      },
    },
  }


  local handlers = require("plugins.lsp.handlers")
  for _, server in ipairs(lsp_installer.get_installed_servers()) do
    -- local opts = get_server_config(server)
    local opts = {
      on_attach = handlers.on_attach,
      capabilities = handlers.capabilities,
      flags = { debounce_text_changes = 150 }, -- this make lsp not reload immediately everytime while you typing the words
      root_dir = handlers.root_dir,
    }

    -- Add personal server configs
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

      -- Add nvim api document plugin
      if server.name == "sumneko_lua" then
        local luadev_ok, luadev = pcall(require, "lua-dev")
        if not luadev_ok then
          return
        end
        opts = vim.tbl_deep_extend("force", opts, luadev.setup())
      end
    end

    -- This is the same as lspconfig's setup function.
    local lspconfig_ok, lspconfig = pcall(require, "lspconfig")
    if not lspconfig_ok then
      return
    end
    lspconfig[server.name].setup(opts)
  end
end

return M
