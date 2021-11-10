local M = {}

local lsp_installer = require 'nvim-lsp-installer'

function M.setup()

  -- TODO: make it global variable
  local config = { lsp = {} }
  config.lsp.servers = {
    sumneko_lua = function()
      return {
        settings = {
          Lua = {
            diagnostics = {
              -- Get the language server to recognize the `vim` global
              globals = { 'vim' },
            },
            workspace = {
              -- Make the server aware of Neovim runtime files
              library = {
                [vim.fn.expand('$VIMRUNTIME/lua')] = true,
                [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true,
              },
              maxPreload = 10000,
            },
          },
        },
      }
    end,
    jsonls = function()
      return {
        settings = {
          json = {
            -- Schemas https://www.schemastore.org
            schemas = {
              {
                fileMatch = { 'package.json' },
                url = 'https://json.schemastore.org/package.json',
              },
              {
                fileMatch = { 'tsconfig*.json' },
                url = 'https://json.schemastore.org/tsconfig.json',
              },
              {
                fileMatch = {
                  '.prettierrc',
                  '.prettierrc.json',
                  'prettier.config.json',
                },
                url = 'https://json.schemastore.org/prettierrc.json',
              },
              {
                fileMatch = { '.eslintrc', '.eslintrc.json' },
                url = 'https://json.schemastore.org/eslintrc.json',
              },
              {
                fileMatch = { '.babelrc', '.babelrc.json', 'babel.config.json' },
                url = 'https://json.schemastore.org/babelrc.json',
              },
              {
                fileMatch = { 'lerna.json' },
                url = 'https://json.schemastore.org/lerna.json',
              },
              {
                fileMatch = { 'now.json', 'vercel.json' },
                url = 'https://json.schemastore.org/now.json',
              },
              {
                fileMatch = {
                  '.stylelintrc',
                  '.stylelintrc.json',
                  'stylelint.config.json',
                },
                url = 'http://json.schemastore.org/stylelintrc.json',
              },
            },
          },
        },
      }
    end,
  }

  local lsp_installer_servers = require 'nvim-lsp-installer.servers'
  for name, _ in pairs(config.lsp.servers) do
    ---@type boolean, table|string
    local ok, server = lsp_installer_servers.get_server(name)
    if ok then
      if not server:is_installed() then
        server:install()
      end
    end
  end

  function get_server_config(server)
    local conf = config.lsp.servers[server.name]
    local conf_type = type(conf)
    local opts = conf_type == 'table' and conf or conf_type == 'function' and conf() or {}

    opts.on_attach = require("plugins.lsp").on_attach

    opts.flags = { debounce_text_changes = 150 }

    opts.root_dir = function(fname)
      local util = require('lspconfig').util
      return util.root_pattern('.git')(fname)
        or util.root_pattern('tsconfig.base.json')(fname)
        or util.root_pattern('package.json')(fname)
        or util.root_pattern('.eslintrc.js')(fname)
        or util.root_pattern('tsconfig.json')(fname)
    end

    local nvim_lsp_ok, cmp_nvim_lsp = pcall(require, 'cmp_nvim_lsp')
    if nvim_lsp_ok then
      opts.capabilities = cmp_nvim_lsp.update_capabilities(vim.lsp.protocol.make_client_capabilities())
    end

    return opts
  end

  lsp_installer.on_server_ready(function(server)
    server:setup(get_server_config(server))
    vim.cmd [[ do User LspAttachBuffers ]]
  end)
end

return M
