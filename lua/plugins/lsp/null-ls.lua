local M = {}

function M.setup()
  local null_ls_status_ok, null_ls = pcall(require, "null-ls")
  if not null_ls_status_ok then
    return
  end

  -- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/formatting
  local formatting = null_ls.builtins.formatting
  -- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/diagnostics
  local diagnostics = null_ls.builtins.diagnostics
  -- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/code_actions
  local code_actions = null_ls.builtins.code_actions

  local handlers = require("plugins.lsp.handlers")

  null_ls.setup {
    sources = {
      formatting.prettier.with { extra_args = { "--no-semi", "--single-quote", "--jsx-single-quote" } },
      formatting.black.with { extra_args = { "--fast" } },
      formatting.stylua,
      -- diagnostics.flake8
      formatting.latexindent,
      diagnostics.shellcheck,
      code_actions.shellcheck,
      -- code_actions.gitsigns, -- gitsigns plugins integration
    },
    debug = false,
    on_attach = handlers.on_attach,
    capabilities = handlers.capabilities,
    flags = { debounce_text_changes = 150 }, -- this make lsp not reload immediately everytime while you typing the words
    root_dir = handlers.root_dir,
  }

end

return M
