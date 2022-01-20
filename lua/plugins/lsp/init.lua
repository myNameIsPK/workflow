local M = {}

function M.setup()
  local status_ok, _ = pcall(require, "lspconfig")
  if not status_ok then
    return
  end

  -- require("plugins.lsp.lsp_handlers").setup() -- NOTE: use lsp_installer to setup instead
  require("plugins.lsp.installer").setup()
  require("plugins.lsp.null-ls").setup()
  require("plugins.lsp.diagnostic").setup()
end

return M
