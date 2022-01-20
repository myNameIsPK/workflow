local M = {}

function M.setup()
  -- require("plugins.lsp.lsp_handlers").setup() -- NOTE: use lsp_installer to setup instead
  require("plugins.lsp.installer").setup()
  require("plugins.lsp.null-ls").setup()
end

return M
