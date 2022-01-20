local M = {}

function M.setup()

  -- require("plugins.lsp.lsp_handlers").setup() -- NOTE: use lsp_installer to setup instead
  require("plugins.lsp.installer").setup()

end

return M
