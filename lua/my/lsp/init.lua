local status_ok, _ = pcall(require, "lspconfig")
if not status_ok then
  return
end

-- require("my.lsp.lsp_handlers").setup() -- note that you should use lsp_installer to setup instead
require("my.lsp.mason").setup()
require("my.lsp.null-ls").setup()
