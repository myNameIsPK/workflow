local status_ok, _ = pcall(require, "lspconfig")
if not status_ok then
  return
end

-- require("plugins_config.lsp.lsp_handlers").setup() -- note that you should use lsp_installer to setup instead
require("plugins_config.lsp.diagnostic").setup()
require("plugins_config.lsp.mason").setup()
require("plugins_config.lsp.null-ls").setup()
