local M = {}

function M.setup()
  local config = {
    virtual_text = true,
    signs = true,
    update_in_insert = true,
    underline = true,
    severity_sort = true,
    float = {
      focusable = true,
      style = "minimal",
      border = "none",
      source = true,
      header = "",
      prefix = "",
    },
  }

  vim.diagnostic.config(config)
end

return M
