local M = {}

function M.setup()

  -- define signs from _G.my.diagnostic_signs
  for _, sign in ipairs(my.diagnostic_signs) do
    vim.fn.sign_define(sign.name, { texthl = sign.name, text = sign.text, numhl = "" })
  end

  local config = {
    virtual_text = true,
    signs = {
      active = my.diagnostic_signs
    },
    update_in_insert = true,
    underline = true,
    severity_sort = true,
    float = {
      focusable = false,
      style = "minimal",
      border = "rounded",
      source = "always",
      header = "",
      prefix = "",
    },
  }

  vim.diagnostic.config(config)

  -- See `:help vim.diagnostic.*` for documentation on any of the below functions
  local opts = { noremap=true, silent=true }
  vim.api.nvim_set_keymap('n', '<leader>de', '<cmd>lua vim.diagnostic.open_float()<CR>', opts)
  vim.api.nvim_set_keymap('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
  vim.api.nvim_set_keymap('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
  vim.api.nvim_set_keymap('n', '<leader>dq', '<cmd>lua vim.diagnostic.setloclist()<CR>', opts)

  -- telescope TODO: make this safer
  vim.api.nvim_set_keymap('n', '<leader>fdb', '<cmd>Telescope diagnostics bufnr=0<cr>', opts)
  vim.api.nvim_set_keymap('n', '<leader>fdw', '<cmd>Telescope diagnostics<cr>', opts)
end

return M
