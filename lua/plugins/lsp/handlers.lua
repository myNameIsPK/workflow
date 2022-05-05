local M = {}

local function lsp_keymaps(bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  -- Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  local opts = { noremap=true, silent=true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<leader>lwa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<leader>lwr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<leader>lwl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<leader>lD', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<leader>lrn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<leader>lca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', '<leader>lf', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)

  -- telescope TODO: make this safer
  buf_set_keymap('n', '<leader>lgd', '<cmd>lua require("telescope.builtin").lsp_definitions()<cr>', opts)
  buf_set_keymap('n', '<leader>lgt', '<cmd>lua require("telescope.builtin").lsp_type_definitions()<cr>', opts)
  buf_set_keymap('n', '<leader>lgi', '<cmd>lua require("telescope.builtin").lsp_implementations()<cr>', opts)
  buf_set_keymap('n', '<leader>lgr', '<cmd>lua require("telescope.builtin").lsp_references()<cr>', opts)
end

-- TODO: understand this
-- FIX: this keep send error when stop lsp servers
local function lsp_highlight_document(client)
  -- Set autocommands conditional on server_capabilities
  if client.server_capabilities.document_highlight then
    vim.cmd [[
      augroup lsp_document_highlight
        autocmd! * <buffer>
        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
      augroup END
    ]]
  end
end

function M.on_attach(client, bufnr)
  lsp_keymaps(bufnr)
  lsp_highlight_document(client)
end

function M.root_dir(fname)
  local root_pattern = require('lspconfig.util').root_pattern
  return root_pattern('.git')(fname)
    or root_pattern('tsconfig.base.json')(fname)
    or root_pattern('package.json')(fname)
    or root_pattern('.eslintrc.js')(fname)
    or root_pattern('tsconfig.json')(fname)
end

local capabilities = vim.lsp.protocol.make_client_capabilities()
local nvim_lsp_ok, cmp_nvim_lsp = pcall(require, 'cmp_nvim_lsp')
if not nvim_lsp_ok then
  return
end
M.capabilities = cmp_nvim_lsp.update_capabilities(capabilities)

return M
