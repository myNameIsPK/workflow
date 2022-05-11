local M = {}

local function lsp_keymaps(bufnr)
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  -- Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  local map = require("utils.mappings").map
  local lsp = vim.lsp.buf

  map('n', 'gD', function() lsp.declaration() end, { desc = "Go to declaration"})
  map('n', 'gd', function() lsp.definition() end, { desc = "Go to definition"})
  map('n', 'gr', function() lsp.references() end, { desc = "List references" })
  map('n', 'gi', function() lsp.implementation() end, { desc = "Go to implementation"})
  map('n', 'K', function() lsp.hover() end, { desc = "Hover" })
  map('n', '<C-k>', function() lsp.signature_help() end, { desc = "Signature help" })
  map('n', '<localleader>wa', function() lsp.add_workspace_folder() end, { desc = "Workspace Add folder" })
  map('n', '<localleader>wr', function() lsp.remove_workspace_folder() end, { desc = "Workspace Delete folder" })
  map('n', '<localleader>wl', function() print(vim.inspect(lsp.list_workspace_folders())) end, { desc = "Workspace list folder" })
  map('n', '<localleader>d', function() lsp.type_definition() end, { desc = "Type definition" })
  map('n', '<localleader>rn', function() lsp.rename() end, { desc = "Rename" })
  map('n', '<localleader>ca', function() lsp.code_action() end, { desc = "Code action" })
  map('v', '<localleader>ca', function() lsp.range_code_action() end, { desc = "Code action" })
  map('n', '<localleader>F', function() lsp.formatting() end, { desc = "formatting" })
  -- FIXME: range format not work
  map('v', '<localleader>F', function() lsp.range_formatting() end, { desc = "formatting" })

  -- telescope
  local ts = require("telescope.builtin")
  map('n', '<localleader>ld', function() ts.lsp_definitions() end, { desc = "telescope definition" })
  map('n', '<localleader>lt', function() ts.lsp_type_definitions() end, { desc = "telescope type definition" })
  map('n', '<localleader>li', function() ts.lsp_implementations() end, { desc = "telescope implementation" })
  map('n', '<localleader>lr', function() ts.lsp_references() end, { desc = "telescope references" })
  map('n', '<localleader>ls', function() ts.lsp_document_symbols() end, { desc = "telescope document symbols" })
  map('n', '<localleader>lS', function() ts.lsp_workspace_symbols() end, { desc = "telescope workspace symbols" })
end

local function lsp_highlight_document(client)
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
