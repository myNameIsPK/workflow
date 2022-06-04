local M = {}

local function lsp_keymaps(bufnr)
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end

  -- Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  buf_set_keymap('n', 'gD', "<Cmd>lua vim.lsp.buf.declaration()<CR>", { desc = "Go to declaration"})
  buf_set_keymap('n', '<C-]>', "<Cmd>lua vim.lsp.buf.definition()<CR>", { desc = "Go to definition"})
  buf_set_keymap('n', 'gr', "<Cmd>lua vim.lsp.buf.references()<CR>", { desc = "List references" })
  buf_set_keymap('n', 'gi', "<Cmd>lua vim.lsp.buf.implementation()<CR>", { desc = "Go to implementation"})
  buf_set_keymap('n', 'K', "<Cmd>lua vim.lsp.buf.hover()<CR>", { desc = "Hover" })
  buf_set_keymap('n', '<C-k>', "<Cmd>lua vim.lsp.buf.signature_help()<CR>", { desc = "Signature help" })
  buf_set_keymap('n', '<localleader>wa', "<Cmd>lua vim.lsp.buf.add_workspace_folder()<CR>", { desc = "Workspace Add folder" })
  buf_set_keymap('n', '<localleader>wr', "<Cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>", { desc = "Workspace Delete folder" })
  buf_set_keymap('n', '<localleader>wl', "<Cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>", { desc = "Workspace list folder" })
  buf_set_keymap('n', '<localleader>d', "<Cmd>lua vim.lsp.buf.type_definition()<CR>", { desc = "Type definition" })
  buf_set_keymap('n', '<localleader>rn', "<Cmd>lua vim.lsp.buf.rename()<CR>", { desc = "Rename" })
  buf_set_keymap('n', '<localleader>ca', "<Cmd>lua vim.lsp.buf.code_action()<CR>", { desc = "Code action" })
  buf_set_keymap('v', '<localleader>ca', "<Cmd>lua vim.lsp.buf.range_code_action()<CR>", { desc = "Code action" })
  buf_set_keymap('n', '<localleader>F', "<Cmd>lua vim.lsp.buf.formatting()<CR>", { desc = "formatting" })
  -- FIXME: range format not work
  -- buf_set_keymap('v', '<localleader>F', "<Cmd>lua vim.lsp.buf.range_formatting()<CR>", { desc = "formatting" })

  -- telescope
  buf_set_keymap('n', '<localleader>ld', "<Cmd>lua require('telescope.builtin').vim.lsp.buf_definitions()<CR>", { desc = "telescope definition" })
  buf_set_keymap('n', '<localleader>lt', "<Cmd>lua require('telescope.builtin').vim.lsp.buf_type_definitions()<CR>", { desc = "telescope type definition" })
  buf_set_keymap('n', '<localleader>li', "<Cmd>lua require('telescope.builtin').vim.lsp.buf_implementations()<CR>", { desc = "telescope implementation" })
  buf_set_keymap('n', '<localleader>lr', "<Cmd>lua require('telescope.builtin').vim.lsp.buf_references()<CR>", { desc = "telescope references" })
  buf_set_keymap('n', '<localleader>ls', "<Cmd>lua require('telescope.builtin').vim.lsp.buf_document_symbols()<CR>", { desc = "telescope document symbols" })
  buf_set_keymap('n', '<localleader>lS', "<Cmd>lua require('telescope.builtin').vim.lsp.buf_workspace_symbols()<CR>", { desc = "telescope workspace symbols" })
end

local function lsp_highlight_document(client)
  if client.server_capabilities.document_highlight then
    vim.cmd [[
      augroup lsp_document_highlight
        autocmd! * <buffer>
        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorHoldI <buffer> lua vim.lsp.buf.document_highlight()
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
