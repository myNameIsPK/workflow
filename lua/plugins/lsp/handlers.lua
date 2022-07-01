local M = {}

local function lsp_buf_options(bufnr, client_id)
  local client = vim.lsp.get_client_by_id(client_id)
  if client.server_capabilities.completionProvider then
    -- Enable completion triggered by <c-x><c-o>
    vim.bo[bufnr].omnifunc = "v:lua.vim.lsp.omnifunc"
  end
  if client.server_capabilities.definitionProvider then
    -- Enable tags command like <c-]> to use LSP
    vim.bo[bufnr].tagfunc = "v:lua.vim.lsp.tagfunc"
  end
end

local function lsp_keymaps(bufnr)
  local function nmap(mode, lhs, rhs, desc)
    vim.keymap.set(mode, lhs, rhs, { buffer = bufnr, desc = desc })
  end

  nmap('n', 'gD', "<Cmd>lua vim.lsp.buf.declaration()<CR>", "Go to declaration")
  -- nmap('n', 'gd', "<Cmd>lua vim.lsp.buf.definition()<CR>", "Go to definition") -- use <C-]> instead
  nmap('n', 'gr', "<Cmd>lua vim.lsp.buf.references()<CR>", "List references")
  nmap('n', 'gi', "<Cmd>lua vim.lsp.buf.implementation()<CR>", "Go to implementation")
  nmap('n', 'K', "<Cmd>lua vim.lsp.buf.hover()<CR>", "Hover")
  nmap('n', '<C-k>', "<Cmd>lua vim.lsp.buf.signature_help()<CR>", "Signature help")
  nmap('n', '<localleader>wa', "<Cmd>lua vim.lsp.buf.add_workspace_folder()<CR>", "Workspace Add folder")
  nmap('n', '<localleader>wr', "<Cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>", "Workspace Delete folder")
  nmap('n', '<localleader>wl', "<Cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>", "Workspace list folder")
  nmap('n', '<localleader>d', "<Cmd>lua vim.lsp.buf.type_definition()<CR>", "Type definition")
  nmap('n', '<localleader>rn', "<Cmd>lua vim.lsp.buf.rename()<CR>", "Rename")
  nmap('n', '<localleader>ca', "<Cmd>lua vim.lsp.buf.code_action()<CR>", "Code action")
  nmap('v', '<localleader>ca', "<Cmd>lua vim.lsp.buf.range_code_action()<CR>", "Code action")
  nmap('n', '<localleader>F', "<Cmd>lua vim.lsp.buf.formatting()<CR>", "formatting")
  -- FIXME: range format not work
  -- nmap('v', '<localleader>F', "<Cmd>lua vim.lsp.buf.range_formatting()<CR>", "formatting")

  -- telescope
  nmap('n', '<localleader>ld', "<Cmd>lua require('telescope.builtin').lsp_definitions()<CR>", "telescope definition")
  nmap('n', '<localleader>lt', "<Cmd>lua require('telescope.builtin').lsp_type_definitions()<CR>", "telescope type definition")
  nmap('n', '<localleader>li', "<Cmd>lua require('telescope.builtin').lsp_implementations()<CR>", "telescope implementation")
  nmap('n', '<localleader>lr', "<Cmd>lua require('telescope.builtin').lsp_references()<CR>", "telescope references")
  nmap('n', '<localleader>ls', "<Cmd>lua require('telescope.builtin').lsp_document_symbols()<CR>", "telescope document symbols")
  nmap('n', '<localleader>lS', "<Cmd>lua require('telescope.builtin').lsp_workspace_symbols()<CR>", "telescope workspace symbols")
end

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    lsp_buf_options(args.buf, args.data.client_id)
    lsp_keymaps(args.buf)
  end
})

-- TODO: convert to lua
local function lsp_highlight_document(client)
  if client.server_capabilities.documentHighlightProvider then
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
  vim.notify(
    "attaching LSP: " .. client.name .. " to buffer: " .. bufnr,
    vim.log.levels.INFO
  )
  -- lsp_keymaps(bufnr) -- use autocmd LspAttach innstead
  lsp_highlight_document(client)
end

-- TODO: refactor
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
