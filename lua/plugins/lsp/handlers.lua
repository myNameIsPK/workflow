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
  local function map(mode, lhs, rhs, desc)
    vim.keymap.set(mode, lhs, rhs, { buffer = bufnr, desc = desc })
  end

  -- stylua: ignore start
  map('n', 'gD', "<Cmd>lua vim.lsp.buf.declaration()<CR>", "Go to declaration")
  map('n', 'gd', "<Cmd>lua vim.lsp.buf.definition()<CR>", "Go to definition") -- use <C-]> instead
  map('n', 'gr', "<Cmd>lua vim.lsp.buf.references()<CR>", "List references")
  map('n', 'gi', "<Cmd>lua vim.lsp.buf.implementation()<CR>", "Go to implementation")
  map('n', 'K', "<Cmd>lua vim.lsp.buf.hover()<CR>", "Hover")
  map('n', '<C-k>', "<Cmd>lua vim.lsp.buf.signature_help()<CR>", "Signature help")
  map('n', '<localleader>lwa', "<Cmd>lua vim.lsp.buf.add_workspace_folder()<CR>", "Workspace Add folder")
  map('n', '<localleader>lwd', "<Cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>", "Workspace Delete folder")
  map('n', '<localleader>lwl', "<Cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>", "Workspace list folder")
  map('n', '<localleader>ld', "<Cmd>lua vim.lsp.buf.type_definition()<CR>", "Type definition")
  map('n', '<localleader>lrn', "<Cmd>lua vim.lsp.buf.rename()<CR>", "Rename")
  map('n', '<localleader>la', "<Cmd>lua vim.lsp.buf.code_action()<CR>", "Code action")
  map('v', '<localleader>la', "<Cmd>'<,'>lua vim.lsp.buf.range_code_action()<CR>", "Code action")
  -- FIXME: use vim.lsp.buf.format
  map('n', '<localleader>lf', "<Cmd>lua vim.lsp.buf.formatting()<CR>", "formatting")
  map('v', '<localleader>lf', "<Cmd>'<,'>lua vim.lsp.buf.range_formatting()<CR>", "formatting")

  -- telescope
  map('n', '<localleader>lD', "<Cmd>lua require('telescope.builtin').lsp_definitions()<CR>", "telescope definition")
  map('n', '<localleader>lT', "<Cmd>lua require('telescope.builtin').lsp_type_definitions()<CR>", "telescope type definition")
  map('n', '<localleader>lI', "<Cmd>lua require('telescope.builtin').lsp_implementations()<CR>", "telescope implementation")
  map('n', '<localleader>lR', "<Cmd>lua require('telescope.builtin').lsp_references()<CR>", "telescope references")
  map('n', '<localleader>lS', "<Cmd>lua require('telescope.builtin').lsp_document_symbols()<CR>", "telescope document symbols")
  map('n', '<localleader>lW', "<Cmd>lua require('telescope.builtin').lsp_workspace_symbols()<CR>", "telescope workspace symbols")
  -- stylua: ignore end
end

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    lsp_buf_options(args.buf, args.data.client_id)
    lsp_keymaps(args.buf)
  end,
})

local function lsp_format_on_save(bufnr)
  local lsp_formatting = function()
    vim.lsp.buf.format {
      filter = function(client)
        return client.name == "null-ls"
      end,
      bufnr = bufnr,
    }
  end

  vim.api.nvim_create_augroup("_lsp_auto_format", { clear = true })
  vim.api.nvim_create_autocmd("BufWritePre", {
    group = "_lsp_auto_format",
    buffer = bufnr,
    callback = lsp_formatting,
  })
end

-- TODO: convert to lua
local function lsp_highlight_document(client)
  if client.server_capabilities.documentHighlightProvider then
    vim.cmd [[
      augroup _lsp_document_highlight
        autocmd! * <buffer>
        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorHoldI <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
      augroup END
    ]]
  end
end

function M.on_attach(client, bufnr)
  vim.notify("attaching LSP: " .. client.name .. " to buffer: " .. bufnr, vim.log.levels.INFO)
  -- lsp_keymaps(bufnr) -- use autocmd LspAttach innstead
  lsp_format_on_save(bufnr)
  lsp_highlight_document(client)
end

-- TODO: refactor
function M.root_dir(fname)
  local root_pattern = require("lspconfig.util").root_pattern
  return root_pattern ".git"(fname)
    or root_pattern "tsconfig.base.json"(fname)
    or root_pattern "package.json"(fname)
    or root_pattern ".eslintrc.js"(fname)
    or root_pattern "tsconfig.json"(fname)
end

local nvim_lsp_ok, cmp_nvim_lsp = pcall(require, "cmp_nvim_lsp")
if nvim_lsp_ok then
  M.capabilities = cmp_nvim_lsp.default_capabilities()
else
  M.capabilities = vim.lsp.protocol.make_client_capabilities()
end

return M
