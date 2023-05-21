local M = {}

local lsp_formatting = function(bufnr)
  bufnr = bufnr or nil
  vim.lsp.buf.format {
    filter = function(client)
      return client.name == "null-ls"
    end,
    bufnr,
  }
end

local function lsp_keymaps(bufnr)
  local function map(mode, lhs, rhs, desc)
    vim.keymap.set(mode, lhs, rhs, { buffer = bufnr, desc = desc })
  end

  -- stylua: ignore start
  map('n', 'gD', function() vim.lsp.buf.declaration() end, "Go to declaration")
  map('n', 'gd', function() vim.lsp.buf.definition() end, "Go to definition") -- use <C-]> instead
  map('n', 'gr', function() vim.lsp.buf.references() end, "List references")
  map('n', 'gi', function() vim.lsp.buf.implementation() end, "Go to implementation")
  map('n', 'K', function() vim.lsp.buf.hover() end, "Hover")
  map('n', '<C-k>', function() vim.lsp.buf.signature_help() end, "Signature help")
  map('n', '<localleader>lwa', function() vim.lsp.buf.add_workspace_folder() end, "Workspace Add folder")
  map('n', '<localleader>lwd', function() vim.lsp.buf.remove_workspace_folder() end, "Workspace Delete folder")
  map('n', '<localleader>lwl', function() print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end, "Workspace list folder")
  map('n', '<localleader>ld', function() vim.lsp.buf.type_definition() end, "Type definition")
  map('n', '<localleader>lrn', function() vim.lsp.buf.rename() end, "Rename")
  map('n', '<localleader>la', function() vim.lsp.buf.code_action() end, "Code action")
  map('v', '<localleader>la', function() vim.lsp.buf.range_code_action() end, "Code action")

  map('n', '<localleader>lf', function() lsp_formatting() end, "formatting")
  map('v', '<localleader>lf', function() lsp_formatting() end, "formatting")

  -- telescope
  map('n', '<localleader>lD', function() require('telescope.builtin').lsp_definitions() end, "telescope definition")
  map('n', '<localleader>lT', function() require('telescope.builtin').lsp_type_definitions() end, "telescope type definition")
  map('n', '<localleader>lI', function() require('telescope.builtin').lsp_implementations() end, "telescope implementation")
  map('n', '<localleader>lR', function() require('telescope.builtin').lsp_references() end, "telescope references")
  map('n', '<localleader>lS', function() require('telescope.builtin').lsp_document_symbols() end, "telescope document symbols")
  map('n', '<localleader>lW', function() require('telescope.builtin').lsp_workspace_symbols() end, "telescope workspace symbols")

  map("n", "<localleader>lsc", function()
    local clients = vim.lsp.get_active_clients() 
    if #clients >= 0 then
      for _, c in pairs(clients) do
        I(c.server_capabilities)
      end
    end
  end,
    "print LSP servercapabilities"
  )
  -- stylua: ignore end
end

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local bufnr = args.buf
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    -- NOTE: disable semantic tokens highlight because it mess up TS todo comment
    if not my.opts.lsp.samantic_tokens and client.server_capabilities.semanticTokensProvider then
      client.server_capabilities.semanticTokensProvider = nil
    end
    lsp_keymaps(bufnr)
  end,
})

local function lsp_format_on_save(bufnr)
  vim.api.nvim_create_augroup("_lsp_auto_format", { clear = true })
  vim.api.nvim_create_autocmd("BufWritePre", {
    group = "_lsp_auto_format",
    buffer = bufnr,
    callback = function()
      lsp_formatting(bufnr)
    end,
  })
end

-- TODO: convert to lua
local function lsp_highlight_document(client)
  if my.opts.lsp.document_highlight and client.server_capabilities.documentHighlightProvider then
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
  -- vim.notify("attaching LSP: " .. client.name .. " to buffer: " .. bufnr, vim.log.levels.INFO)
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
