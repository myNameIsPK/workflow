local M = {}

-- NOTE: TRY HARDER
function M.lsp_diagnostics()
  vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
    virtual_text = true,
    underline = false,
    signs = true,
    update_in_insert = false,
  })

  local on_references = vim.lsp.handlers["textDocument/references"]
  vim.lsp.handlers["textDocument/references"] = vim.lsp.with(on_references, { loclist = true, virtual_text = true })

  -- Send diagnostics to quickfix list
  do
    local method = "textDocument/publishDiagnostics"
    local default_handler = vim.lsp.handlers[method]
    vim.lsp.handlers[method] = function(err, meth, result, client_id, bufnr, config)
      default_handler(err, meth, result, client_id, bufnr, config)
      local diagnostics = vim.lsp.diagnostic.get_all()
      local qflist = {}
      for buf, diagnostic in pairs(diagnostics) do
        for _, d in ipairs(diagnostic) do
          d.bufnr = buf
          d.lnum = d.range.start.line + 1
          d.col = d.range.start.character + 1
          d.text = d.message
          table.insert(qflist, d)
        end
      end
      vim.lsp.util.set_qflist(qflist)
    end
  end
end

function M.lsp_highlight(client)
  -- Set autocommands conditional on server_capabilities
  if client.resolved_capabilities.document_highlight then
    vim.api.nvim_exec(
      [[
        hi LspReferenceRead cterm=bold ctermbg=red guibg=#585858
        hi LspReferenceText cterm=bold ctermbg=red guibg=#585858
        hi LspReferenceWrite cterm=bold ctermbg=red guibg=#585858
        augroup lsp_document_highlight
        autocmd! * <buffer>
        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
        augroup END
        ]],
      false
    )
  end
end

function M.lsp_buffer_keybindings(bufnr)
--   local status_ok, wk = pcall(require, "which-key")
--   if not status_ok then
--     return
--   end
-- 
--   local keys = {
--     ["K"] = { "<cmd>lua vim.lsp.buf.hover()<CR>", "Show hover" },
--     ["gd"] = { "<cmd>lua vim.lsp.buf.definition()<CR>", "Goto Definition" },
--     ["gD"] = { "<cmd>lua vim.lsp.buf.declaration()<CR>", "Goto declaration" },
--     ["gr"] = { "<cmd>lua vim.lsp.buf.references()<CR>", "Goto references" },
--     ["gI"] = { "<cmd>lua vim.lsp.buf.implementation()<CR>", "Goto Implementation" },
--     ["gs"] = { "<cmd>lua vim.lsp.buf.signature_help()<CR>", "show signature help" },
--     ["gp"] = { "<cmd>lua require'lsp.peek'.Peek('definition')<CR>", "Peek definition" },
--     ["gl"] = {
--       "<cmd>lua require'lsp.handlers'.show_line_diagnostics()<CR>",
--       "Show line diagnostics",
--     },
--   }
--   wk.register(keys, { mode = "n", buffer = bufnr })

  --   TODO: edit later
--   local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
--   local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end
-- buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')
-- 
--   local opts = { noremap=true, silent=true }
-- 
--   -- See `:help vim.lsp.*` for documentation on any of the below functions
--   buf_set_keymap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
--   buf_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
--   buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
--   buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
--   buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
--   buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
--   buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
--   buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
--   buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
--   buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
--   buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
--   buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
--   buf_set_keymap('n', '<space>de', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
--   buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
--   buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
--   buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
--   buf_set_keymap('n', '<space>f', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)

  require("keymappings").setup_lsp_mappings()

end

function M.get_capabilities()
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities.textDocument.completion.completionItem.snippetSupport = true
  capabilities.textDocument.completion.completionItem.resolveSupport = {
    properties = {
      "documentation",
      "detail",
      "additionalTextEdits",
    },
  }

  local status_ok, cmp_nvim_lsp = pcall(require, "cmp_nvim_lsp")
  if status_ok then
    capabilities = cmp_nvim_lsp.update_capabilities(capabilities)
  end

  return capabilities
end

function M.lsp_attach(client, bufnr)
  M.lsp_highlight(client)
  M.lsp_buffer_keybindings(bufnr)
end

function M.get_common_opts()
  return {
    on_attach = M.common_on_attach,
--    on_init = M.common_on_init,
    capabilities = M.common_capabilities(),
  }
end
-- NOTE: AGAIN AND AGAIN

local function setup_servers()
  local lspinstall = require "lspinstall"

  lspinstall.setup()

  -- null-ls
  -- require("config.lsp.null-ls").setup()

  local servers = lspinstall.installed_servers()
  for _, server in pairs(servers) do
    -- TODO: Edit this
    -- require "lspconfig"[server].setup{}
    require "lspconfig"[server].setup(vim.tbl_deep_extend("force", {
      on_attach = M.lsp_attach,
      -- on_exit = M.lsp_exit,
      -- on_init = M.lsp_init,
      capabilities = M.get_capabilities(),
      flags = { debounce_text_changes = 150 },
    }, {}))
  end


  -- TODO: Edit hard code
  require "lspconfig".lua.setup(vim.tbl_deep_extend("force", {
    -- TODO: this is redundant
    on_attach = M.lsp_attach,
    -- on_exit = M.lsp_exit,
    -- on_init = M.lsp_init,
    capabilities = M.get_capabilities(),
    flags = { debounce_text_changes = 150 },
    -- TODO END: redundant end here
    settings = {
      Lua = {
        diagnostics = { globals = { "vim" } },
        workspace = {
          library = {
            [vim.fn.expand "$VIMRUNTIME/lua"] = true,
            [vim.fn.expand "$VIMRUNTIME/lua/vim/lsp"] = true,
          },
          maxPreload = 100000,
          preloadFileSize = 1000,
        }
      }
    }
  }, {}))
end

local function post_install()
  local lspinstall = require "lspinstall"
  lspinstall.post_install_hook = function()
    setup_servers()
    vim.cmd "bufdo e"
  end
end

function M.setup()
  post_install()
  setup_servers()
end

return M
