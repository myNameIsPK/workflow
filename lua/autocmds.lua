local function group(group_name)
  return vim.api.nvim_create_augroup(group_name, { clear = false })
end
local autocmd = vim.api.nvim_create_autocmd

autocmd("TextYankPost", {
  group = group "_general_settings",
  -- pattern = "*",
  callback = function()
    vim.highlight.on_yank()
  end,
  -- command = "silent! lua vim.highlight.on_yank()", -- or use command instead of callback
  desc = "Highlight yanked text",
})

autocmd("TermOpen", {
  group = group "_terminal",
  pattern = "term://*",
  callback = function()
    vim.opt_local.number = false
    vim.opt_local.relativenumber = false
    vim.cmd.startinsert()
  end,
  desc = "Fix terminal behavior",
})

autocmd("TermClose", {
  group = group "_terminal",
  pattern = "term://*",
  callback = function()
    vim.fn.feedkeys "q"
  end,
  desc = "Close without showing exitcode",
})

vim.api.nvim_create_autocmd("BufEnter", {
  group = group "_lsp_auto_start",
  pattern = vim.fn.stdpath "config" .. "/*.lua",
  command = "LspStart",
  desc = "Auto start LSP in Nvim config directory",
})

local lsp_formatting = function(bufnr)
  bufnr = bufnr or nil
  vim.lsp.buf.format {
    filter = function(client)
      return client.name == "null-ls"
    end,
    bufnr,
  }
end

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local bufnr = args.buf
    local function map(mode, lhs, rhs, desc)
      vim.keymap.set(mode, lhs, rhs, { buffer = bufnr, desc = desc })
    end

    -- stylua: ignore start
    local client = vim.lsp.get_client_by_id(args.data.client_id) or {}
    if client.server_capabilities.declarationProvider then
      map('n', 'gD', function() vim.lsp.buf.declaration() end, "Go to declaration")
    end
    if client.server_capabilities.definitionProvider then
      map('n', 'gd', function() vim.lsp.buf.definition() end, "Go to definition") -- use <C-]> instead
    end
    if client.server_capabilities.hoverProvider then
      map('n', 'K', function() vim.lsp.buf.hover() end, "Hover")
    end
    map('n', '<C-k>', function() vim.lsp.buf.signature_help() end, "Signature help")
    map('n', 'gr', function() vim.lsp.buf.references() end, "List references")
    map('n', '<localleader>i', function() vim.lsp.buf.implementation() end, "Go to implementation")
    map('n', '<localleader>wa', function() vim.lsp.buf.add_workspace_folder() end, "Workspace Add folder")
    map('n', '<localleader>wd', function() vim.lsp.buf.remove_workspace_folder() end, "Workspace Delete folder")
    map('n', '<localleader>wl', function() print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end, "Workspace list folder")
    map('n', '<localleader>d', function() vim.lsp.buf.type_definition() end, "Type definition")
    map('n', '<localleader>n', function() vim.lsp.buf.rename() end, "Rename")
    map('n', '<localleader>a', function() vim.lsp.buf.code_action() end, "Code action")
    map('v', '<localleader>a', function() vim.lsp.buf.code_action() end, "Code action")

    map('n', '<localleader>f', function() lsp_formatting() end, "formatting")
    map('v', '<localleader>f', function() lsp_formatting() end, "formatting")

    -- telescope
    map('n', '<localleader>D', function() require('telescope.builtin').lsp_definitions() end, "telescope definition")
    map('n', '<localleader>R', function() require('telescope.builtin').lsp_references() end, "telescope references")
    map('n', '<localleader>T', function() require('telescope.builtin').lsp_type_definitions() end, "telescope type definition")
    map('n', '<localleader>I', function() require('telescope.builtin').lsp_implementations() end, "telescope implementation")
    map('n', '<localleader>S', function() require('telescope.builtin').lsp_document_symbols() end, "telescope document symbols")
    map('n', '<localleader>W', function() require('telescope.builtin').lsp_workspace_symbols() end, "telescope workspace symbols")
    map('n', '<localleader><C-i>', function() require('telescope.builtin').lsp_incoming_calls() end, "telescope incoming calls")
    map('n', '<localleader><C-o>', function() require('telescope.builtin').lsp_outgoing_calls() end, "telescope outgoing calls")

    map("n", "<localleader>sc", function()
      local result = {}
      -- TODO: deprecate
      for _, c in ipairs(vim.lsp.get_active_clients()) do
        table.insert(result, "=== " .. c.name .. " ===")
        table.insert(result, c.server_capabilities)
      end
      vim.print(result)
    end,
      "print LSP servercapabilities"
    )
    -- stylua: ignore end

    -- TODO: deprecate
    for _, c in ipairs(vim.lsp.get_active_clients { bufnr = bufnr }) do
      if c.server_capabilities.documentHighlightProvider then
        -- Autocommands in autocommand??
        vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
          buffer = bufnr,
          callback = function()
            vim.lsp.buf.document_highlight()
          end,
        })
        vim.api.nvim_create_autocmd({ "CursorMoved" }, {
          buffer = bufnr,
          callback = function()
            vim.lsp.buf.clear_references()
          end,
        })
      end
    end
  end,
})

autocmd("BufWritePre", {
  group = group "_lsp_auto_format",
  callback = function()
    lsp_formatting(nil)
  end,
})

vim.cmd [[

    " augroup _smart_relativenumber
    "   autocmd!
    "   autocmd InsertEnter * :set norelativenumber
    "   autocmd InsertLeave * :set relativenumber
    " augroup END

    augroup _iminsert
      autocmd!
      autocmd InsertLeave * :set iminsert=0
    augroup END

    augroup _quit_with_q
      autocmd!
      autocmd FileType qf,help,man,lspinfo nnoremap <silent> <buffer> q :close<CR>
    augroup END

    augroup _reload_config
      autocmd BufWritePost *.utf-8.add :mkspell! ~/.config/nvim/spell/en.utf-8.add
      autocmd BufWritePost xresources :silent !xrdb -merge ~/.config/x11/xresources
      autocmd BufWritePost sxhkdrc :silent !pkill -USR1 -x sxhkd; notify-send sxhkd reloaded\!\!
      autocmd BufWritePost skippy-xd.rc :silent !pkill -USR1 -x skippy-xd; notify-send skippy-xd reloaded\!\!
      autocmd BufWritePost $XDG_CONFIG_HOME/polybar/config.ini :silent !pkill -USR1 -x polybar && notify-send polybar reloaded\!\!
      autocmd BufWritePost $XDG_CONFIG_HOME/colorsync/config.json :silent !colorsync && template-gen && reload
    augroup END

    augroup _compile
      autocmd BufWritePost *.tex :silent !latexmk -pdf -interaction=nonstopmode %
    augroup END

    augroup _ansible
      autocmd!
      autocmd BufRead *.yaml,*.yml if search('hosts:\|tasks:', 'nw') | set ft=yaml.ansible | endif
    augroup END

  ]]
