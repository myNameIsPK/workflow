local opts = {
  settings = {
    Lua = {
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = { "vim" },
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = {
          [vim.fn.expand "$VIMRUNTIME/lua"] = true,
          [vim.fn.expand "$VIMRUNTIME/lua/vim/lsp"] = true,
        },
        maxPreload = 10000,
      },
    },
  },
}

-- Add nvim api document plugin
local neodev_ok, neodev = pcall(require, "neodev")
if neodev_ok then
  neodev.setup()
end

return opts
