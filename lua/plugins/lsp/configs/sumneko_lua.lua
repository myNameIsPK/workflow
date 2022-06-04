local opts = {
  settings = {
    Lua = {
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = { 'vim' },
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = {
          [vim.fn.expand('$VIMRUNTIME/lua')] = true,
          [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true,
        },
        maxPreload = 10000,
      },
    },
  },
}

-- Add nvim api document plugin
local luadev_ok, luadev = pcall(require, "lua-dev")
if not luadev_ok then
  return
end
opts = vim.tbl_deep_extend("force", opts, luadev.setup())

return opts
