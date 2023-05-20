local opts = {
  settings = {
    Lua = {
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = { "vim" },
      },
      workspace = {
        checkThirdParty = false, ---@see https://github.com/LuaLS/lua-language-server/issues/679
        -- Make the server aware of Neovim runtime files
        library = {
          [vim.fn.expand "$VIMRUNTIME/lua"] = true,
          [vim.fn.expand "$VIMRUNTIME/lua/vim/lsp"] = true,
        },
        maxPreload = 10000,
      },
      telemetry = {
        enable = false,
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
