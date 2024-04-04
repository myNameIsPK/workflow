local opts = {
  settings = {
    Lua = {
      runtime = {
        -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
        version = "LuaJIT",
      },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = { "vim" },
      },
      workspace = {
        checkThirdParty = false, ---@see https://github.com/LuaLS/lua-language-server/issues/679
        -- -- Make the server aware of Neovim runtime files
        -- library = vim.api.nvim_get_runtime_file("", true),
        maxPreload = 10000,
      },
      telemetry = {
        enable = false,
      },
      hint = { enable = true },
    },
  },
}

-- Add nvim api document plugin
local neodev_ok, neodev = pcall(require, "neodev")
if neodev_ok then
  neodev.setup()
end

return opts
