_G.my = {} -- this is my personal global stuffs

vim.g.mapleader = " "
if vim.fn.has "nvim-0.10.0" == 1 then
  vim.g.maplocalleader = vim.keycode "<BS>"
else
  vim.cmd 'let maplocalleader = "\\<BS>"'
end

pcall(require, "impatient")

-- vim.cmd "colorscheme custom"
-- vim.cmd "colorscheme retrobox"

require "globals"
require "options"
require "commands"
require "keymappings"
require "autocmds"

require "lazy_boostrap"

require "my.telescope"
-- require "my.luasnip"
-- require "my.lsp"
