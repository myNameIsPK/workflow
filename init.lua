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

require "plugins"
require "plugins.telescope"
require "plugins.lsp"
require "plugins.luasnip"
require "plugins.cmp"
require "plugins.autopairs"
require "plugins.treesitter"
require "plugins.gitsigns"
require "plugins.neogit"
require "plugins.zk"
require "plugins.orgmode"
require "plugins.neorg"
require "plugins.whichkey"
require "plugins.comment"
