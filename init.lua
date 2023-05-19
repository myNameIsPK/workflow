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
require "plugins_config.telescope"
require "plugins_config.lsp"
require "plugins_config.luasnip"
require "plugins_config.cmp"
require "plugins_config.autopairs"
require "plugins_config.treesitter"
require "plugins_config.gitsigns"
require "plugins_config.neogit"
require "plugins_config.zk"
require "plugins_config.whichkey"
require "plugins_config.comment"
