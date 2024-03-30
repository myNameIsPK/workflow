_G.my = {} -- this is my personal global stuffs

vim.g.mapleader = " "
if vim.fn.has "nvim-0.10.0" == 1 then
  vim.g.maplocalleader = vim.keycode "<BS>"
else
  vim.cmd 'let maplocalleader = "\\<BS>"'
end

vim.loader.enable()

require "globals"
require "options"

-- TODO: make it place more appropriate
local is_override, _ = pcall(require, "local_override")
my.got_override = is_override

vim.cmd.colorscheme(my.opts.colorscheme.default)
vim.opt.background = my.opts:background_resolve()

require "commands"
require "keymappings"
require "autocmds"

require "lazy_boostrap"
