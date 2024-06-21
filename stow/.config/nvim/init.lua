_G.my = {} -- this is my personal global stuffs

vim.g.mapleader = " "
vim.cmd [[ let g:maplocalleader="\<BS>" ]]
-- vim.g.maplocalleader = vim.keycode "<BS>"

vim.loader.enable()

require "globals"
require "options"

-- TODO: make it place more appropriate
my.got_override, _ = pcall(require, "local_override")

vim.cmd.colorscheme(my.opts.colorscheme.default)
vim.opt.background = my.opts:background_resolve()

require "commands"
require "keymappings"
require "autocmds"

require "lazy_boostrap"
