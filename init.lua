_G.my = {} -- this is my personal global stuffs

if vim.fn.has "nvim-0.10.0" == 0 then
  error "Need at least version 0.10.0 and more"
  return
end
vim.g.mapleader = " "
vim.g.maplocalleader = vim.keycode "<BS>"

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
