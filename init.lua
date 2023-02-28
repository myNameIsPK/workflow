-- ██╗███╗   ██╗██╗████████╗██╗     ██╗   ██╗ █████╗
-- ██║████╗  ██║██║╚══██╔══╝██║     ██║   ██║██╔══██╗
-- ██║██╔██╗ ██║██║   ██║   ██║     ██║   ██║███████║
-- ██║██║╚██╗██║██║   ██║   ██║     ██║   ██║██╔══██║
-- ██║██║ ╚████║██║   ██║██╗███████╗╚██████╔╝██║  ██║
-- ╚═╝╚═╝  ╚═══╝╚═╝   ╚═╝╚═╝╚══════╝ ╚═════╝ ╚═╝  ╚═╝

vim.g.mapleader = " "
vim.cmd 'let maplocalleader = "\\<BS>"' -- map <BS> to localleader

pcall(require, "impatient")

vim.cmd "colorscheme custom"

require "globals"
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
require "plugins.hydra"
require "plugins.comment"
require "plugins.alpha"
require "plugins.indentblankline"
-- require "plugins.todocomments"
