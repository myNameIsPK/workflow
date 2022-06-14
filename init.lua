-- ██╗███╗   ██╗██╗████████╗██╗     ██╗   ██╗ █████╗
-- ██║████╗  ██║██║╚══██╔══╝██║     ██║   ██║██╔══██╗
-- ██║██╔██╗ ██║██║   ██║   ██║     ██║   ██║███████║
-- ██║██║╚██╗██║██║   ██║   ██║     ██║   ██║██╔══██║
-- ██║██║ ╚████║██║   ██║██╗███████╗╚██████╔╝██║  ██║
-- ╚═╝╚═╝  ╚═══╝╚═╝   ╚═╝╚═╝╚══════╝ ╚═════╝ ╚═╝  ╚═╝

-- set leader key before all plugins for safety
vim.g.mapleader = " "
vim.g.maplocalleader = ","

require "highlight"
require "globals"
require "plugins"
pcall(require, "packer_compiled")

require "plugins.telescope"
require "plugins.lsp"
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
require "plugins.alpha"
require "plugins.todocomments"
