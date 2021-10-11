-- ██╗███╗   ██╗██╗████████╗██╗     ██╗   ██╗ █████╗
-- ██║████╗  ██║██║╚══██╔══╝██║     ██║   ██║██╔══██╗
-- ██║██╔██╗ ██║██║   ██║   ██║     ██║   ██║███████║
-- ██║██║╚██╗██║██║   ██║   ██║     ██║   ██║██╔══██║
-- ██║██║ ╚████║██║   ██║██╗███████╗╚██████╔╝██║  ██║
-- ╚═╝╚═╝  ╚═══╝╚═╝   ╚═╝╚═╝╚══════╝ ╚═════╝ ╚═╝  ╚═╝

vim.g.mapleader = ' '
vim.g.maplocalleader = ","

local fn = vim.fn
local cmd = vim.cmd

local function packer_init()
  local install_path = fn.stdpath "data" .. "/site/pack/packer/opt/packer.nvim"
  if fn.empty(fn.glob(install_path)) > 0 then
    cmd("!git clone https://github.com/wbthomason/packer.nvim " .. install_path)
    cmd "packadd packer.nvim"
  end
  cmd "autocmd bufwritepost plugins.lua packercompile"
end

local function sys_init()
  -- performance
  require "impatient"
end

packer_init()

sys_init()

require("settings").setup()

require("keymappings").setup()

require("config.lsp").setup()

require("plugins").setup()

-- From "impatient"
require "packer_compiled"
