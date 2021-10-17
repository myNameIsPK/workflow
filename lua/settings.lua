local M = {}

function M.setup()

  local opt = vim.opt
  local g = vim.g
  local wo = vim.wo
  local cmd = vim.cmd

  local indent = 2

  -- cmd "colorscheme slate"

  cmd "colorscheme gruvbox-material"
  cmd "set background=dark"
  g.gruvbox_material_background = "soft"
  g.gruvbox_material_disable_italic_comment = 1
  g.gruvbox_material_visual = "reverse"

  cmd "au TextYankPost * silent! lua vim.highlight.on_yank()"

  -- steal from lunarvim
  opt.shortmess:append "c" -- shorten ins-completion-menu message
  opt.backup = false -- creates a backup file
  opt.clipboard = "unnamedplus" -- allows neovim to access the system clipboard
  -- opt.cmdheight = 2 -- more space in the neovim command line for displaying messages
  opt.colorcolumn = "99999" -- fixes indentline for now
  opt.completeopt = "menuone,noselect"
  opt.conceallevel = 0 -- so that `` is visible in markdown files
  opt.fileencoding = "utf-8" -- the encoding written to a file
  opt.foldmethod = "manual" -- folding, set to "expr" for treesitter based folding
  opt.foldexpr = "" -- set to "nvim_treesitter#foldexpr()" for treesitter based folding
  opt.guifont = "monospace:h17" -- the font used in graphical neovim applications
  opt.hidden = true -- required to keep multiple buffers and open multiple buffers
  opt.hlsearch = true -- highlight all matches on previous search pattern
  opt.ignorecase = true -- ignore case in search patterns
  opt.mouse = "a" -- allow the mouse to be used in neovim
  opt.pumheight = 10 -- pop up menu height
  opt.showmode = false -- we don't need to see things like -- INSERT -- anymore
  opt.showtabline = 1 -- always show tabs
  opt.smartcase = true -- smart case
  opt.smartindent = true -- make indenting smarter again
  opt.splitbelow = true -- force all horizontal splits to go below current window
  opt.splitright = true -- force all vertical splits to go to the right of current window
  opt.swapfile = false -- creates a swapfile
  opt.termguicolors = true -- set term gui colors (most terminals support this)
  opt.timeoutlen = 400 -- time to wait for a mapped sequence to complete (in milliseconds)
  opt.title = true -- set the title of window to the value of the titlestring
  -- opt.titlestring = "%<%F%=%l/%L - nvim" -- what the title of the window will be set to
  opt.undofile = true -- enable persistent undo
  opt.updatetime = 300 -- faster completion
  opt.writebackup = false -- if a file is being edited by another program (or was written to file while editing with another program), it is not allowed to be edited
  opt.expandtab = true -- convert tabs to spaces
  opt.shiftwidth = indent -- the number of spaces inserted for each indentation
  opt.tabstop = indent -- insert 2 spaces for a tab
  wo.cursorline = true -- highlight the current line
  opt.number = true -- set numbered lines
  -- opt.relativenumber = false -- set relative numbered lines
  opt.numberwidth = 2 -- set number column width to 2 {default 4}
  opt.signcolumn = "yes" -- always show the sign column, otherwise it would shift the text each time
  opt.wrap = false -- display lines as one long line
  opt.spell = false
  opt.spelllang = "en"
  opt.scrolloff = 8 -- is one of my fav
  opt.sidescrolloff = 8

  opt.guicursor = {
    [[n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50]],
    [[a:blinkwait700-blinkoff400-blinkon250-Cursor/lCursor]],
    [[sm:block-blinkwait175-blinkoff150-blinkon175]],
  }
  -- TODO: transparent window
end

return M
