local opt = vim.opt

opt.exrc = true
opt.list = true
opt.listchars = {
  tab = "  →",
  trail = "·",
  -- lead = '·',
  extends = "»",
  precedes = "«",
  nbsp = "×",
  -- eol = '↲',
  multispace = "·",
  lead = " ",
}
opt.keymap = "thai" -- use keymap in `keymap/{keymap}.vim`
opt.iminsert = 0 -- not use keymap by default
-- opt.shortmess:append "c" -- shorten ins-completion-menu message
opt.shortmess:append "I" -- no intro message
opt.backup = false -- creates a backup file
-- opt.clipboard = "unnamedplus" -- allows neovim to access the system clipboard
opt.cmdheight = 1 -- more space in the neovim command line for displaying messages
opt.colorcolumn = "99999" -- fixes indentline for now
opt.completeopt = "menuone,noselect"
opt.conceallevel = 0 -- so that `` is visible in markdown files
opt.fileencoding = "utf-8" -- the encoding written to a file
opt.foldmethod = "manual" -- folding, set to "expr" for treesitter based folding
opt.foldexpr = "" -- set to `v:lua.vim.treesitter.foldexpr()` for treesitter based folding
opt.foldtext = "" -- use virtual_text folded text
-- opt.guifont = "monospace:h17" -- the font used in graphical neovim applications
opt.hidden = true -- required to keep multiple buffers and open multiple buffers
opt.hlsearch = true -- highlight all matches on previous search pattern
opt.ignorecase = true -- ignore case in search patterns
opt.mouse = "a" -- allow the mouse to be used in neovim
opt.pumheight = 10 -- pop up menu height
-- opt.showcmd = false -- disable showing keystrokes below statusline
opt.showmode = true -- we don't need to see things like -- INSERT -- anymore
opt.showtabline = 1 -- always show tabs
opt.smartcase = true -- smart case
opt.smartindent = true -- make indenting smarter again
opt.splitbelow = true -- force all horizontal splits to go below current window
opt.splitright = true -- force all vertical splits to go to the right of current window
opt.swapfile = false -- creates a swapfile
opt.timeoutlen = 800 -- time to wait for a mapped sequence to complete (in milliseconds)
opt.title = true -- set the title of window to the value of the titlestring
-- opt.titlestring = "%<%F%=%l/%L - nvim" -- what the title of the window will be set to
opt.undofile = true -- enable persistent undo
opt.updatetime = 300 -- faster completion
opt.writebackup = false -- if a file is being edited by another program (or was written to file while editing with another program), it is not allowed to be edited
opt.expandtab = true -- convert tabs to spaces
opt.shiftwidth = 4 -- the number of spaces inserted for each indentation
opt.tabstop = 4 -- insert 4 spaces for a tab
opt.cursorline = true -- highlight the current line
opt.number = true -- set numbered lines
opt.relativenumber = true -- set relative numbered lines
opt.numberwidth = 2 -- set number column width to 2 {default 4}
opt.signcolumn = "auto"
opt.wrap = false
opt.breakindent = true
opt.spell = false
opt.spelllang = "en"
opt.spelloptions:append { "camel" } -- spell check CamelCase
opt.scrolloff = 8
opt.sidescrolloff = 8
opt.smoothscroll = true
opt.guicursor = {
  [[n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50]],
  [[a:blinkwait700-blinkoff400-blinkon250-Cursor/lCursor]],
  [[sm:block-blinkwait175-blinkoff150-blinkon175]],
}
opt.inccommand = "split" -- when `:s` also show in preview window
opt.path:append { "**" } -- fuzzy find file in cwd with `:find`

-- opt.quickfixtextfunc = "v:lua.require'my.plugin.quickfixtext'.quickfixtext"

vim.g.netrw_banner = 0

vim.diagnostic.config {
  virtual_text = true,
  -- signs = true,
  update_in_insert = false,
  underline = true,
  severity_sort = true,
  float = {
    focusable = true,
    style = "minimal",
    border = "none",
    source = true,
    header = "",
    prefix = "",
  },
}
