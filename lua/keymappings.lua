-- stylua: ignore start
local map = require("utils.mappings").map

--- Plaground make it work
map("n", "<leader>rf", "<cmd>lua my.save_and_exec()<CR>")
map("n", "<leader>ra", "<cmd>lua require('utils.reload').reload_all()<CR>")
---

-- map("i", { "jk", "kj", "jj", "kk" }, "<Esc>")

-- like in emacs keybinds <c-n>,<c-p> is cursor up,down
-- this make <c-f>,<c-b> is <right>,<left> in emacs too
-- if you want full page up,down use <PageUp/Dn> instead
map("n", "<c-f>", "<right>")
map("n", "<c-b>", "<left>")

map("i", "<M-Down>", "<Esc>:m .+1<CR>==gi")
map("i", "<M-Up>", "<Esc>:m .-2<CR>==gi")

map({ "v", "x" } , "<M-Up>", ":m '<-2<CR>gv=gv")
map({ "v", "x" } , "<M-Down>", ":m '>+1<CR>gv=gv")

map("n", "<M-Down>", ":m .+1<CR>==")
map("n", "<M-Up>", ":m .-2<CR>==")

map("i", ",", ",<c-g>u")
map("i", ".", ".<c-g>u")
map("i", "!", "!<c-g>u")
map("i", "?", "?<c-g>u")

-- map("i", "<C-j>", 'pumvisible() ? "\\<C-n>" : "\\<C-j>"', { expr = true, noremap = true })
-- map("i", "<C-k>", 'pumvisible() ? "\\<C-p>" : "\\<C-k>"', { expr = true, noremap = true })

map("n", "<Up>", "g<Up>")
map("n", "<Down>", "g<Down>")

-- map("n", "<C-h>", "<C-w>h")
-- map("n", "<C-j>", "<C-w>j")
-- map("n", "<C-k>", "<C-w>k")
-- map("n", "<C-l>", "<C-w>l")

-- map("n", "<C-Up>", ":resize -2<CR>")
-- map("n", "<C-Down>", ":resize +2<CR>")
-- map("n", "<C-Left>", ":vertical resize -2<CR>")
-- map("n", "<C-Right>", ":vertical resize +2<CR>")

map("n", { "<M-]>", "]b" }, ":bn<Cr>")
map("n", { "<M-[>", "[b" }, ":bp<Cr>")
map("n", "]q", "<Cmd>cnext<Cr>")
map("n", "[q", "<Cmd>cprev<Cr>")
map("n", "]l", "<Cmd>lnext<Cr>")
map("n", "[l", "<Cmd>lprev<Cr>")

map("n", "n", "nzzzv")
map("n", "N", "Nzzzv")
map("n", "J", "mzJ`z")

-- [count] j/k become jump motion
map("n", "<expr> j", "(v:count > 1 ? \"m'\" . v:count : '') . 'j'")
map("n", "<expr> k", "(v:count > 1 ? \"m'\" . v:count : '') . 'k'")

map("v", "<", "<gv")
map("v", ">", ">gv")

map("v", "p", '"_dP') -- not yank before paste in visual select
-- map("n", "<del>", '"_') -- send all delete to null on every opration
map("n", "<leader>p", '"+p')
map("n", "<leader>P", '"*p')
map("n", "<leader>y", '"+y')
map("n", "<leader>Y", '"+Y')

map("t", "<c-g>", "<C-\\><C-n>")

-- map("t", "<C-h>", "<C-\\><C-n><C-w>h")
-- map("t", "<C-j>", "<C-\\><C-n><C-w>j")
-- map("t", "<C-k>", "<C-\\><C-n><C-w>k")
-- map("t", "<C-l>", "<C-\\><C-n><C-w>l")

map("c", "<C-a>", "<Home>")
-- map("c", "<C-j>", 'pumvisible() ? "\\<C-n>" : "\\<C-j>"', { expr = true, noremap = true })
-- map("c", "<C-k>", 'pumvisible() ? "\\<C-p>" : "\\<C-k>"', { expr = true, noremap = true })

-- See `:help vim.diagnostic.*` for documentation on any of the below functions
map('n', '<leader>de', function() vim.diagnostic.open_float() end, { desc = "Diagnostic Float" })
map('n', '[d', function() vim.diagnostic.goto_prev() end, { desc = "Diagnostic Prev" })
map('n', '<leader>dq', function() vim.diagnostic.setloclist() end, { desc = "Diagnostic Quickfix" })
map('n', ']d', function() vim.diagnostic.goto_next() end, { desc = "Diagnostic Next" })

map("n", "<leader>si", vim.cmd.Inspect, { desc = "Inspect" })

map("n", "<leader>tt", vim.cmd.terminal, { desc = "Terminal" })
map("n", "<leader>tl", function() vim.cmd.terminal("lf") end, { desc = "LF in terminal" })

map("n", "<leader>ts", function() vim.opt.spell = not(vim.opt.spell:get()) end, { desc = "Spell toggle" })
map("n", "<leader>tq", function() vim.cmd.QfToggle() end, { desc = "Quickfix window toggle" })

map("n", "<leader>ee", vim.cmd.Ex, { desc = "File Explorer"})

map("n", "<leader>rm", vim.cmd.RemoveThisFile, { desc = "remove current files" })

-- Telescope
-- Builtin
map("n", "<leader>fa", "<Cmd>Telescope builtin<Cr>", { desc = "All Builtin" })
map("n", "<leader>ff", "<Cmd>Telescope find_files<Cr>", { desc = "Files" })
map("n", "<leader>fg", "<Cmd>Telescope live_grep<Cr>", { desc = "Grep Files" })
map("n", "<leader>fb", "<Cmd>Telescope buffers<Cr>", { desc = "Buffers" })
map("n", "<leader>fr", "<Cmd>Telescope oldfiles<Cr>", { desc = "Recent Files" })
map("n", "<leader>fq", "<Cmd>Telescope quickfix<Cr>", { desc = "Quickfix" })
map("n", "<leader>hh", "<Cmd>Telescope help_tags<Cr>", { desc = "Helps" })
map("n", "<leader>hm", "<Cmd>Telescope man_pages<Cr>", { desc = "Man pages" })
map('n', '<leader>fdb', '<cmd>Telescope diagnostics bufnr=0<cr>', { desc = "Diagnostics Buffer" })
map('n', '<leader>fdw', '<cmd>Telescope diagnostics<cr>', { desc = "Diagnostics Workspace" })
-- Loaded Extensions
map("n", "<leader>fp", "<Cmd>Telescope projects<Cr>", { desc = "Projects" })
map("n", "<leader>fl", "<Cmd>Telescope bibtex<Cr>", { desc = "Bib(La)tex" })
-- Custom
map("n", "<leader>fc", function() require("my.telescope").find_vim_files() end, { desc = "Config files(Neovim)" })
map("n", "<leader>fC", function() require("my.telescope").find_vim_data() end, { desc = "Plugins files(Neovim)" })
map("n", "<leader>fD", function() require("my.telescope").find_dotfiles() end, { desc = "Dotfiles" })

map("n", "<leader>sp", "<Cmd>Lazy<Cr>")
map("n", "<leader>sm", "<Cmd>Mason<Cr>")
map("n", "<leader>sn", "<Cmd>NullLsInfo<Cr>")
map("n", "<leader>sc", "<Cmd>CmpStatus<Cr>")
-- stylua: ignore end
