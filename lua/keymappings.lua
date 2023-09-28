-- stylua: ignore start
local map = require("my.map-helper").map

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
map("n", "]a", "<Cmd>next<Cr>")
map("n", "[a", "<Cmd>prev<Cr>")
map("n", "]A", "<Cmd>last<Cr>")
map("n", "[A", "<Cmd>first<Cr>")
map("n", "]q", "<Cmd>cnext<Cr>")
map("n", "[q", "<Cmd>cprev<Cr>")
map("n", "]Q", "<Cmd>clast<Cr>")
map("n", "[Q", "<Cmd>cfirst<Cr>")
map("n", "]l", "<Cmd>lnext<Cr>")
map("n", "[l", "<Cmd>lprev<Cr>")
map("n", "]L", "<Cmd>llast<Cr>")
map("n", "[L", "<Cmd>lfirst<Cr>")

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
map("n", "<M-y>", '"+]p', {desc="Past from clipboard"})
map({"n","v"}, "<leader>y", '"+y', {desc="yank to Clipboard"})
map({"n","v"}, "<leader>Y", '"+y$' , {desc="yank to Clipboard"})
map("n", "<leader><C-y>", '<Cmd>let @+=@"<Cr>', {desc = "Send last yank to Clipboard"})

map("t", "<c-g>", "<C-\\><C-n>")

-- map("t", "<C-h>", "<C-\\><C-n><C-w>h")
-- map("t", "<C-j>", "<C-\\><C-n><C-w>j")
-- map("t", "<C-k>", "<C-\\><C-n><C-w>k")
-- map("t", "<C-l>", "<C-\\><C-n><C-w>l")

map("c", "<C-a>", "<Home>")
-- map("c", "<C-j>", 'pumvisible() ? "\\<C-n>" : "\\<C-j>"', { expr = true, noremap = true })
-- map("c", "<C-k>", 'pumvisible() ? "\\<C-p>" : "\\<C-k>"', { expr = true, noremap = true })

map("n", "<leader>h", "<Nop>", {desc = "Help Prefix"})
map("n", "<leader>p", "<Nop>", {desc = "Projects Prefix"})
map("n", "<leader>f", "<Nop>", {desc = "File Prefix"})
map("n", "<leader>b", "<Nop>", {desc = "Buffer Prefix"})
map("n", "<leader>s", "<Nop>", {desc = "Search Prefix"})
map("n", "<leader>i", "<Nop>", {desc = "Insert Prefix"})
map("n", "<leader>c", "<Nop>", {desc = "Coding Prefix"})
map("n", "<leader>g", "<Nop>", {desc = "Git Prefix"})
map("n", "<leader>t", "<Nop>", {desc = "Toggle & Setting Prefix"})
map("n", "<leader>n", "<Nop>", {desc = "Note Prefix"})
map("n", "<leader>d", "<Nop>", {desc = "Diagnostic Prefix"})

-- See `:help vim.diagnostic.*` for documentation on any of the below functions
map('n', '<leader>de', function() vim.diagnostic.open_float() end, { desc = "Diagnostic Float" })
map('n', '[d', function() vim.diagnostic.goto_prev() end, { desc = "Diagnostic Prev" })
map('n', '<leader>dc', function() vim.diagnostic.setloclist() end, { desc = "Diagnostic Loclist" })
map('n', ']d', function() vim.diagnostic.goto_next() end, { desc = "Diagnostic Next" })

map("n", "<leader>pd", vim.cmd.Ex, { desc = "Projects Directory"})

map("n", "<leader>hI", vim.cmd.Inspect, { desc = "Inspect" })

map("n", "<leader>tsh", vim.cmd.terminal, { desc = "Terminal" })
map("n", "<leader>tlf", function() vim.cmd.terminal("lf") end, { desc = "LF in terminal" })
map("n", "<leader>tsp", function() vim.opt.spell = not(vim.opt.spell:get()) end, { desc = "Toggle Spell" })
map("n", "<leader>tq", function() vim.cmd.QfToggle() end, { desc = "Toggle Quickfix window" })
map("n", "<leader>twk", function() require("which-key") end, { desc = "Enable Which Key" })

map("n", "<leader>rf", "<cmd>lua my.save_and_exec()<CR>", { desc = "save and eval file" })
map("n", "<leader>ra", "<cmd>lua require('my.plugin.reload').reload_all()<CR>", { desc = "BREAK!! reload all plugins"})

-- Telescope
-- Builtin
map("n", "<leader>ta", "<Cmd>Telescope builtin<Cr>", { desc = "All Builtin" })
map("n", "<leader>tr", "<Cmd>Telescope resume<Cr>", { desc = "Resume Picker" })
map("n", "<leader>ff", "<Cmd>Telescope fd hidden=true<Cr>", { desc = "Files Grep" })
map("n", "<leader>fg", "<Cmd>Telescope live_grep<Cr>", { desc = "Files Grep" })
map("n", "<leader>fr", "<Cmd>Telescope oldfiles<Cr>", { desc = "Recent Files" })
map("n", "<leader>fl", "<Cmd>Telescope current_buffer_fuzzy_find<Cr>", { desc = "Find Lines" })
map("n", "<leader>ft", "<Cmd>Telescope current_buffer_tags<Cr>", { desc = "Find Tags" })
map("n", "<leader>hh", "<Cmd>Telescope help_tags<Cr>", { desc = "Helps" })
map("n", "<leader>ha", "<Cmd>Telescope help_tags<Cr>", { desc = "Helps" })
map("n", "<leader>hm", "<Cmd>Telescope man_pages<Cr>", { desc = "Man pages" })
map("n", "<leader>hk", "<Cmd>Telescope keymaps<Cr>", { desc = "Keymaps" })
map("n", "<leader>hH", "<Cmd>Telescope highlights<Cr>", { desc = "Highlight Fonts" })
map("n", "<leader>hv", "<Cmd>Telescope vim_options<Cr>", { desc = "Vim Options" })
map("n", "<leader>bb", "<Cmd>Telescope buffers<Cr>", { desc = "Buffers" })
map("n", "<leader>qq", "<Cmd>Telescope quickfix<Cr>", { desc = "Quickfix" })
map("n", "<leader>qh", "<Cmd>Telescope quickfixhistory<Cr>", { desc = "Quickfix History" })
map('n', '<leader>db', '<cmd>Telescope diagnostics bufnr=0<cr>', { desc = "Diagnostics Buffer" })
map('n', '<leader>dw', '<cmd>Telescope diagnostics<cr>', { desc = "Diagnostics Workspace" })
map("n", "<leader>pf", "<Cmd>Telescope git_files<Cr>", { desc = "Projects Files" })
map("n", "<leader>pg", "<Cmd>Telescope live_grep<Cr>", { desc = "Projects Grep" })
-- Loaded Extensions
map("n", "<leader>pp", "<Cmd>Telescope projects<Cr>", { desc = "Projects Picker" })
-- map("n", "<leader>fl", "<Cmd>Telescope bibtex<Cr>", { desc = "Bib(La)tex" })
-- Custom
map("n", "<leader>fc", require("my.plugin.telescope").find_vim_files, { desc = "Config files(Neovim)" })
map("n", "<leader>fP", require("my.plugin.telescope").find_vim_data, { desc = "Plugins files(Neovim)" })
map("n", "<leader>fD", require("my.plugin.telescope").find_dotfiles, { desc = "Dotfiles" })

map("n", "<leader>hp", "<Cmd>Lazy<Cr>")
map("n", "<leader>hl", "<Cmd>LspInfo<Cr>")
map("n", "<leader>hM", "<Cmd>Mason<Cr>")
map("n", "<leader>hn", "<Cmd>NullLsInfo<Cr>")
map("n", "<leader>hc", "<Cmd>CmpStatus<Cr>")
-- stylua: ignore end
