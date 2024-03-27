-- stylua: ignore start

-- map("i", { "jk", "kj", "jj", "kk" }, "<Esc>")

-- like in emacs keybinds <c-n>,<c-p> is cursor up,down
-- this make <c-f>,<c-b> is <right>,<left> in emacs too
-- if you want full page up,down use <PageUp/Dn> instead
my.map("n", "<c-f>", "<right>")
my.map("n", "<c-b>", "<left>")

my.map("i", "<M-Down>", "<Esc>:m .+1<CR>==gi")
my.map("i", "<M-Up>", "<Esc>:m .-2<CR>==gi")

my.map({ "v", "x" } , "<M-Up>", ":m '<-2<CR>gv=gv")
my.map({ "v", "x" } , "<M-Down>", ":m '>+1<CR>gv=gv")

my.map("n", "<M-Down>", ":m .+1<CR>==")
my.map("n", "<M-Up>", ":m .-2<CR>==")

my.map("i", ",", ",<c-g>u")
my.map("i", ".", ".<c-g>u")
my.map("i", "!", "!<c-g>u")
my.map("i", "?", "?<c-g>u")

-- map("i", "<C-j>", 'pumvisible() ? "\\<C-n>" : "\\<C-j>"', { expr = true, noremap = true })
-- map("i", "<C-k>", 'pumvisible() ? "\\<C-p>" : "\\<C-k>"', { expr = true, noremap = true })

my.map("n", "<Up>", "g<Up>")
my.map("n", "<Down>", "g<Down>")

-- map("n", "<C-h>", "<C-w>h")
-- map("n", "<C-j>", "<C-w>j")
-- map("n", "<C-k>", "<C-w>k")
-- map("n", "<C-l>", "<C-w>l")

-- map("n", "<C-Up>", ":resize -2<CR>")
-- map("n", "<C-Down>", ":resize +2<CR>")
-- map("n", "<C-Left>", ":vertical resize -2<CR>")
-- map("n", "<C-Right>", ":vertical resize +2<CR>")

my.map("n", { "<M-]>", "]b" }, ":bn<Cr>")
my.map("n", { "<M-[>", "[b" }, ":bp<Cr>")
my.map("n", "]a", "<Cmd>next<Cr>")
my.map("n", "[a", "<Cmd>prev<Cr>")
my.map("n", "]A", "<Cmd>last<Cr>")
my.map("n", "[A", "<Cmd>first<Cr>")
my.map("n", "]q", "<Cmd>cnext<Cr>")
my.map("n", "[q", "<Cmd>cprev<Cr>")
my.map("n", "]Q", "<Cmd>clast<Cr>")
my.map("n", "[Q", "<Cmd>cfirst<Cr>")
my.map("n", "]l", "<Cmd>lnext<Cr>")
my.map("n", "[l", "<Cmd>lprev<Cr>")
my.map("n", "]L", "<Cmd>llast<Cr>")
my.map("n", "[L", "<Cmd>lfirst<Cr>")
my.map("n", "]<Space>", "mzo<Esc>`z")
my.map("n", "[<Space>", "mzO<Esc>`z")

local nohl_delay_timer
if vim.fn.has "nvim-0.10.0" == 1 then
  nohl_delay_timer = vim.uv.new_timer()
else
  nohl_delay_timer = vim.loop.new_timer()
end
--- @param key string Keymap use to send delayed `:nohlsearch`
local function delay_nohl(key)
  vim.api.nvim_feedkeys(key, 'n', false)
  nohl_delay_timer:stop()
  nohl_delay_timer = vim.defer_fn(function() vim.cmd.nohlsearch() end, 1000)
end

my.map("n", "*", function() delay_nohl("*") end)
my.map("n", "#", function() delay_nohl("#") end)
my.map("n", "n", function() delay_nohl("nzzzv") end)
my.map("n", "N", function() delay_nohl("Nzzzv") end)

my.map("n", "J", "mzJ`z")

-- [count] j/k become jump motion
my.map("n", "j", "(v:count > 1 ? \"m'\" . v:count : '') . 'j'", { expr = true })
my.map("n", "k", "(v:count > 1 ? \"m'\" . v:count : '') . 'k'", { expr = true })

my.map("v", "<", "<gv")
my.map("v", ">", ">gv")

my.map("v", "p", '"_dP') -- not yank before paste in visual select
-- map("n", "<del>", '"_') -- send all delete to null on every opration
my.map("n", "<M-y>", '"+]p', {desc="Past from clipboard"})
my.map({"n","v"}, "<leader>y", '"+y', {desc="yank to Clipboard"})
my.map({"n","v"}, "<leader>Y", '"+y$' , {desc="yank to Clipboard"})
my.map("n", "<leader><C-y>", '<Cmd>let @+=@"<Cr>', {desc = "Send last yank to Clipboard"})

my.map("t", "<c-g>", "<C-\\><C-n>")

-- map("t", "<C-h>", "<C-\\><C-n><C-w>h")
-- map("t", "<C-j>", "<C-\\><C-n><C-w>j")
-- map("t", "<C-k>", "<C-\\><C-n><C-w>k")
-- map("t", "<C-l>", "<C-\\><C-n><C-w>l")

my.map("c", "<C-a>", "<Home>")
-- map("c", "<C-j>", 'pumvisible() ? "\\<C-n>" : "\\<C-j>"', { expr = true, noremap = true })
-- map("c", "<C-k>", 'pumvisible() ? "\\<C-p>" : "\\<C-k>"', { expr = true, noremap = true })

my.map("n", "<leader>h", "<Nop>", {desc = "Help Prefix"})
my.map("n", "<leader>p", "<Nop>", {desc = "Projects Prefix"})
my.map("n", "<leader>f", "<Nop>", {desc = "File Prefix"})
my.map("n", "<leader>b", "<Nop>", {desc = "Buffer Prefix"})
my.map("n", "<leader>s", "<Nop>", {desc = "Search Prefix"})
my.map("n", "<leader>i", "<Nop>", {desc = "Insert Prefix"})
my.map("n", "<leader>j", "<Nop>", {desc = "Jump Prefix"})
my.map("n", "<leader>c", "<Nop>", {desc = "Coding Prefix"})
my.map("n", "<leader>g", "<Nop>", {desc = "Git Prefix"})
my.map("n", "<leader>t", "<Nop>", {desc = "Toggle & Setting Prefix"})
my.map("n", "<leader>n", "<Nop>", {desc = "Note Prefix"})
my.map("n", "<leader>d", "<Nop>", {desc = "Diagnostic Prefix"})

-- See `:help vim.diagnostic.*` for documentation on any of the below functions
my.map('n', '<leader>de', function() vim.diagnostic.open_float() end, { desc = "Diagnostic Float" })
my.map('n', '[d', function() vim.diagnostic.goto_prev() end, { desc = "Diagnostic Prev" })
my.map('n', '<leader>dc', function() vim.diagnostic.setloclist() end, { desc = "Diagnostic Loclist" })
my.map('n', ']d', function() vim.diagnostic.goto_next() end, { desc = "Diagnostic Next" })

my.map("n", "<leader>pd", vim.cmd.Ex, { desc = "Projects Directory"})

my.map("n", "<leader>bd", vim.cmd.bdelete, { desc = "Buffer delete" })

my.map("n", "<leader>hI", vim.cmd.Inspect, { desc = "Inspect" })

my.map("n", "<leader>tit", vim.cmd.InspectTree, { desc = "Open InspectTree" })
my.map("n", "<leader>tsh", vim.cmd.terminal, { desc = "Terminal" })
my.map("n", "<leader>tlf", function() vim.cmd.terminal("lf") end, { desc = "LF in terminal" })
my.map("n", "<leader>tsp", function() vim.opt.spell = not(vim.opt.spell:get()) end, { desc = "Toggle Spell" })
my.map("n", "<leader>tq", function() vim.cmd.QfToggle() end, { desc = "Toggle Quickfix window" })
my.map("n", "<leader>twk", function() require("which-key") end, { desc = "Enable Which Key" })

my.map("n", "<leader>rf", "<cmd>lua my.save_and_exec()<CR>", { desc = "save and eval file" })
my.map("n", "<leader>ra", "<cmd>lua require('my.plugin.reload').reload_all()<CR>", { desc = "BREAK!! reload all plugins"})

-- Telescope
-- Builtin
my.map("n", "<leader>ta", "<Cmd>Telescope builtin<Cr>", { desc = "All Builtin" })
my.map("n", "<leader>tr", "<Cmd>Telescope resume<Cr>", { desc = "Resume Picker" })
my.map("n", "<leader>tft", "<Cmd>Telescope filetypes<Cr>", { desc = "Set filetypes" })
my.map("n", "<leader>ff", "<Cmd>Telescope fd hidden=true<Cr>", { desc = "Find Files" })
my.map("n", "<leader>fg", "<Cmd>Telescope live_grep<Cr>", { desc = "Files Grep" })
my.map("n", "<leader>fr", "<Cmd>Telescope oldfiles<Cr>", { desc = "Recent Files" })
my.map("n", "<leader>fl", "<Cmd>Telescope current_buffer_fuzzy_find<Cr>", { desc = "Find Lines" })
my.map("n", "<leader>ft", "<Cmd>Telescope current_buffer_tags<Cr>", { desc = "Find Tags" })
my.map("n", "<leader>hh", "<Cmd>Telescope help_tags<Cr>", { desc = "Helps" })
my.map("n", "<leader>ha", "<Cmd>Telescope help_tags<Cr>", { desc = "Helps" })
my.map("n", "<leader>hm", "<Cmd>Telescope man_pages<Cr>", { desc = "Man pages" })
my.map("n", "<leader>hk", "<Cmd>Telescope keymaps<Cr>", { desc = "Keymaps" })
my.map("n", "<leader>hH", "<Cmd>Telescope highlights<Cr>", { desc = "Highlight Fonts" })
my.map("n", "<leader>hv", "<Cmd>Telescope vim_options<Cr>", { desc = "Vim Options" })
my.map("n", "<leader>bb", "<Cmd>Telescope buffers<Cr>", { desc = "Buffers" })
my.map("n", "<leader>qq", "<Cmd>Telescope quickfix<Cr>", { desc = "Quickfix" })
my.map("n", "<leader>qh", "<Cmd>Telescope quickfixhistory<Cr>", { desc = "Quickfix History" })
my.map('n', '<leader>db', '<cmd>Telescope diagnostics bufnr=0<cr>', { desc = "Diagnostics Buffer" })
my.map('n', '<leader>dw', '<cmd>Telescope diagnostics<cr>', { desc = "Diagnostics Workspace" })
my.map("n", "<leader>pf", "<Cmd>Telescope git_files<Cr>", { desc = "Projects(Git) Files" })
my.map("n", "<leader>pg", "<Cmd>Telescope live_grep<Cr>", { desc = "Projects Grep" })
my.map("n", "<leader>ps", "<Cmd>Telescope git_status<Cr>", { desc = "Projects(Git) Status" })
-- Loaded Extensions
my.map("n", "<leader>pp", "<Cmd>Telescope projects<Cr>", { desc = "Projects Picker" })
-- map("n", "<leader>fl", "<Cmd>Telescope bibtex<Cr>", { desc = "Bib(La)tex" })
-- Custom
my.map("n", "<leader>fc", require("my.plugin.telescope").find_vim_files, { desc = "Config files(Neovim)" })
my.map("n", "<leader>fP", require("my.plugin.telescope").find_vim_data, { desc = "Plugins files(Neovim)" })
my.map("n", "<leader>fD", require("my.plugin.telescope").find_dotfiles, { desc = "Dotfiles" })

my.map("n", "<leader>hp", "<Cmd>Lazy<Cr>")
my.map("n", "<leader>hl", "<Cmd>LspInfo<Cr>")
my.map("n", "<leader>hM", "<Cmd>Mason<Cr>")
my.map("n", "<leader>hn", "<Cmd>NullLsInfo<Cr>")
my.map("n", "<leader>hc", "<Cmd>CmpStatus<Cr>")
-- stylua: ignore end
