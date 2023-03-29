-- stylua: ignore start
local map = require("utils.mappings").map
local plugin_installed = require("utils.plugins").plugin_installed

--- Plaground make it work
map("n", "<leader>rf", "<cmd>lua my.save_and_exec()<CR>")
map("n", "<leader>ra", "<cmd>lua require('utils.reload').reload_all()<CR>")
---

-- map("i", { "jk", "kj", "jj", "kk" }, "<Esc>")

map("i", { "<M-j>", "<M-Down>" }, "<Esc>:m .+1<CR>==gi")
map("i", { "<M-k>", "<M-Up>", }, "<Esc>:m .-2<CR>==gi")

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

map("n", "<C-Up>", ":resize -2<CR>")
map("n", "<C-Down>", ":resize +2<CR>")
map("n", "<C-Left>", ":vertical resize -2<CR>")
map("n", "<C-Right>", ":vertical resize +2<CR>")

map("n", { "<M-j>", "<M-Down>" }, ":m .+1<CR>==")
map("n", { "<M-k>", "<M-Up>" }, ":m .-2<CR>==")

map("n", "]b", "<Cmd>bn<Cr>")
map("n", "[b", "<Cmd>bp<Cr>")
map("n", "]q", "<Cmd>cnext<Cr>")
map("n", "[q", "<Cmd>cprev<Cr>")
map("n", "]l", "<Cmd>lnext<Cr>")
map("n", "[l", "<Cmd>lprev<Cr>")

map("n", "n", "nzzzv")
map("n", "N", "Nzzzv")
map("n", "J", "mzJ`z")

map("n", "<expr> j", "(v:count > 1 ? \"m'\" . v:count : '') . 'j'")
map("n", "<expr> k", "(v:count > 1 ? \"m'\" . v:count : '') . 'k'")

map("v", "<", "<gv")
map("v", ">", ">gv")

map({ "v", "x" } , { "<M-k>", "<M-Up>" }, ":m '<-2<CR>gv=gv")
map({ "v", "x" } , { "<M-j>", "<M-Down>" }, ":m '>+1<CR>gv=gv")

map("v", "p", '"_dP') -- not yank before paste in visual select

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
map('n', '<leader>dq', function() vim.diagnostic.setloclist() end, { desc = "Diagnostic Quixfix" })
map('n', ']d', function() vim.diagnostic.goto_next() end, { desc = "Diagnostic Next" })

map("n", "<leader>gg", "<Cmd>Neogit<Cr>")
map("n", "<leader>sp", "<Cmd>Lazy<Cr>")
map("n", "<leader>sm", "<Cmd>Mason<Cr>")
map("n", "<leader>sn", "<Cmd>NullLsInfo<Cr>")
map("n", "<leader>sc", "<Cmd>CmpStatus<Cr>")
map("n", "<leader>si", vim.cmd.Inspect)

map("n", "<leader>tt", vim.cmd.terminal)
map("n", "<leader>tl", function() vim.cmd.terminal("lf") end)

map("n", "<leader>ee", vim.cmd.Ex)

-- Telescope
if plugin_installed "telescope.nvim" then
  -- Builtin
  map("n", "<leader>fa", "<Cmd>Telescope builtin<Cr>", { desc = "All Builtin" })
  map("n", "<leader>ff", "<Cmd>Telescope find_files<Cr>", { desc = "Files" })
  map("n", "<leader>fg", "<Cmd>Telescope live_grep<Cr>", { desc = "Grep Files" })
  map("n", "<leader>fb", "<Cmd>Telescope buffers<Cr>", { desc = "Buffers" })
  map("n", "<leader>fr", "<Cmd>Telescope oldfiles<Cr>", { desc = "Recent Files" })

  map("n", "<leader>hh", "<Cmd>Telescope help_tags<Cr>", { desc = "Helps" })
  map("n", "<leader>hm", "<Cmd>Telescope man_pages<Cr>", { desc = "Man pages" })

  map('n', '<leader>fdb', '<cmd>Telescope diagnostics bufnr=0<cr>', { desc = "Diagnostics Buffer" })
  map('n', '<leader>fdw', '<cmd>Telescope diagnostics<cr>', { desc = "Diagnostics Workspace" })

  -- Loaded Extensions
  map("n", "<leader>fp", "<Cmd>Telescope projects<Cr>", { desc = "Projects" })

  -- Custom
  map("n", "<leader>fc", function() require("plugins.telescope").find_vim_files() end, { desc = "Config files(Neovim)" })
  map("n", "<leader>fC", function() require("plugins.telescope").find_vim_data() end, { desc = "Plugins files(Neovim)" })
  map("n", "<leader>fD", function() require("plugins.telescope").find_dotfiles() end, { desc = "Dotfiles" })

end

map("n", "<leader>ts", "<Cmd>set spell!<Cr>", { desc = "Spell toggle" })

if plugin_installed("nabla.nvim") then
  map("n", "<leader>te", "<Cmd>lua require('nabla').popup()<Cr>", { desc = "Equation Preview" })
end

if plugin_installed("markdown-preview.nvim") then
  map("n", "<leader>tm", "<Cmd>MarkdownPreviewToggle<Cr>", { desc = "Markdown Preview" })
end

if plugin_installed("neorg") then
  map("n", "<leader>ng", "<Cmd>NeorgStart<Cr>")
end

if plugin_installed("mason-lspconfig.nvim") then
  map("n", "<leader>sl", "<Cmd>LspInfo<Cr>")
end

if plugin_installed("nvim-treesitter") then
  map("n", "<leader>st", "<Cmd>TSModuleInfo<Cr>")
  map("n", "<leader>tp", "<Cmd>TSPlaygroundToggl<Cr>")
end

if plugin_installed("nvim-colorizer.lua") then
  map("n", "<leader>tc", "<Cmd>ColorizerToggle<Cr>")
end

if plugin_installed("indent-blankline.nvim") then
  map("n", "<leader>ti", "<Cmd>IndentBlanklineToggle<Cr>")
end

if plugin_installed("plenary.nvim") then
  map("n", "<leader>td", "<Cmd>w<Cr><Plug>PlenaryTestFile")
end

local wk_ok, wk = pcall(require, "which-key")
if wk_ok then
  wk.register {
    ["<leader>f"] = "Telescope",
    ["<leader>d"] = "Diagnostics", -- in diagnostic.lua
    ["<leader>l"] = "LSP", -- in lsp_handlers.lua
    ["<leader>h"] = "Helps",
    ["<leader>s"] = "Status",
    ["<leader>t"] = "Toggle",
  }
end
-- stylua: ignore end
