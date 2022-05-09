local map = require("utils.mappings").map
local plugin_installed = require("utils.plugins").plugin_installed

map("i", { "jk", "kj", "jj", "kk" }, "<Esc>")

map("i", "<M-j>", "<Esc>:m .+1<CR>==gi")
map("i", "<M-k>", "<Esc>:m .-2<CR>==gi")

map("i", ",", ",<c-g>u")
map("i", ".", ".<c-g>u")
map("i", "!", "!<c-g>u")
map("i", "?", "?<c-g>u")

map("i", "<C-j>", 'pumvisible() ? "\\<C-n>" : "\\<C-j>"', { expr = true, noremap = true })
map("i", "<C-k>", 'pumvisible() ? "\\<C-p>" : "\\<C-k>"', { expr = true, noremap = true })

map("n", "j", "gj")
map("n", "k", "gk")

-- map("n", "<C-h>", "<C-w>h")
-- map("n", "<C-j>", "<C-w>j")
-- map("n", "<C-k>", "<C-w>k")
-- map("n", "<C-l>", "<C-w>l")

map("n", "<C-Up>", ":resize -2<CR>")
map("n", "<C-Down>", ":resize +2<CR>")
map("n", "<C-Left>", ":vertical resize -2<CR>")
map("n", "<C-Right>", ":vertical resize +2<CR>")

map("n", "<M-j>", ":m .+1<CR>==")
map("n", "<M-k>", ":m .-2<CR>==")

map("n", "[b", "<Cmd>bp<Cr>")
map("n", "]b", "<Cmd>bn<Cr>")

map("n", "n", "nzzzv")
map("n", "N", "Nzzzv")
map("n", "J", "mzJ`z")

map("n", "<expr> j", "(v:count > 1 ? \"m'\" . v:count : '') . 'j'")
map("n", "<expr> k", "(v:count > 1 ? \"m'\" . v:count : '') . 'k'")

map("v", "<", "<gv")
map("v", ">", ">gv")

map("v", "K", ":m '<-2<CR>gv=gv") -- FIXME: concurent with lsp hover
map("v", "J", ":m '>+1<CR>gv=gv")
map("v", "<M-k>", ":m '<-2<CR>gv=gv")
map("v", "<M-j>", ":m '>+1<CR>gv=gv")

map("x", "K", ":m '<-2<CR>gv=gv") -- FIXME: concurent with lsp hover
map("x", "J", ":m '>+1<CR>gv=gv")
map("x", "<M-k>", ":m '<-2<CR>gv=gv")
map("x", "<M-j>", ":m '>+1<CR>gv=gv")

map("v", "p", '"_dP') -- not yank before paste in visual select

-- map("t", "<C-w><C-o>", "<C-\\><C-n> :MaximizerToggle!<CR>")
map("t", "jk", "<C-\\><C-n>")

-- map("t", "<C-h>", "<C-\\><C-n><C-w>h")
-- map("t", "<C-j>", "<C-\\><C-n><C-w>j")
-- map("t", "<C-k>", "<C-\\><C-n><C-w>k")
-- map("t", "<C-l>", "<C-\\><C-n><C-w>l")

map("c", "<C-a>", "<Home>")
map("c", "<C-j>", 'pumvisible() ? "\\<C-n>" : "\\<C-j>"', { expr = true, noremap = true })
map("c", "<C-k>", 'pumvisible() ? "\\<C-p>" : "\\<C-k>"', { expr = true, noremap = true })

-- disable because sandwich.vim, `s` could be replaced by `cl`
map({ "n", "x" }, "s", "<Nop>")

-- Telescope
if plugin_installed "telescope.nvim" then
  -- Builtin
  map("n", "<leader>fa", "<Cmd>Telescope builtin<Cr>", { desc = "All Builtin" })
  map("n", "<leader>ff", "<Cmd>Telescope find_files<Cr>", { desc = "Files" })
  map("n", "<leader>fg", "<Cmd>Telescope live_grep<Cr>", { desc = "Grep Files" })
  map("n", "<leader>fh", "<Cmd>Telescope help_tags<Cr>", { desc = "Buffers" })
  map("n", "<leader>fb", "<Cmd>Telescope buffers<Cr>", { desc = "Help Tags" })
  map("n", "<leader>fr", "<Cmd>Telescope oldfiles<Cr>", { desc = "Recent Files" })

  -- Loaded Extensions
  map("n", "<leader>fp", "<Cmd>Telescope projects<Cr>", { desc = "Projects" })

  -- Custom
  map("n", "<leader>fc", require("plugins.telescope").find_vim_files, { desc = "Config files(Neovim)" })
  map("n", "<leader>fC", require("plugins.telescope").find_vim_data, { desc = "Data files(Neovim)" })
  map("n", "<leader>fD", require("plugins.telescope").find_dotfiles, { desc = "Dotfiles" })
  map("n", "<leader>td", require("plugins.telescope").todo_comments, { desc = "TODO comments" })

  local wk_ok, wk = pcall(require, "which-key")
  if wk_ok then
    wk.register {
      ["<leader>f"] = "+Telescope",
      ["<leader>d"] = "Diagnostics", -- in diagnostic.lua
      ["<leader>l"] = "LSP", -- in lsp_handlers.lua
      ["<leader>h"] = "Hunk(gitsigns)", -- in gitsigns.lua
    }
  end
end

if plugin_installed("zk-nvim") then
  map("n", "<leader>zf", "<Cmd>ZkNotes<Cr>")
  map("n", "<leader>zd", "<Cmd>ZkNew { dir = 'journal/daily' }<Cr>")
end

if plugin_installed("neorg") then
  map("n", "<leader>ng", "<Cmd>NeorgStart<Cr>")
end
