local opts = { noremap = true, silent = false }

local generic_opts = {
  insert_mode = opts,
  normal_mode = opts,
  visual_mode = opts,
  command_mode = opts,
  visual_block_mode = opts,
  term_mode = { silent = false },
}

local mode_adapters = {
  insert_mode = "i",
  normal_mode = "n",
  term_mode = "t",
  visual_mode = "v",
  visual_block_mode = "x",
  command_mode = "c",
}

local keymappings = {
  insert_mode = {
    ["jk"] = "<Esc>",
    ["kj"] = "<Esc>",
    ["jj"] = "<Esc>",
    ["kk"] = "<Esc>",

    ["<M-j>"] = "<Esc>:m .+1<CR>==gi",
    ["<M-k>"] = "<Esc>:m .-2<CR>==gi",

    [","] = ",<c-g>u",
    ["."] = ".<c-g>u",
    ["!"] = "!<c-g>u",
    ["?"] = "?<c-g>u",

    ["<C-j>"] = {
      'pumvisible() ? "\\<C-n>" : "\\<C-j>"',
      { expr = true, noremap = true },
    },
    ["<C-k>"] = {
      'pumvisible() ? "\\<C-p>" : "\\<C-k>"',
      { expr = true, noremap = true },
    },
  },
  normal_mode = {
    -- ["s"] = {
      -- "<Plug>(easymotion-overwin-f)",
      -- { noremap = false, silent = false },
    -- },
    ["<leader>l"] = "<Cmd>noh<CR>",
    -- ["<C-w><C-o>"] = "<Cmd>MaximizerToggle!<CR>",
    ["<C-h>"] = "<C-w>h",
    ["<C-j>"] = "<C-w>j",
    ["<C-k>"] = "<C-w>k",
    ["<C-l>"] = "<C-w>l",

    ["<C-Up>"] = ":resize -2<CR>",
    ["<C-Down>"] = ":resize +2<CR>",
    ["<C-Left>"] = ":vertical resize -2<CR>",
    ["<C-Right>"] = ":vertical resize +2<CR>",

    ["<M-j>"] = ":m .+1<CR>==",
    ["<M-k>"] = ":m .-2<CR>==",

    ["Y"] = "y$",

    -- ["<S-h>"] = "<Cmd>bp<Cr>",
    -- ["<S-l>"] = "<Cmd>bn<Cr>",

    ["n"] = "nzzzv",
    ["N"] = "Nzzzv",
    ["J"] = "mzJ`z",

    ["<expr> j"] = "(v:count > 1 ? \"m'\" . v:count : '') . 'j'",
    ["<expr> k"] = "(v:count > 1 ? \"m'\" . v:count : '') . 'k'",

    -- Telescope TODO: Move leader to which key
    ["<leader>ff"] = "<Cmd>Telescope find_files<Cr>",
    ["<leader>fg"] = "<Cmd>Telescope live_grep<Cr>",
    ["<leader>fh"] = "<Cmd>Telescope help_tags<Cr>",
    ["<leader>fb"] = "<Cmd>Telescope buffers<Cr>",
    ["<leader>fp"] = "<Cmd>Telescope projects<Cr>",

    -- Packer
    ["<leader>ps"] = "<Cmd>PackerStatus<Cr>",

  },
  visual_mode = {
    ["<"] = "<gv",
    [">"] = ">gv",

    ["K"] = ":m '<-2<CR>gv=gv",
    ["J"] = ":m '>+1<CR>gv=gv",
    ["<M-k>"] = ":m '<-2<CR>gv=gv",
    ["<M-j>"] = ":m '>+1<CR>gv=gv",
  },
  term_mode = {
    -- ["<C-w><C-o>"] = "<C-\\><C-n> :MaximizerToggle!<CR>",
    ["jj"] = "<C-\\><C-n>",

    ["<C-h>"] = "<C-\\><C-n><C-w>h",
    ["<C-j>"] = "<C-\\><C-n><C-w>j",
    ["<C-k>"] = "<C-\\><C-n><C-w>k",
    ["<C-l>"] = "<C-\\><C-n><C-w>l",
  },
  command_mode = {
    ["<C-a>"] = "<Home>",
    ["<C-j>"] = {
      'pumvisible() ? "\\<C-n>" : "\\<C-j>"',
      { expr = true, noremap = true },
    },
    ["<C-k>"] = {
      'pumvisible() ? "\\<C-p>" : "\\<C-k>"',
      { expr = true, noremap = true },
    },
  },
}

-- local lsp_keymappings = {

--   normal_mode = {
--     ["K"] = "<Cmd>lua vim.lsp.buf.hover()<CR>",
--     ["gD"] = "<Cmd>lua vim.lsp.buf.declaration()<CR>",
--     ["gd"] = "<Cmd>lua vim.lsp.buf.definition()<CR>",
--     ["gi"] = "<Cmd>lua vim.lsp.buf.implementation()<CR>",
--     ["<space>ca"] = "<cmd>lua vim.lsp.buf.code_action()<CR>",
--     -- ["<C-k>"] = "<Cmd>lua vim.lsp.buf.signature_help()<CR>",
--     ["[d"] = "<Cmd>lua vim.lsp.diagnostic.goto_prev()<CR>",
--     ["]d"] = "<Cmd>lua vim.lsp.diagnostic.goto_next()<CR>",
--     -- ["[e"] = "<Cmd>Lspsaga diagnostic_jump_next<CR>",
--     -- ["]e"] = "<Cmd>Lspsaga diagnostic_jump_prev<CR>",
--   },
-- }

function set_keymaps(mode, key, val)
  local opt = generic_opts[mode] and generic_opts[mode] or opts
  if type(val) == "table" then
    opt = val[2]
    val = val[1]
  end
  vim.api.nvim_set_keymap(mode, key, val, opt)
end

function map(mode, keymaps)
  mode = mode_adapters[mode] and mode_adapters[mode] or mode
  for k, v in pairs(keymaps) do
    set_keymaps(mode, k, v)
  end
end

for mode, mapping in pairs(keymappings) do
  map(mode, mapping)
end
