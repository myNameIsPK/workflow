local M = {}

local status_ok, telescope = pcall(require, "telescope")
if not status_ok then
  return
end

local actions = require "telescope.actions"
local action_layout = require "telescope.actions.layout"

local mappings = {
  i = {
    ["<C-n>"] = actions.move_selection_next,
    ["<C-p>"] = actions.move_selection_previous,

    ["<C-j>"] = actions.cycle_history_next,
    ["<C-k>"] = actions.cycle_history_prev,

    ["<C-l>"] = action_layout.cycle_layout_next,
    ["<C-h>"] = action_layout.cycle_layout_prev,

    ["<C-c>"] = actions.close,

    ["<Down>"] = actions.cycle_history_next,
    ["<Up>"] = actions.cycle_history_prev,

    ["<C-q>"] = actions.smart_send_to_qflist + actions.open_qflist,
    ["<CR>"] = actions.select_default + actions.center,
    -- To disable a keymap, put [map] = false
    -- So, to not map "<C-n>", just put
    -- ["<c-t>"] = trouble.open_with_trouble,
    -- ["<c-x>"] = false,
    -- ["<esc>"] = actions.close,
    -- Otherwise, just set the mapping to the function that you want it to be.
    -- ["<C-i>"] = actions.select_horizontal,
    -- Add up multiple actions
    -- You can perform as many actions in a row as you like
    -- ["<CR>"] = actions.select_default + actions.center + my_cool_custom_action,
  },
  n = {
    ["<C-n>"] = actions.move_selection_next,
    ["<C-p>"] = actions.move_selection_previous,

    ["<C-j>"] = actions.move_selection_next,
    ["<C-k>"] = actions.move_selection_previous,

    ["<C-c>"] = actions.close,

    ["<C-q>"] = actions.smart_send_to_qflist + actions.open_qflist,
    -- ["<c-t>"] = trouble.open_with_trouble,
    -- ["<C-i>"] = my_cool_custom_action,
  },
}

telescope.load_extension "fzf"
telescope.load_extension "projects"
telescope.load_extension "ui-select"
telescope.load_extension "zk"

local extensions = {
  fzf = {
    fuzzy = true,
    override_generic_sorter = true,
    override_file_sorter = true,
    case_mode = "smart_case",
  },
  -- ["ui-select"] = require("telescope.themes").get_cursor(),
}

local default_opts = require("telescope.themes").get_ivy {
  -- local default_opts = {
  vimgrep_arguments = { -- default
    "rg",
    "--color=never",
    "--no-heading",
    "--with-filename",
    "--line-number",
    "--column",
    "--smart-case",
  },
  cycle_layout_list = {
    "vertical",
    "horizontal",
    "center",
    "cursor",
    "flex",
    "bottom_pane",
  },
  border = false,
  -- winblend = 15,
  borderchars = { "─", "│", "─", "│", "┌", "┐", "┘", "└" },
  dynamic_preview_title = true,
  results_title = "",
  mappings = mappings,
}

telescope.setup {
  defaults = default_opts,
  extensions = extensions,
  -- pickers = {},
}

local function gen_picker(picker, opt)
  return function(opts)
    opts = opts or {}
    opts = vim.tbl_deep_extend("force", opt, opts)
    picker(opts)
  end
end

-- Custom
local builtin = require "telescope.builtin"
M.find_vim_files = gen_picker(builtin.find_files, {
  prompt_title = "Vim files",
  cwd = "$HOME/.config/nvim",
  find_command = { "git", "ls-files" },
})

M.find_vim_data = gen_picker(builtin.find_files, {
  prompt_title = "Vim plugin",
  cwd = "$XDG_DATA_HOME/nvim/site",
  find_command = { "find", "-maxdepth", "4", "-path", "**/**/**/**/**", "-type", "d" },
})

local dotfile_dir = vim.env.DOTFILES
local home_dir = vim.env.HOME
M.find_dotfiles = gen_picker(builtin.find_files, {
  prompt_title = "Dot files",
  cwd = "$HOME",
  find_command = { "git", "--git-dir=" .. dotfile_dir, "--work-tree=" .. home_dir, "ls-files" },
})

-- TODO: make todo icon, color and only map on comments
local todo_keywords = {
  FIX = {
    icon = " ", -- icon used for the sign, and in search results
    color = "error", -- can be a hex color, or a named color (see below)
    alt = { "FIXME", "BUG", "FIXIT", "ISSUE" }, -- a set of other keywords that all map to this FIX keywords
    -- signs = false, -- configure signs for some keywords individually
  },
  TODO = { icon = " ", color = "info" },
  HACK = { icon = " ", color = "warning" },
  WARN = { icon = " ", color = "warning", alt = { "WARNING", "XXX" } },
  PERF = { icon = " ", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
  NOTE = { icon = " ", color = "hint", alt = { "INFO" } },
}

local todo_patterns = ""
for keyword, values in pairs(todo_keywords) do
  todo_patterns = todo_patterns .. "|" .. keyword
  for _, alt_keyword in ipairs(values.alt or {}) do
    todo_patterns = todo_patterns .. "|" .. alt_keyword
  end
end
-- delete leading "|"
todo_patterns = todo_patterns:sub(2, todo_patterns:len())

M.todo_comments = gen_picker(builtin.grep_string, {
  prompt_title = "Todo comments",
  use_regex_boolean = true,
  search = todo_patterns,
})

return M
