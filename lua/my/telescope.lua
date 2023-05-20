local M = {}

-- Custom
M.find_vim_files = function()
  require("telescope.builtin").find_files {
    prompt_title = "Vim files",
    cwd = "$HOME/.config/nvim",
    find_command = { "git", "ls-files" },
  }
end

M.find_vim_data = function()
  require("telescope.builtin").find_files {
    prompt_title = "Vim plugin",
    -- cwd = "$XDG_DATA_HOME/nvim/site",
    -- find_command = { "find", "-maxdepth", "4", "-path", "**/**/**/**/**", "-type", "d" },
    cwd = "$XDG_DATA_HOME/nvim/lazy",
    find_command = { "find", "-type", "f" },
  }
end

M.find_dotfiles = function()
  local dotfile_dir = vim.env.DOTFILES
  local home_dir = vim.env.HOME
  require("telescope.builtin").find_files {
    prompt_title = "Dot files",
    cwd = "$HOME",
    find_command = { "git", "--git-dir=" .. dotfile_dir, "--work-tree=" .. home_dir, "ls-files" },
  }
end

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

M.todo_comments = function()
  require("telescope.builtin").grep_string {
    prompt_title = "Todo comments",
    use_regex_boolean = true,
    search = todo_patterns,
  }
end

return M
