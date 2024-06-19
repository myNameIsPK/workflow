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
  require("telescope.builtin").find_files {
    prompt_title = "Dotfiles",
    cwd = vim.env.DOTFILES,
    hidden = true,
  }
end

return M
