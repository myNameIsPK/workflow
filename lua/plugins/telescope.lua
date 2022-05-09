local M = {}

function M.setup()
  local status_ok, actions = pcall(require, "telescope.actions")
  if not status_ok then
    return
  end

  local default_opts = require "telescope.themes".get_ivy {
    vimgrep_arguments = { -- default
      "rg",
      "--color=never",
      "--no-heading",
      "--with-filename",
      "--line-number",
      "--column",
      "--smart-case",
    },
    -- prompt_position = "bottom",
    -- prompt_prefix = " ",
    -- selection_caret = " ",
    -- entry_prefix = "  ",
    -- initial_mode = "insert",
    -- selection_strategy = "reset",
    -- sorting_strategy = "ascending",
    -- layout_strategy = "bottom_pane",
    -- layout_defaults = {
    --   horizontal = {
    --     mirror = false,
    --   },
    --   vertical = {
    --     mirror = false,
    --   },
    -- },
    -- file_sorter = require"telescope.sorters".get_fzy_file
    -- file_ignore_patterns = {},
    -- generic_sorter =  require'telescope.sorters'.get_generic_fuzzy_sorter,
    -- shorten_path = true,
    -- winblend = 10,
    -- width = 0.7,
    -- preview_cutoff = 120,
    -- results_height = 1,
    -- results_width = 0.8,
    -- border = false,
    -- borderchars = { '─', '│', '─', '│', '╭', '╮', '╯', '╰' },
    -- borderchars = { "─", "│", "─", "│", "┌", "┐", "┘", "└" },
    -- prompt = { "─", "│", " ", "│", "┌", "┐", "│", "│" },
    -- results = { "─", "│", "─", "│", "├", "┤", "┘", "└" },
    -- preview = { "─", "│", "─", "│", "┌", "┐", "┘", "└" }
    -- ivy theme
    borderchars = {
      prompt = { "─", " ", " ", " ", "─", "─", " ", " " },
      results = { " " },
      preview = { " ", " ", " ", "│", "│", " ", " ", "│" },
    },
    dynamic_preview_title = true,
    -- color_devicons = true,
    -- use_less = true,
    -- set_env = { ['COLORTERM'] = 'truecolor' }, -- default = nil,
    -- file_previewer = require'telescope.previewers'.vim_buffer_cat.new,
    -- grep_previewer = require'telescope.previewers'.vim_buffer_vimgrep.new,
    -- qflist_previewer = require'telescope.previewers'.vim_buffer_qflist.new,

    -- -- Developer configurations: Not meant for general override
    -- buffer_previewer_maker = require'telescope.previewers'.buffer_previewer_maker,
    mappings = {
      i = {
        ["<C-n>"] = actions.move_selection_next,
        ["<C-p>"] = actions.move_selection_previous,

        ["<C-j>"] = actions.cycle_history_next,
        ["<C-k>"] = actions.cycle_history_prev,

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
    },
  }

  local telescope = require "telescope"
  telescope.setup {
    -- TODO: Not Used ?
    extensions = {
      -- fzy_native = {
      --   override_generic_sorter = false,
      --   override_file_sorter = true,
      -- },
      fzf = {
        fuzzy = true, -- false will only do exact matching
        override_generic_sorter = true, -- override the generic sorter
        override_file_sorter = true, -- override the file sorter
        case_mode = "smart_case", -- or "ignore_case" or "respect_case"
      },
      ["ui-select"] = require("telescope.themes").get_cursor(),
    },
    defaults = default_opts,
  }

  -- Load Extensions
  telescope.load_extension "fzf"
  telescope.load_extension "projects"
  telescope.load_extension "ui-select"
  telescope.load_extension "zk"
end

local function gen_picker(picker, default_opts)
  return function(opts)
    opts = opts or {}
    opts = vim.tbl_deep_extend("force", default_opts, opts)
    picker(opts)
  end
end

-- Custom
local builtin = require("telescope.builtin")
M.find_vim_files = gen_picker(
  builtin.find_files,
  {
    prompt_title = "Vim files",
    cwd = "$HOME/.config/nvim",
    find_command = { "git", "ls-files" },
  }
)

M.find_vim_data = gen_picker(
  builtin.find_files,
  {
    prompt_title = "Vim datas",
    cwd = "$XDG_DATA_HOME/nvim",
    find_command = { "find", "-type", "f" },
  }
)

local dotfile_dir = vim.env.DOTFILES
local home_dir = vim.env.HOME
M.find_dotfiles = gen_picker(
  builtin.find_files,
  {
    prompt_title = "Dot files",
    cwd = "$HOME",
    find_command = { "git", "--git-dir=" .. dotfile_dir, "--work-tree=" .. home_dir, "ls-files" },
  }
)


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
todo_patterns = todo_patterns:sub(2,todo_patterns:len())

M.todo_comments = gen_picker(
  builtin.grep_string,
  {
    prompt_title = "Todo comments",
    use_regex_boolean = true,
    search = todo_patterns,
 }
)

return M
