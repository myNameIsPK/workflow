local M = {}

function M.setup()

  local status_ok, actions = pcall(require, "telescope.actions")
  if not status_ok then
    return
  end

  require("telescope").load_extension("projects")

  require("telescope").setup {
    -- TODO: Not Used ?
    -- extensions = {
    --   fzy_native = {
    --     override_generic_sorter = false,
    --     override_file_sorter = true,
    --   },
    -- },
    defaults = {
      -- vimgrep_arguments = {
      --   'rg',
      --   '--color=never',
      --   '--no-heading',
      --   '--with-filename',
      --   '--line-number',
      --   '--column',
      --   '--smart-case'
      -- },
      -- prompt_position = "bottom",
      -- prompt_prefix = " ",
      -- selection_caret = " ",
      -- entry_prefix = "  ",
      -- initial_mode = "insert",
      -- selection_strategy = "reset",
      -- sorting_strategy = "descending",
      -- layout_strategy = "horizontal",
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
      -- border = {},
      -- borderchars = { '─', '│', '─', '│', '╭', '╮', '╯', '╰' },
      -- borderchars = { "─", "│", "─", "│", "┌", "┐", "┘", "└" },
      -- prompt = { "─", "│", " ", "│", "┌", "┐", "│", "│" },
      -- results = { "─", "│", "─", "│", "├", "┤", "┘", "└" },
      -- preview = { "─", "│", "─", "│", "┌", "┐", "┘", "└" }
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
          ["<C-c>"] = actions.close,
          ["<C-j>"] = actions.cycle_history_next,
          ["<C-k>"] = actions.cycle_history_prev,
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
          ["<C-c>"] = actions.close,
          ["<C-q>"] = actions.smart_send_to_qflist + actions.open_qflist,
          -- ["<c-t>"] = trouble.open_with_trouble,
          -- ["<C-i>"] = my_cool_custom_action,
        },
      },
    },
  }
end

-- -- TODO: Delete this ?
-- function M.find_vim_files(opts)
--   opts = opts or {}
--   local themes = require "telescope.themes"
--   local theme_opts = themes.get_ivy {
--     sorting_strategy = "ascending",
--     layout_strategy = "bottom_pane",
--     prompt = ">> ",
--     prompt_title = "~ Vim files ~",
--     cwd = "$HOME/.config/nvim",
--     find_command = { "git", "ls-files" },
--   }
--   opts = vim.tbl_deep_extend("force", theme_opts, opts)
--   require("telescope.builtin").find_files(opts)
-- end

-- function M.grep_vim_files(opts)
--   opts = opts or {}
--   local themes = require "telescope.themes"
--   local theme_opts = themes.get_ivy {
--     sorting_strategy = "ascending",
--     layout_strategy = "bottom_pane",
--     prompt = ">> ",
--     prompt_title = "~ search Vim ~",
--     cwd = "$HOME/.config/nvim",
--   }
--   opts = vim.tbl_deep_extend("force", theme_opts, opts)
--   require("telescope.builtin").live_grep(opts)
-- end

return M
