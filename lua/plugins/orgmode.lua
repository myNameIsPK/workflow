local M = {}

function M.setup()
  local status_ok, orgmode = pcall(require, "orgmode")
  if not status_ok then
    return
  end

  local parser_config = require("nvim-treesitter.parsers").get_parser_configs()

  parser_config.org = {
    install_info = {
      url = "https://github.com/milisims/tree-sitter-org",
      revision = "f110024d539e676f25b72b7c80b0fd43c34264ef",
      files = { "src/parser.c", "src/scanner.cc" },
    },
    filetype = "org",
  }

  require("nvim-treesitter.configs").setup {
    -- If TS highlights are not enabled at all, or disabled via `disable` prop, highlighting will fallback to default Vim syntax highlighting
    highlight = {
      enable = true,
      -- TODO: treesitter orgmode
      disable = { "org" }, -- Remove this to use TS highlighter for some of the highlights (Experimental)
      additional_vim_regex_highlighting = { "org" }, -- Required since TS highlighter doesn't support all syntax features (conceal)
    },
    -- FIX: treesitter load again when open filetype
    -- ensure_installed = { "org" }, -- Or run :TSUpdate org
  }

  orgmode.setup {
    org_agenda_files = { "~/notes/org/*", "~/orgs/**/*" },
    org_default_notes_file = "~/notes/org/notes.org",
    org_todo_keywords = {'TODO(t)', 'NEXT(n)', '|', 'DONE(d)'}
  }
end

return M
