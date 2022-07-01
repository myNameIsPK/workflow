local status_ok, orgmode = pcall(require, "orgmode")
if not status_ok then
  return
end

orgmode.setup_ts_grammar()

-- require("nvim-treesitter.configs").setup {
--   -- If TS highlights are not enabled at all, or disabled via `disable` prop, highlighting will fallback to default Vim syntax highlighting
--   highlight = {
--     enable = true,
--     -- TODO: treesitter orgmode
--     disable = { "org" }, -- Remove this to use TS highlighter for some of the highlights (Experimental)
--     additional_vim_regex_highlighting = { "org" }, -- Required since TS highlighter doesn't support all syntax features (conceal)
--   },
--   -- FIX: treesitter load again when open filetype
--   ensure_installed = { "org" }, -- Or run :TSUpdate org
-- }

orgmode.setup {
  org_agenda_files = { "~/notes/org/*", "~/orgs/**/*" },
  org_default_notes_file = "~/notes/org/notes.org",
  org_todo_keywords = {'TODO(t)', 'NEXT(n)', '|', 'DONE(d)'}
}
