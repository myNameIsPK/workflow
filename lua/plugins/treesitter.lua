local M = {}

function M.setup()
  require"nvim-treesitter.configs".setup {
    ensure_installed = {
      'lua',
      'vim',
      'bash',
      'markdown',
      'yaml',
      'toml',
      'python',
      'make',
      'regex',
      'latex',
      'bibtex',
      'css',
      'html',
      'javascript',
      'jsdoc',
      'json',
      'php',
      'scss',
      'tsx',
      'typescript',
      'query', -- for playground query editor buffer
    }, -- one of "all", "maintained" (parsers with maintainers), or a list of languages
    sync_install = true, -- install languages synchronously (only applied to `ensure_installed`)
    ignore_install = { "" }, -- List of parsers to ignore installing
    highlight = {
      enable = true,              -- false will disable the whole extension
      disable = { "" },  -- list of language that will be disabled
      -- additional_vim_regex_highlighting = true, -- default vim hightlight: should disable
    },
    incremental_selection = {
      -- TODO: Change Keymap
      enable = true,
      -- disable = { "c", "rust" },  -- list of language that will be disabled
      keymaps = {
        init_nelection = "gnn", -- FIX: edit gnn to init selection(it broke vim default gn)
        node_iccremental = "grn",
        node_decremental = "grm",
        scope_incremental = "grc",
      },
    },
    indent = {
      enable = true,
      disable = { "" },  -- list of language that will be disabled
    },
    playground = { -- "nvim-treesitter/playground"
      enable = true,
      disable = {},
      updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
      persist_queries = false, -- Whether the query persists across vim sessions
      keybindings = {
        toggle_query_editor = 'o',
        toggle_hl_groups = 'i',
        toggle_injected_languages = 't',
        toggle_anonymous_nodes = 'a',
        toggle_language_display = 'I',
        focus_language = 'f',
        unfocus_language = 'F',
        update = 'R',
        goto_node = '<cr>',
        show_help = '?',
      },
    },
    query_linter = {
      enable = true,
      use_virtual_text = true,
      lint_events = {"BufWrite", "CursorHold"},
    },
    -- autopairs = {
    --   enable = true,
    -- },
    -- context_commentstring = {
    --   enable = true,
    --   enable_autocmd = false,
    -- },
    -- autotag = {
    --   enable = true,
    -- },
    -- refactor = {
    --   highlight_definitions = { enable = true },
    --   highlight_current_scope = { enable = false },
    -- },
  }
end

return M
