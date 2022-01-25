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
      enable = true, -- false will disable the whole extension
      disable = { "" },  -- list of language that will be disabled
      -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
      -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
      -- Using this option may slow down your editor, and you may see some duplicate highlights.
      -- Instead of true it can also be a list of languages
      additional_vim_regex_highlighting = false,
    },

    incremental_selection = {
      -- TODO: Change Keymap
      enable = true,
      -- disable = { "c", "rust" },  -- list of language that will be disabled
      keymaps = {
        init_selection = "gln", -- FIX: edit gnn to init selection(it broke vim default gn)
        node_incremental = "gln",
        node_decremental = "glp",
        scope_incremental = "gsc",
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

    query_linter = { -- linter in query editor buffer in playground
      enable = true,
      use_virtual_text = true,
      lint_events = {"BufWrite", "CursorHold"},
    },

    -- autopairs = {
    --   enable = true,
    -- },
    --
    -- context_commentstring = {
    --   enable = true,
    --   enable_autocmd = false,
    -- },
    --
    -- autotag = {
    --   enable = true,
    -- },
    --
    -- refactor = {
    --   highlight_definitions = { enable = true },
    --   highlight_current_scope = { enable = false },
    -- },
  }
end

return M
