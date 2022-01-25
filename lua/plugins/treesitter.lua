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
