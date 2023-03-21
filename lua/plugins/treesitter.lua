local ok, tsconfigs = pcall(require, "nvim-treesitter.configs")
if not ok then
  return
end

tsconfigs.setup {
  ensure_installed = {
    "lua",
    "vim",
    "bash",
    "markdown",
    "yaml",
    "toml",
    "python",
    "make",
    "regex",
    "latex",
    "bibtex",
    "css",
    "html",
    "javascript",
    "jsdoc",
    "json",
    "php",
    "scss",
    "tsx",
    "typescript",
    "query", -- for playground query editor buffer
  }, -- one of "all", "maintained" (parsers with maintainers), or a list of languages

  sync_install = true, -- install languages synchronously (only applied to `ensure_installed`)
  ignore_install = { "" }, -- List of parsers to ignore installing

  highlight = {
    enable = true, -- false will disable the whole extension
    disable = { "org" }, -- list of language that will be disabled
    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = { "org" },
  },

  incremental_selection = {
    -- TODO: Change Keymap
    enable = true,
    -- disable = { "c", "rust" },  -- list of language that will be disabled
    keymaps = {
      init_selection = "gln",
      node_incremental = "gln",
      node_decremental = "glp",
      scope_incremental = "gsc",
    },
  },

  indent = {
    enable = true,
    disable = { "" }, -- list of language that will be disabled
  },

  playground = { -- "nvim-treesitter/playground"
    enable = true,
    disable = {},
    updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
    persist_queries = false, -- Whether the query persists across vim sessions
    keybindings = {
      toggle_query_editor = "o",
      toggle_hl_groups = "i",
      toggle_injected_languages = "t",
      toggle_anonymous_nodes = "a",
      toggle_language_display = "I",
      focus_language = "f",
      unfocus_language = "F",
      update = "R",
      goto_node = "<cr>",
      show_help = "?",
    },
  },

  query_linter = { -- linter in query editor buffer in playground
    enable = true,
    use_virtual_text = true,
    lint_events = { "BufWrite", "CursorHold" },
  },

  context_commentstring = {
    enable = true,
    enable_autocmd = false,
  },

  -- autotag = {
  --   enable = true,
  -- },
  --
  -- refactor = {
  --   highlight_definitions = { enable = true },
  --   highlight_current_scope = { enable = false },
  -- },

  textobjects = {
    select = {
      enable = true,
      -- Automatically jump forward to textobj, similar to targets.vim
      lookahead = true,
      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ac"] = "@class.outer",
        ["ic"] = "@class.inner",
        ["il"] = "@loop.inner",
        ["al"] = "@loop.outer",
        ["icd"] = "@conditional.inner",
        ["acd"] = "@conditional.outer",
        ["acm"] = "@comment.outer",
        ["ast"] = "@statement.outer",
        ["isc"] = "@scopename.inner",
        ["iB"] = "@block.inner",
        ["aB"] = "@block.outer",
        ["ia"] = "@parameter.inner",
      },
    },

    -- TODO: change keymap
    swap = {
      enable = true,
      swap_next = {
        ["<leader>a"] = "@parameter.inner",
      },
      swap_previous = {
        ["<leader>A"] = "@parameter.inner",
      },
    },

    move = {
      enable = true,
      set_jumps = true, -- whether to set jumps in the jumplist
      goto_next_start = {
        ["]m"] = "@function.outer",
        ["]]"] = "@class.outer",
        -- ["gnf"] = "@function.outer",
        -- ["gnif"] = "@function.inner",
        -- ["gnp"] = "@parameter.inner",
        -- ["gnc"] = "@call.outer",
        -- ["gnic"] = "@call.inner",
      },
      goto_next_end = {
        ["]M"] = "@function.outer",
        ["]["] = "@class.outer",
        -- ["gnF"] = "@function.outer",
        -- ["gniF"] = "@function.inner",
        -- ["gnP"] = "@parameter.inner",
        -- ["gnC"] = "@call.outer",
        -- ["gniC"] = "@call.inner",
      },
      goto_previous_start = {
        ["[m"] = "@function.outer",
        ["[["] = "@class.outer",
        -- ["gpf"] = "@function.outer",
        -- ["gpif"] = "@function.inner",
        -- ["gpp"] = "@parameter.inner",
        -- ["gpc"] = "@call.outer",
        -- ["gpic"] = "@call.inner",
      },
      goto_previous_end = {
        ["[M"] = "@function.outer",
        ["[]"] = "@class.outer",
        -- ["gpF"] = "@function.outer",
        -- ["gpiF"] = "@function.inner",
        -- ["gpP"] = "@parameter.inner",
        -- ["gpC"] = "@call.outer",
        -- ["gpiC"] = "@call.inner",
      },
    },

    -- peek_definition_code: show textobject surrounding definition as determined using Neovim's built-in LSP in a floating window
    lsp_interop = {
      enable = true,
      border = "none",
      peek_definition_code = {
        ["<leader>df"] = "@function.outer",
        ["<leader>dF"] = "@class.outer",
      },
    },
  },
}
