local M = {}

function M.setup()
  local status_ok, neorg = pcall(require, "neorg")
  if not status_ok then
    return
  end

  local parser_configs = require("nvim-treesitter.parsers").get_parser_configs()

  -- These two are optional and provide syntax highlighting
  -- for Neorg tables and the @document.meta tag
  parser_configs.norg_meta = {
    install_info = {
      url = "https://github.com/nvim-neorg/tree-sitter-norg-meta",
      files = { "src/parser.c" },
      branch = "main",
    },
  }

  parser_configs.norg_table = {
    install_info = {
      url = "https://github.com/nvim-neorg/tree-sitter-norg-table",
      files = { "src/parser.c" },
      branch = "main",
    },
  }

  -- require("nvim-treesitter.configs").setup {
  -- -- FIX: treesitter load again when open filetype
  --   ensure_installed = { "norg", "norg_meta", "norg_table", "markdown" },
  --   highlight = { -- Be sure to enable highlights if you haven't!
  --     enable = true,
  --   },
  -- }

  -- TODO: add https://github.com/nvim-neorg/neorg#core-modules
  neorg.setup {
    load = {
      ["core.defaults"] = {},
      ["core.norg.dirman"] = {
        config = {
          workspaces = {
            gtd = "~/notes/gtd",
          },
        },
      },
      ["core.gtd.base"] = {
        config = {
          workspace = "gtd",
        },
      },
      ["core.norg.completion"] = {
        config = {
          engine = "nvim-cmp",
        },
      },
      ["core.norg.concealer"] = {
        config = {
          icon_preset = "diamond",
          -- markup_preset = "dimmed",
        },
      },
      -- ["core.norg.esupports"] = {
      --   config = {
      --     indent = false,
      --     indent_config = {
      --       current = {
      --         enabled = false,
      --       },
      --       previous = {
      --         enabled = false,
      --       },
      --       realtime = {
      --         enabled = false,
      --       },
      --     },
      --   },
      -- },
      ["core.integrations.telescope"] = {},
      ["core.keybinds"] = {
        config = {
          keybind_presets = { -- this presets is from default config
            default_config = false,
            neorg_leader = "<LocalLeader>",
            neorg = function(keybinds)
              local leader = keybinds.leader

              -- Map all the below keybinds only when the "norg" mode is active
              keybinds.map_event_to_mode("norg", {
                n = { -- Bind keys in normal mode

                  -- Keys for managing TODO items and setting their states
                  { "gtu", "core.norg.qol.todo_items.todo.task_undone" },
                  { "gtp", "core.norg.qol.todo_items.todo.task_pending" },
                  { "gtd", "core.norg.qol.todo_items.todo.task_done" },
                  { "gth", "core.norg.qol.todo_items.todo.task_on_hold" },
                  { "gtc", "core.norg.qol.todo_items.todo.task_cancelled" },
                  { "gtr", "core.norg.qol.todo_items.todo.task_recurring" },
                  { "gti", "core.norg.qol.todo_items.todo.task_important" },
                  { "<C-Space>", "core.norg.qol.todo_items.todo.task_cycle" },

                  -- Keys for managing GTD
                  { leader .. "tc", "core.gtd.base.capture" },
                  { leader .. "tv", "core.gtd.base.views" },
                  { leader .. "te", "core.gtd.base.edit" },

                  -- Keys for managing notes
                  { leader .. "nn", "core.norg.dirman.new.note" },

                  { "<CR>", "core.norg.esupports.hop.hop-link" },
                  { "<M-CR>", "core.norg.esupports.hop.hop-link", "vsplit" },

                  { "<M-k>", "core.norg.manoeuvre.item_up" },
                  { "<M-j>", "core.norg.manoeuvre.item_down" },

                  -- mnemonic: markup toggle
                  { leader .. "mt", "core.norg.concealer.toggle-markup" },

                  { "<C-s>", "core.integrations.telescope.find_linkable" },
                },

                o = {
                  { "ah", "core.norg.manoeuvre.textobject.around-heading" },
                  { "ih", "core.norg.manoeuvre.textobject.inner-heading" },
                  { "at", "core.norg.manoeuvre.textobject.around-tag" },
                  { "it", "core.norg.manoeuvre.textobject.inner-tag" },
                  { "al", "core.norg.manoeuvre.textobject.around-whole-list" },
                },
                i = {
                  { "<C-l>", "core.integrations.telescope.insert_link" },
                },
              }, {
                silent = true,
                noremap = true,
              })

              -- Map the below keys only when traverse-heading mode is active
              keybinds.map_event_to_mode("traverse-heading", {
                n = {
                  -- Rebind j and k to move between headings in traverse-heading mode
                  { "j", "core.integrations.treesitter.next.heading" },
                  { "k", "core.integrations.treesitter.previous.heading" },
                },
              }, {
                silent = true,
                noremap = true,
              })

              keybinds.map_event_to_mode("toc-split", {
                n = {
                  { "<CR>", "core.norg.qol.toc.hop-toc-link" },

                  -- Keys for closing the current display
                  { "q", "core.norg.qol.toc.close" },
                  { "<Esc>", "core.norg.qol.toc.close" },
                },
              }, {
                silent = true,
                noremap = true,
                nowait = true,
              })

              -- Map the below keys on gtd displays
              keybinds.map_event_to_mode("gtd-displays", {
                n = {
                  { "<CR>", "core.gtd.ui.goto_task" },

                  -- Keys for closing the current display
                  { "q", "core.gtd.ui.close" },
                  { "<Esc>", "core.gtd.ui.close" },

                  { "e", "core.gtd.ui.edit_task" },
                  { "<Tab>", "core.gtd.ui.details" },
                },
              }, {
                silent = true,
                noremap = true,
                nowait = true,
              })

              -- Map the below keys on presenter mode
              keybinds.map_event_to_mode("presenter", {
                n = {
                  { "<CR>", "core.presenter.next_page" },
                  { "l", "core.presenter.next_page" },
                  { "h", "core.presenter.previous_page" },

                  -- Keys for closing the current display
                  { "q", "core.presenter.close" },
                  { "<Esc>", "core.presenter.close" },
                },
              }, {
                silent = true,
                noremap = true,
                nowait = true,
              })
              -- Apply the below keys to all modes
              keybinds.map_to_mode("all", {
                n = {
                  { leader .. "mn", ":Neorg mode norg<CR>" },
                  { leader .. "mh", ":Neorg mode traverse-heading<CR>" },
                },
              }, {
                silent = true,
                noremap = true,
              })
            end,
          },
        },
      },
      ["core.norg.manoeuvre"] = {},
      ["core.norg.qol.toc"] = {},
    },
  }
end

return M
