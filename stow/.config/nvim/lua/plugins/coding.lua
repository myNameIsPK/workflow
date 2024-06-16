if my.is_termux then
  return {}
end
return {

  {
    "nvimtools/none-ls.nvim", -- null-ls
    init = function()
      local null_ls = require "null-ls"

      local formatting = null_ls.builtins.formatting
      local diagnostics = null_ls.builtins.diagnostics
      local code_actions = null_ls.builtins.code_actions

      null_ls.setup {
        -- see: sources find in none-ls project `fd ".*\.lua" lua/null-ls/builtins/*/`
        -- see: deprecated sources <https://github.com/nvimtools/none-ls.nvim/issues/58#issue-2077335231>
        sources = {
          -- formatting.prettier.with { extra_args = { "--no-semi", "--single-quote", "--jsx-single-quote" } },
          -- formatting.black.with { extra_args = { "--fast" } },
          formatting.stylua,
          -- diagnostics.yamllint,
          -- diagnostics.ansiblelint,
          -- code_actions.gitsigns, -- gitsigns plugins integration
          flags = { debounce_text_changes = 150 }, -- this make lsp not reload immediately everytime while you typing the words
        },
      }
    end,
  },

  {
    "danymat/neogen",
    init = function()
      require("neogen").setup {}
    end,
  },

  {
    "nvim-neotest/neotest",
    dependencies = {
      "nvim-neotest/nvim-nio",
      "nvim-lua/plenary.nvim",
      "antoinemadec/FixCursorHold.nvim",
      "nvim-treesitter/nvim-treesitter",
      "nvim-neotest/neotest-plenary",
      -- "nvim-neotest/neotest-vim-test",
    },
    init = function()
      require("neotest").setup {
        adapters = {
          require "neotest-plenary",
          -- require("neotest-python")({
          --   dap = { justMyCode = false },
          -- }),
          -- require("neotest-vim-test")({
          --   ignore_file_types = { "python", "vim", "lua" },
          -- }),
        },
      }
    end,
  },

  {
    "mfussenegger/nvim-dap",
    dependencies = {
      { "theHamsta/nvim-dap-virtual-text", config = true },
    },
    init = function()
      local dap = require "dap"
      dap.adapters.gdb = {
        type = "executable",
        command = "gdb",
        args = { "-i", "dap" },
      }
      dap.configurations.c = {
        {
          name = "Launch",
          type = "gdb",
          request = "launch",
          program = function()
            return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
          end,
          cwd = "${workspaceFolder}",
          stopAtBeginningOfMainSubprogram = false,
        },
      }
      -- stylua: ignore start
      vim.keymap.set('n', '<F5>', function() require('dap').continue() end)
      vim.keymap.set('n', '<F10>', function() require('dap').step_over() end)
      vim.keymap.set('n', '<F11>', function() require('dap').step_into() end)
      vim.keymap.set('n', '<F12>', function() require('dap').step_out() end)
      vim.keymap.set('n', '<localleader>b', function() require('dap').toggle_breakpoint() end)
      vim.keymap.set('n', '<localleader>B', function() require('dap').set_breakpoint() end)
      vim.keymap.set('n', '<localleader>lp', function() require('dap').set_breakpoint(nil, nil, vim.fn.input('Log point message: ')) end)
      vim.keymap.set('n', '<localleader>dr', function() require('dap').repl.open() end)
      vim.keymap.set('n', '<localleader>dl', function() require('dap').run_last() end)
      vim.keymap.set({'n', 'v'}, '<localleader>dh', function()
        require('dap.ui.widgets').hover()
      end)
      vim.keymap.set({'n', 'v'}, '<localleader>dp', function()
        require('dap.ui.widgets').preview()
      end)
      vim.keymap.set('n', '<localleader>df', function()
        local widgets = require('dap.ui.widgets')
        widgets.centered_float(widgets.frames)
      end)
      vim.keymap.set('n', '<localleader>ds', function()
        local widgets = require('dap.ui.widgets')
        widgets.centered_float(widgets.scopes)
      end)
      -- stylua: ignore end
    end,
  },
}
