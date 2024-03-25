if vim.env.TERMUX_VERSION then
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
        sources = {
          -- formatting.prettier.with { extra_args = { "--no-semi", "--single-quote", "--jsx-single-quote" } },
          -- formatting.black.with { extra_args = { "--fast" } },
          formatting.stylua,
          formatting.jq,
          -- diagnostics.flake8
          -- formatting.latexindent,
          diagnostics.shellcheck,
          -- diagnostics.yamllint,
          -- diagnostics.ansiblelint,
          -- code_actions.shellcheck,
          -- code_actions.gitsigns, -- gitsigns plugins integration
          flags = { debounce_text_changes = 150 }, -- this make lsp not reload immediately everytime while you typing the words
        },
      }
    end,
  },

  -- {
  --   "danymat/neogen",
  --   dependencies = "nvim-treesitter/nvim-treesitter",
  -- },

  -- TODO: neotest or vimtest

  -- { "mfussenegger/nvim-dap" },
}
