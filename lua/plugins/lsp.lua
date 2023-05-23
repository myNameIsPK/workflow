return {
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      { "williamboman/mason.nvim" },
      { "williamboman/mason-lspconfig.nvim" },
    },
    config = function()
      require "my.lsp"
    end,
  },
  {
    "williamboman/mason.nvim",
    -- lazy = false,
    cmd = "Mason",
    config = function()
      -- `:h mason-setting`
      require("mason").setup()
    end,
  },
  {
    "williamboman/mason-lspconfig.nvim",
    keys = { { "<leader>sl", "<cmd>LspInfo<cr>" } },
  },
}
