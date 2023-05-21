return {
  {
    "nvim-treesitter/nvim-treesitter",
    dependencies = {
      { "nvim-treesitter/playground" },
      { "nvim-treesitter/nvim-treesitter-textobjects" },
      { "JoosepAlviste/nvim-ts-context-commentstring" },
    },
    event = { "BufReadPost", "BufNewFile" },
    config = function()
      require "plugins.config.treesitter"
      vim.keymap.set("n", "<leader>st", "<Cmd>TSModuleInfo<Cr>")
      vim.keymap.set("n", "<leader>tp", "<Cmd>TSPlaygroundToggl<Cr>")
    end,
  },
}
