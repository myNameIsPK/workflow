return {
  -- { "nvim-lua/popup.nvim" },
  {
    "nvim-lua/plenary.nvim",
    config = function()
      vim.keymap.set("n", "<leader>td", "<Cmd>w<Cr><Plug>PlenaryTestFile")
    end,
  }, -- always install usefull stuff
  { "tpope/vim-repeat" },
  {
    "folke/neodev.nvim",
    ft = "lua",
  },
}
