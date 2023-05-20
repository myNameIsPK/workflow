return {
  {
    "iamcco/markdown-preview.nvim",
    cmd = "MarkdownPreviewToggle",
    config = function()
      vim.keymap.set("n", "<leader>tm", "<Cmd>MarkdownPreviewToggle<Cr>", { desc = "Markdown Preview" })
    end,
    build = function()
      vim.fn["mkdp#util#install"]()
    end,
  },

  {
    "nvim-telescope/telescope-fzf-native.nvim",
    dependencies = "nvim-telescope/telescope.nvim",
    build = "make",
  },
}
