return {

  { "lewis6991/gitsigns.nvim" },
  { "TimUntersberger/neogit" },
  -- { "sindrets/diffview.nvim" },
  {
    "ahmedkhalf/project.nvim",
    config = function()
      require("project_nvim").setup()
    end,
  },
}
