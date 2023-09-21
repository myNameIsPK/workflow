return {

  {
    "lewis6991/gitsigns.nvim",
    lazy = false,
    config = function()
      require "plugins.config.gitsigns"
    end,
  },
  -- { "sindrets/diffview.nvim" },
  {
    "ahmedkhalf/project.nvim",
    config = function()
      require("project_nvim").setup()
    end,
  },
  {
    "TimUntersberger/neogit",
    keys = { { "<leader>gg", "<Cmd>Neogit<Cr>" } },
    config = function()
      require("neogit").setup {
        integrations = {
          -- Requires you to have `sindrets/diffview.nvim` installed.
          diffview = true,
        },
      }
    end,
  },
}
