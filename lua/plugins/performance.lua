return {
  {
    "antoinemadec/FixCursorHold.nvim",
    lazy = false,
  },

  -- { "dstein64/vim-startuptime" } -- use impatient LuaCacheProfile instead

  {
    "lewis6991/impatient.nvim",
    lazy = false,
    config = function()
      require("impatient").enable_profile() -- enable LuaCacheProfile
    end,
  },
}
