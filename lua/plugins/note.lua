return {
  {
    "mickael-menu/zk-nvim",
    keys = "<leader>n",
    ft = "md",
    dev = true,
    config = function()
      require "plugins.config.zk"
    end,
  },

  -- { "nvim-neorg/neorg", dependencies = "nvim-neorg/neorg-telescope" },
}
