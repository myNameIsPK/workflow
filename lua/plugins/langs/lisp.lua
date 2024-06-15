return {
  {
    "gpanders/nvim-parinfer",
    init = function()
      vim.g.parinfer_filetypes = {
        "clojure",
        "scheme",
        "query", -- from :EditQuery
        "lisp",
        "racket",
        "hy",
        "fennel",
        "janet",
        "carp",
        "wast",
        "yuck",
      }
    end,
    lazy = false,
    config = function()
      vim.keymap.set("n", "<leader>tl", "<cmd>ParinferToggle!<cr>")
    end,
  },
}
