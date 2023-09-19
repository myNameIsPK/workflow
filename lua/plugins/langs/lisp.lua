return {
  {
    "gpanders/nvim-parinfer",
    ft = { "clojure", "scheme", "lisp", "racket", "hy", "fennel", "janet", "carp", "wast", "yuck" },
    lazy = false,
    config = function()
      vim.keymap.set("n", "<leader>tl", "<cmd>ParinferToggle!<cr>")
    end,
  },
}
