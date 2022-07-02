local status_ok, Hydra = pcall(require, "hydra")
if not status_ok then
  return
end

Hydra {
  name = "Wincmd",
  mode = "n",
  body = "<C-w>",
  heads = {
    { "h", "<C-w>h" },
    { "j", "<C-w>j" },
    { "k", "<C-w>k" },
    { "l", "<C-w>l" },

    { "H", "<C-w>H" },
    { "J", "<C-w>J" },
    { "K", "<C-w>K" },
    { "L", "<C-w>L" },

    { "s", "<C-w>s" },
    { "v", "<C-w>v" },

    { "c", "<C-w>c" },

    { "<", "<C-w><" },
    { ">", "<C-w>>" },

    { "+", "<C-w>+" },
    { "-", "<C-w>-" },
  },
}
