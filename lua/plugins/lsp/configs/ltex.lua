local path = vim.fn.stdpath("config") .. "/spell/en.utf-8.add"
local words = {}

for word in io.open(path, "r"):lines() do
	table.insert(words, word)
end

return {
  autostart = false,
  settings = {
    ltex = {
      disabledRules = { ["en-US"] = { "PROFANITY" } },
      dictionary = {
        ["en-US"] = words,
      },
    },
  },
}
