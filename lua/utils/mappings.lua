local M = {}

-- TODO: use vim.keymap.set
-- M.map = vim.keymap.set
M.map = function(mode, keys, cmd, opt)
   local options = { noremap = true, silent = true }
   if opt then
      options = vim.tbl_extend("force", options, opt)
   end

   -- helper function for M.map
   -- can gives multiple modes and keys
   local function map_wrapper(mode, lhs, rhs, options)
      if type(lhs) == "table" then
        for _, key in ipairs(lhs) do
           map_wrapper(mode, key, rhs, options)
        end
      else
        vim.keymap.set(mode, lhs, rhs, options)
      end
   end

   map_wrapper(mode, keys, cmd, options)
end

return M
