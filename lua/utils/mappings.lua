local M = {}

local function map_wrapper(mode, lhs, rhs, options)
    if type(lhs) == "table" then
      for _, key in ipairs(lhs) do
         map_wrapper(mode, key, rhs, options)
      end
    else
      vim.keymap.set(mode, lhs, rhs, options)
    end
 end

M.map = function(mode, keys, cmd, opt)
   local options = { noremap = true, silent = false }
   if opt then
      options = vim.tbl_extend("force", options, opt)
   end
   map_wrapper(mode, keys, cmd, options)
end

return M
