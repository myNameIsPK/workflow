local M = {}
-- FIX: not working properly

function M.unload_all_module()
  for pack, _ in pairs(package.loaded) do
    if pack:match("config") then
      print(pack)
      package.loaded[pack] = nil
    end
  end
end

function M.reload(module_name)
  -- print("loop " .. module_name)
  for pack, _ in pairs(package.loaded) do
    if pack:match("^" .. module_name) then
      print(pack)
      package.loaded[pack] = nil
    end
  end
end

function M.reload_all()
  for _, pack in ipairs(vim.fn.glob("./lua/**", false, true)) do
    pack = string.gsub(pack, "..lua/", "")
    M.reload(pack)
  end
  M.unload_all_module()
  vim.cmd([[luafile init.lua]])
  print("reload!!")
end

return M
