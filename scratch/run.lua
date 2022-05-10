local package_list = vim.tbl_keys(package.loaded)
for _, package in ipairs(package_list) do
  print(package)
end
