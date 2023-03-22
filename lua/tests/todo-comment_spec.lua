local eq = assert.are.same
local todo_comment = require "todo-comment"

local test_keywords = {
  TODO = {},
  FIX = { alt = { "FIXME" } },
}

describe("pattern generation", function()
  local gen_keywords = todo_comment.get_keyword_list
  it("should get all keyword from table", function()
    local test_list = { "TODO", "FIX", "FIXME" }
    local keyword_list = gen_keywords(test_keywords)
    table.sort(test_list)
    table.sort(keyword_list)
    eq(test_list, keyword_list)
  end)

  it("should get correct pattern", function()
    local keyword_list = gen_keywords(test_keywords)
    table.sort(keyword_list)
    local pattern = todo_comment.gen_todo_pattern(keyword_list)
    eq('"(FIX|FIXME|TODO)\\(?.*\\)?:"', pattern)
  end)
end)

-- describe("pattern matching", function()
--   local test_text = [[
-- TODO: not comment
-- "TODO: in string"
-- -- TODO: in comment
-- -- TODO(name): have name
-- # FIX: fix
-- // FIXME: alternate name
-- ]]
--
--   it("should not match non comment", function() end)
--
--   it("should match PATTERN", function() end)
--
--   it("should match alternate name", function() end)
-- end)
