function iter (a, i)
  i = i + 1
  local v = a[i]
  if v then
    return i, v
  end
end

function ipairs (a)
  return iter, a, 0
end

local cnt = 0
local arr = {3,5}
for k,v in ipairs(arr) do
  cnt = cnt + k + v
end
return (cnt == 11)