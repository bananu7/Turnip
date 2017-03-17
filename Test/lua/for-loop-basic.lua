local t = function(x)
  local f = function(s,v)
     if v > 0 then
      return v - 1
    else
      return nil
    end
  end
  local s = nil
  local v = x
  return f, s, v
end
local cnt = 0
for i in t(4) do
  cnt = cnt + i
end
return (cnt == 6)