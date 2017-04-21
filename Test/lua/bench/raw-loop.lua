v = 100000

t = {}
t[0] = 1
t[1] = 1

for i=2,v do
	t[i] = t[i-1] + t[i-2]
end

return t[v]