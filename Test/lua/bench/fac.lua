function fac(i)
	if i == 1 then return 1 end
	return i * fac(i-1)
end

fac(1000000)

return true