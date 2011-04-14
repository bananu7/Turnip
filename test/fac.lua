function factorial(n)
  if n == 0.0 then
    return 1.0
  else
    return n * factorial(n - 1.0)
  end
end

