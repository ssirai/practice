N = 1000
a, b = BigInt(1), BigInt(1)
i = 2
while true
  i += 1
  a, b = (b, a+b)
  if length(string(b)) >= N
    println(i)
    break
  end
end
