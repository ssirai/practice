N = 1000
i = 0
n = 1
s = 1
while i < N
  n = n + 2
  a = n^2
  b = n^2 - n + 1
  c = n^2 - 2n + 2
  d = n^2 - 3n + 3
  s += a + b + c + d
  i += 2
end

println(s)
