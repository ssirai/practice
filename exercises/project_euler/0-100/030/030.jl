N = 1000000
f(x) = x^5
ans = 0
for x in 2:N
  s = map(x -> int(x) - '0', collect(string(x)))
  k = sum(map(f, s))
  if x == k
    ans += x
  end
end

println(ans)
