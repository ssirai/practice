function divisors{T<:Integer}(n :: T)
  d = Int[]
  for i in 1:div(n,2)
    n % i == 0 && push!(d, i)
  end
  d
end

ans = 0
for i in 2:10000
  x = sum(divisors(i))
  if x != i && i == sum(divisors(x))
    ans += i
  end
end

println(ans)
