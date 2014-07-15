function divisors{T<:Integer}(n :: T)
  d = Int[]
  for i in 1:div(n,2)
    n % i == 0 && push!(d, i)
  end
  d
end

N = 28123
d = Dict()
for i in 2:N
  x = divisors(i)
  s = sum(x)
  if s > i
    d[i] = true
  end
end

ans = sum(1:11)
for i in 12:N
  b = true
  for (k, _) in d
    t = i - k
    if t <= 0
      continue
    elseif haskey(d, t)
      b = false
      break
    end
  end
  if b
    ans += i
  end
end

println(ans)

