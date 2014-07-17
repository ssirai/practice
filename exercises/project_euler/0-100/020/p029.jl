N = 100
as = 2:N
bs = 2:N

xs = BigInt[]

for a in as
  for b in bs
    push!(xs, BigInt(a)^b)
  end
end

x = unique(xs)
print(length(x))
