N = 20
x = ones(Int, N+1, N+1)

for i in 2:N+1
  for j in 2:N+1
    x[i, j] = x[i-1, j] + x[i, j-1]
  end
end

println(x[N+1, N+1])
