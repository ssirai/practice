N = 1000000
i = 0
for v in permutations(0:9)
  i += 1
  if i == N
    join(v) |> println; break
  end
end
