using Dates

ans = 0
for y in 1901:2000
  for m in 1:12
    if Sunday == dayofweek(Date(y, m, 1))
      ans += 1
    end
  end
end

println(ans)
