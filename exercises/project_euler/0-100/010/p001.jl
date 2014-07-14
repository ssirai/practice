ans = filter(x -> x % 3 == 0 || x % 5 == 0, 1:999) |> sum
println(ans)
