ls = 1:100
sum(ls)^2 - sum(map(x -> x^2, ls)) |> println
