f(xs) = map(x -> int(x) - '0', xs)
BigInt(2)^1000 |> string |> collect |> f |> sum |> println
