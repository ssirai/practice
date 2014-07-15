f(xs) = map(x -> int(x) - '0', xs)
factorial(BigInt(100)) |> string |> collect |> f |> sum |> println
