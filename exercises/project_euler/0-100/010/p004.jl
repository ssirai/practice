ispalindrome(x) = string(x) == reverse(string(x))

fst(x) = x[1]
snd(x) = x[2]

ans = [[(ispalindrome(i * j), i * j) for i in 100:999] for j in 100:999] |> x -> vcat(x...) |> x -> filter(fst, x) |> maximum |> snd

println(ans)
