path = joinpath(@__FILE__() |> dirname, "names.txt")
open(path, "r") do f
  x = split(replace(readall(f), "\"", ""), ",") |> sort
  [sum(map(x -> int(x) - 'A' + 1, collect(v))) * i for (i, v) in enumerate(x)] |> sum |> println
end
