d1 = "one two three four five six seven eight nine" |> split
tens = "ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen" |> split
d2 = "twenty thirty forty fifty sixty seventy eighty ninety" |> split
d2 = [d1, tens, [[y*x for x in ["", d1]] for y in d2]...]
d3 = [d2, [[y*"hundred"*x for x in ["", map(z->"and"*z, d2)]] for y in d1]...]
d4 = [d3, "onethousand"]
join(d4) |> length |> println
