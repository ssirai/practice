function collatz{T<:Integer}(n :: T)
  step = 0
  while n != 1
    if iseven(n)
      n = div(n, 2)
    else
      n = 3n + 1
    end
    step += 1
  end
  step
end

N = 1000000
[collatz(i) for i in 1:N] |> indmax |> println
