function factor{T<:Integer}(x :: T)
  lists = Int[]
  t = sqrt(x)
  i = 3
  if x % 2 == 0
    push!(lists, 2)
  end
  while i < t
    if x % i == 0 && isprime(i)
      push!(lists, i)
    end
    i += 2
  end
  lists
end

ans = factor(600851475143) |> maximum

# cheating?
# ans, _ = factor(600851475143) |> maximum

println(ans)
