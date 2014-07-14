N = 20
function f(x)
  ans = 1
  while true
    t = ans * x
    t > N && return ans
    ans = t
  end
end

primes(N) |> x -> map(f, x) |> prod |> println
