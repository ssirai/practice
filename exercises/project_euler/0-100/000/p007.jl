p = 3
i = 2
N = 10001
while true
  if isprime(p)
    i += 1
    if i > N
      println(p)
      break
    end
  end
  p += 2
end

