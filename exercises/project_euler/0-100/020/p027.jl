function main()
  m = 0 :: Int
  ma, mb = 0 :: Int , 0 :: Int
  p = primes(1000)
  for a in -999:999
    for b in p
      n, i = 0, 0
      while isprime(n^2 + n*a + b)
        n += 1
        i += 1
      end
      if m < i
        m = i
        ma, mb = a, b
      end
    end
  end

  println(ma*mb)
end


println(@elapsed main())
