N = 1000
xs = 1:999
ys = 1:999

for a = xs
  for b = ys
    c = N - a - b
    a^2 + b^2 == c^2 && (println(a*b*c); return)
  end
end

