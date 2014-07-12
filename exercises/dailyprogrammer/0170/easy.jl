using Base.Test

function solve(lines::Array{Union(ASCIIString, UTF8String), 1})
  # Not implemented
  "Alice has won!"
end

# Test
begin
  test_files = ["easy_input_01", "easy_input_02"]
  expected_outputs   = ["Alice has won!", "David has won with a 5-card trick!"]

  for (name, output) = zip(test_files, expected_outputs)
    path = joinpath("0170", "$(name).txt")
    open(path, "r") do f
      lines = readlines(f)
      # n = lines[1] |> rstrip |> int
      xs = lines |> (x -> map(rstrip, x))
      answer = solve(lines[2:end])
      @test answer == output
    end
  end
end 
