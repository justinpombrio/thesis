macro timeit(ex)
  return quote
    local t0 = time()
    println("Time: ", t0, " x: ", x)
    local val = $(esc(ex))
    local t1 = time()
    println("elapsed time: ", t1 - t0, " seconds")
    println("A")
    val
    println("B")
  end
end

main() = begin
  x = 17
#  time() = "Yeah, it's time." -- breaks everything!
  println("Beg")
  @timeit time()
  println("End")
end

main()
