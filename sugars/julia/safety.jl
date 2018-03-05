# Julia is not hygienic
  macro unbound_x()
    return :(println(x))
  end
  
  call_unbound_x() = begin
    x = 5
    @unbound_x()
  end
  
  # No error! Not hygienic
  call_unbound_x()


# Julia is not scope safe
  macro unbound_y()
    return :(println(y))
  end # No error! Not scope safe.


# Julia is untyped ("dynamically typed"), so its macros certainly aren't type safe.
  f() = 1 + "two"
  # f() -- runtime error
  a = :3
  b = :"four"
  c = :($a + $b) # No error! Not type safe.


# Julia macros are syntax-safe, though.
  macro inner(x) :($x + 1) end
  macro outer(x) :($x * 2) end
  println("(10 + 1) * 2 = ", @outer(@inner(10)))
