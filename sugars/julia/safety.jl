# Julia is not hygienic
macro unhygienic(i, body)
  quote
    x = "macro"
    $(i) = "user"
    println("macro->", x)
    x = "macro"
    $(body)
  end
end
@unhygienic(x, println("user->", x)) # user/macro get swapped!

# Not hygienic in another way too
macro unbound_x()
  return :(println(x))
end

call_unbound_x() = begin
  x = 5
  @unbound_x()
end

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


# Julia macros are not syntax safe.
macro syntax_unsafe(x)
  quote
    $(x) = 0
    "ok"
  end
end
# println(@syntax_unsafe(1 + 2)) # syntax error only on use
