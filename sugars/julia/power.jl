
# Macros can define macros
macro macro_defining_macro()
  quote
    macro macro_defined_macro()
      :("macro-defined macro")
    end
  end
end

@macro_defining_macro()

println(@macro_defined_macro())

# Macro arguments can only be "expressions, literal values, and symbols"
# Thus this macro does not work:
macro define_plus_one(i, defn)
  quote
    $(i) = $(defn) + 1
  end
end

#@define_plus_one(:(x), 2)
#println("2 + 1 = ", x) # syntax error, because patterns cannot be arguments to macros

# Except that sometimes the argument *can* be used as a pattern?
macro pattern(i)
  :($(i))
end
@pattern((x, y)) = ("pattern", "argument")
println(x, " ", y)

# Julia macros can deconstruct their arguments
macro deconstruct(x)
  println("Deconstructed: ", x.head, " ", x.args)
end
@deconstruct((1 + 2 + 3))