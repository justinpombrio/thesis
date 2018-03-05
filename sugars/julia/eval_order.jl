macro outer(x)
    print("Begin outer call\n")
    answer = :($x * 2)
    print("End outer call\n")
    return answer
end

macro inner(x)
    print("Begin inner call\n")
    answer = :($x + 1)
    print("End inner call\n")
    return answer
end

print("(10 + 1) * 2 = ", @outer(@inner(10)), "\n")

#=
Output:
  Begin outer call
  End outer call
  Begin inner call
  End inner call
  (10 + 1) * 2 = 22
Evaluation order: OI*
Syntax safe: yes
=#