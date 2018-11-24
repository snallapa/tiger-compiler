structure RegKey =
struct
    type ord_key = string
    val compare = fn(s1, s2) => case String.compare(s1, s2) of GREATER => LESS | LESS => GREATER | EQUAL => EQUAL
end

structure RegSet = ListSetFn(RegKey)