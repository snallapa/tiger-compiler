signature ENV =
sig
    type access
    type ty

    datatype enventry = VarEntry of {access: Translate.access, ty: ty}
                      | FunEntry of {level: Translate.level,
                                     label: Temp.label,
                                     formals: ty list, result: ty}

    val base_venv : enventry Symbol.table
    val base_tenv : ty Symbol.table
end

structure Env : ENV =
struct
    type ty = Types.ty
    type access = unit

    datatype enventry = VarEntry of {access: Translate.access, ty: ty}
                      | FunEntry of {level: Translate.level,
                                     label: Temp.label,
                                     formals: ty list, result: ty}

    val base_functions = [
        (Symbol.symbol "print", Types.STRING :: nil, Types.UNIT),
        (Symbol.symbol "printi", Types.INT :: nil, Types.UNIT),
        (Symbol.symbol "flush", nil, Types.UNIT),
        (Symbol.symbol "getchar", nil, Types.STRING),
        (Symbol.symbol "ord", Types.STRING :: nil, Types.INT),
        (Symbol.symbol "chr", Types.INT :: nil, Types.STRING),
        (Symbol.symbol "size", Types.STRING :: nil, Types.INT),
        (Symbol.symbol "substring", Types.STRING :: Types.INT :: Types.INT :: nil, Types.STRING),
        (Symbol.symbol "concat", Types.STRING :: Types.STRING :: nil, Types.STRING),
        (Symbol.symbol "not", Types.INT :: nil, Types.INT),
        (Symbol.symbol "exit", Types.INT :: nil, Types.UNIT)]
    fun appendFunction((name, formals, result), table) =
      Symbol.enter(table,
                   name,
                   FunEntry{level=Translate.outermost,
                            label=name,
                            formals=formals,
                            result=result})
    val base_types = [
        (Symbol.symbol "int", Types.INT),
        (Symbol.symbol "string", Types.STRING)]
    fun appendTable((name, value), table) = Symbol.enter(table, name, value)

    val base_venv = (foldl appendFunction Symbol.empty base_functions)
    val base_tenv = (foldl appendTable Symbol.empty base_types)
end
