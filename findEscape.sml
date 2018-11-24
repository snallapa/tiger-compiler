structure FindEscape : sig val prog: Absyn.exp -> unit
                       end =
struct
  type depth = int
  type escEnv = (depth * bool ref) Symbol.table

  fun testDepth(env, symbol, currentDepth, pos) =
    let val (definedDepth, escape) = case Symbol.look(env, symbol) of
                            SOME entry => entry
                            | NONE => (ErrorMsg.error(pos, "symbol not found"); (~1, ref false))
     in if currentDepth > definedDepth then escape := true else ()
    end

  fun traverseVar(env: escEnv, d:depth, s:Absyn.var) : unit =
    let fun travVar(Absyn.SimpleVar(symbol, pos)) = testDepth(env, symbol, d, pos)
        | travVar(Absyn.FieldVar(var, symbol, pos)) = travVar(var)
        | travVar(Absyn.SubscriptVar(var, exp, pos)) = (
          traverseExp(env, d, exp);
          travVar(var))
     in travVar(s)
    end

  and traverseExp(env: escEnv, d:depth, s:Absyn.exp) : unit =
    let fun travExp(Absyn.VarExp(var)) = traverseVar(env, d, var)
        | travExp(Absyn.CallExp{func=_, args, pos=_}) = (
          (map (fn (exp) => travExp(exp)) args);
          ())
        | travExp(Absyn.OpExp{left, oper=_, right, pos=_}) = (
          travExp(left);
          travExp(right))
        | travExp(Absyn.RecordExp{fields, typ, pos}) = (
          (map (fn (_, exp, _) => travExp(exp)) fields);
          ())
        | travExp(Absyn.SeqExp(explist)) = (app (fn (exp, pos) => travExp(exp)) explist)
        | travExp(Absyn.AssignExp{var=_, exp, pos=_}) = travExp(exp)
        | travExp(Absyn.IfExp{test, then', else', pos=_}) = (
          travExp(test);
          travExp(then');
          (case else' of 
            SOME exp => travExp(exp) 
            | NONE => ()))
        | travExp(Absyn.WhileExp{test, body, pos=_}) = (
          travExp(test);
          travExp(body))
        | travExp(Absyn.ForExp{var, escape, lo, hi, body, pos=_}) = (
          travExp(lo);
          travExp(hi);
          traverseExp(Symbol.enter(env, var, (d, escape)), d, body))
        | travExp(Absyn.LetExp{decs, body, pos=_}) = traverseExp(traverseDecs(env, d, decs), d, body)
        | travExp(Absyn.ArrayExp{typ, size, init, pos}) = (
          travExp(size);
          travExp(init))
        | travExp(_) = ()
     in travExp(s)
    end

  and traverseDecs(env, d, s:Absyn.dec list) : escEnv =
    let fun travDec(env, Absyn.TypeDec(_)) = env
        | travDec(env, Absyn.FunctionDec(fundecs)) =
          (foldl (fn ({name, params, result, body, pos}, env) =>
                     (traverseExp(foldl (fn ({name, escape, typ, pos}, e) => Symbol.enter(e, name, (d + 1, escape)))
                                        env
                                        params,
                                   d + 1,
                                   body);
                      env))
                 env
                 fundecs)
        | travDec(env, Absyn.VarDec{name, escape, typ, init, pos}) =
          Symbol.enter(env, name, (d, escape))
     in (foldl (fn (dec, env) => travDec(env, dec)) env s)
    end

  fun prog(prog: Absyn.exp) : unit = traverseExp(Symbol.empty, 0, prog)
end
