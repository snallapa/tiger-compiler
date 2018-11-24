signature SEMANT =
sig
    type venv = Env.enventry Symbol.table
    type tenv = Env.ty Symbol.table
    type expty = {exp: Translate.exp, ty: Types.ty}

    val transProg: Absyn.exp -> Translate.frag list

    val transVar: Translate.level * venv * tenv * Absyn.var -> expty
    val transExp: Translate.level * venv * tenv * Absyn.exp * bool -> expty
    val transDec: Translate.level * venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv, exp: Translate.exp list}
    val transTy: tenv * Absyn.ty -> Types.ty
end

structure SemAnt =
struct
    (* label used for breaks in invalid context *)
    val dummyLabel = Temp.newlabel()

    fun transProg(exp) =
      let val {exp=tree, ty=ty} = transExp(Translate.outermost, Env.base_venv, Env.base_tenv, exp, false, dummyLabel)
       in Translate.procEntryExit{level=Translate.outermost, body=tree};
          Translate.getResult()
      end

    and typesMatch(expected_ty, actual_ty) =
    (case expected_ty of Types.RECORD(list, expected_unique) =>
                             (case actual_ty of Types.RECORD(list, actual_unique) => expected_unique = actual_unique
                                                | Types.NIL => true
                                                | _         => false)
                         | Types.ARRAY(_, expected_unique) =>
                             (case actual_ty of Types.ARRAY(list, actual_unique) => (expected_unique = actual_unique)
                                                | Types.INT=> false
                                                | _         => false)
                         | Types.NIL =>
                             (case actual_ty of Types.RECORD(_,_) => true
                                                | _         => false)
                         | _ => expected_ty = actual_ty)

    and checkType(tenv, actual_ty, expected_ty, pos) =
        let val areTypesEqual = typesMatch(actualType(tenv, expected_ty), actualType(tenv, actual_ty))
         in if areTypesEqual then () else (ErrorMsg.error(pos, "expected equal types"))
        end

    and actualType(tenv, Types.NAME(symbol, ref (SOME ty))) = actualType(tenv, ty)
        | actualType(tenv, Types.NAME(symbol, ref NONE)) = (ErrorMsg.error(0, "type not found"); Types.UNIT)
        | actualType(tenv, ty) = ty

    and transTy(tenv, Absyn.NameTy(symbol, pos)) =
            (case Symbol.look(tenv, symbol) of SOME ty => Types.NAME(symbol, ref (SOME ty))
                                              | NONE => Types.NAME(symbol, ref NONE))
        | transTy(tenv, Absyn.RecordTy(fields)) =
            Types.RECORD(map
              (fn{name, escape, typ, pos} =>
                (name, case Symbol.look(tenv, typ) of SOME ty => ty
                                                      | NONE => (ErrorMsg.error(pos, "type not found"); Types.UNIT)))
              fields, ref ())
        | transTy(tenv, Absyn.ArrayTy(symbol, pos)) =
            (case Symbol.look(tenv, symbol) of SOME ty => Types.ARRAY(ty, ref ())
                                              | NONE => (ErrorMsg.error(pos, "type not found"); Types.INT))

    and transVar(level, venv, tenv, var) =
        let fun trvar(Absyn.SimpleVar(symbol, pos)) = (case Symbol.look(venv, symbol) of
                SOME(Env.VarEntry{access, ty}) => {exp=Translate.simpleVar(access, level), ty=actualType(tenv, ty)}
                | SOME _ => (ErrorMsg.error(pos, "not a first order language"); {exp=Translate.error(), ty=Types.UNIT})
                | NONE => (ErrorMsg.error(pos, ("undefined variable {" ^ (Symbol.name symbol) ^ "}")); {exp=Translate.error(), ty=Types.UNIT}))
            | trvar(Absyn.FieldVar(var, symbol, pos)) =
                let val {exp=var_tree, ty=ty} = trvar(var)
                    fun findWithIndex (nil, f, index) = NONE
                        | findWithIndex(first :: rest, f, index) = if f(first)
                                                                   then SOME((first, index))
                                                                   else findWithIndex(rest, f, index + 1)
                 in case ty of
                    Types.RECORD((params, _)) =>
                      (case (findWithIndex(params, (fn (name, ty) => name = symbol), 0)) of SOME ((name, ty), index) => {exp=Translate.fieldVar(var_tree, index), ty=actualType(tenv, ty)}
                                                                                           | NONE => (ErrorMsg.error(pos, "unknown record field {" ^ (Symbol.name symbol) ^ "}"); {exp=Translate.error(), ty=Types.UNIT}))
                    | _ => (ErrorMsg.error(pos, "cannot get field of non-record"); {exp=Translate.error(), ty=Types.UNIT})
                end
            | trvar(Absyn.SubscriptVar(var, exp, pos)) =
                let val {exp=var_tree, ty=ty} = trvar(var)
                    val {exp=exp_tree, ty=actual_ty} = transExp(level, venv, tenv, exp, false, dummyLabel)
                 in checkType(tenv, actual_ty, Types.INT, pos);
                    case ty of Types.ARRAY(ty, _) => {exp=Translate.subscriptVar(var_tree, exp_tree), ty=actualType(tenv, ty)}
                               | _ => (ErrorMsg.error(pos, "cannot subscript a nonarray"); {exp=Translate.error(), ty=Types.UNIT})
                end
         in trvar(var)
        end

    and transDec(level, venv, tenv, dec, break) =
        let fun transparams(params) =
                let fun transparam{name, typ, escape, pos} =
                        case Symbol.look(tenv, typ) of SOME t => {name=name, ty=actualType(tenv, t)}
                                                     | NONE => (ErrorMsg.error(pos, "type not found"); {name=name, ty=Types.UNIT})
                    in map transparam params
                end

            fun addFunctionHeaders(venv, {name, params, body, pos, result} :: fundecs) =
                let val result_ty = case result of SOME(rt, rt_pos) => (case Symbol.look(tenv, rt) of SOME t => actualType(tenv, t) | NONE => (ErrorMsg.error(pos, "function result type not found"); Types.UNIT))
                                                   | NONE => Types.UNIT
                    val funlabel = Temp.namedlabel(Symbol.name name  ^ "_" ^ (Symbol.name (Temp.newlabel())))
                    val params' = transparams(params)
                    val venv' = Symbol.enter(venv,
                                             name,
                                             Env.FunEntry{level=Translate.newLevel(
                                                             {parent=level,
                                                               name=funlabel,
                                                               formals=(map (fn {name, escape, typ, pos} => !escape) params)}),
                                                          label=funlabel,
                                                          formals = map #ty params', result=result_ty})
                 in addFunctionHeaders(venv', fundecs)
                end
            | addFunctionHeaders(venv,  nil) = venv

            fun addTypeHeaders(tenv, {name, ty, pos} :: typedecs) =
                addTypeHeaders(Symbol.enter(tenv, name, Types.NAME(name, ref NONE)), typedecs)
                | addTypeHeaders(tenv, nil) = tenv

            fun loopExists(tenv, ty, pos, visitedTypes) =
                if List.exists (fn typ => typ = ty) visitedTypes
                then (ErrorMsg.error(pos, "circular reference"); true)
                else case ty of
                    Types.NAME(_, ref(SOME(typ))) => loopExists(tenv, typ, pos, ty::visitedTypes)
                    | _ => false

            fun trdec(Absyn.FunctionDec(fundecs)) =
                let val venv' = addFunctionHeaders(venv, fundecs)
                 in (map (fn {name, params, result, body, pos} =>
                      let val flevel = (case Symbol.look(venv', name) of SOME(Env.FunEntry{level, label, formals, result}) => level
                                                                         | _ => level)
                          val accesses = Translate.formals(flevel)
                          fun enterparam(({name,ty}, access), venv) = Symbol.enter(venv, name, Env.VarEntry{access=access, ty=ty})
                          val params' = transparams(params)

                          val venv'' = (foldl enterparam venv' (ListPair.zip(params', accesses)))
                          val {exp=exp_tree, ty=actual_ty} = transExp(flevel, venv'', tenv, body, false, break)
                          val result_ty = case result of SOME(rt, rt_pos) => (case Symbol.look(tenv, rt) of SOME t => actualType(tenv, t) | NONE => (ErrorMsg.error(pos, "function result type not found"); Types.UNIT))
                                                         | NONE => Types.UNIT
                       in Translate.procEntryExit{level=flevel, body=exp_tree};
                          if actual_ty = result_ty then () else ErrorMsg.error(pos, "body type does not match function return type")
                      end) fundecs);
                    {venv=venv', tenv=tenv, exp=nil}
                end
            | trdec(Absyn.VarDec{name, escape, typ=NONE, init, pos}) =
                  let val {exp=exp_tree, ty} = transExp(level, venv, tenv, init, false, break)
                      val access = Translate.allocLocal(level)(!escape)
                   in (if ty = Types.NIL then ErrorMsg.error(pos, "cannot determine type of nil in context") else ();
                      {venv=Symbol.enter(venv,
                                         name,
                                         Env.VarEntry{access=access, ty=ty}),
                       tenv=tenv,
                       exp=Translate.translateAssign(Translate.simpleVar(access, level), exp_tree) :: nil})
                  end
            | trdec(Absyn.VarDec{name, escape, typ=SOME (expected_ty_symbol, ty_pos), init, pos}) =
                  let val {exp=exp_tree, ty} = transExp(level, venv, tenv, init, false, break)
                      val expected_ty =
                        case Symbol.look(tenv, expected_ty_symbol) of SOME t => actualType(tenv, t)
                                                                      | NONE => (ErrorMsg.error(pos, "type not found"); ty)
                      val access = Translate.allocLocal(level)(!escape)
                  in if typesMatch(expected_ty, ty) then () else ErrorMsg.error(pos, "unexpected type for variable {" ^ (Symbol.name name) ^ "}");
                     {venv=Symbol.enter(venv,
                                        name,
                                        Env.VarEntry{access=access, ty=expected_ty}),
                      tenv=tenv,
                      exp=Translate.translateAssign(Translate.simpleVar(access, level), exp_tree) :: nil}
                  end
            | trdec(Absyn.TypeDec(typedecs)) =
                let val tenv' = addTypeHeaders(tenv, typedecs)
                 in ((map (fn{name, ty, pos} =>
                     case Symbol.look(tenv', name)
                         of SOME(Types.NAME(_,refType)) => refType := SOME(transTy(tenv', ty))
                            | _ => ErrorMsg.impossible("could not find inserted value")) typedecs);
                    if List.exists (fn{name, ty, pos} => case Symbol.look(tenv', name)
                        of SOME(Types.NAME(_,ref(SOME(typ)))) => loopExists(tenv', typ, pos, nil)
                           | _ => (ErrorMsg.impossible("could not find inserted value"); false)) typedecs
                    then  {venv=venv, tenv=tenv, exp=nil}
                    else {venv=venv, tenv=tenv', exp=nil})
                end
         in trdec(dec)
        end

    and transExp(level, venv, tenv, exp, inloop, break) =
        let fun trexp(Absyn.VarExp(var)) = transVar(level, venv, tenv, var)
            | trexp(Absyn.NilExp) = {exp=Translate.translateNil(), ty=Types.NIL}
            | trexp(Absyn.IntExp(i)) = {exp=Translate.translateInt(i), ty=Types.INT}
            | trexp(Absyn.StringExp(str, _)) = {exp=Translate.translateString(str), ty=Types.STRING}
            | trexp(Absyn.CallExp{func, args, pos}) =
                let val (formals, result, funlevel, label) = case Symbol.look(venv, func)
                        of SOME(Env.FunEntry{level, label, formals, result}) => (formals, result, level, label)
                           | SOME _ => (ErrorMsg.error(pos, "cannot call non-function {" ^ (Symbol.name func) ^ "}"); (nil, Types.UNIT, Translate.outermost, Temp.newlabel()))
                           | NONE => (ErrorMsg.error(pos, "undefined function {" ^ (Symbol.name func) ^ "}"); (nil, Types.UNIT, Translate.outermost, Temp.newlabel()))
                    val (arg_exps, arg_types) = (ListPair.unzip (map (fn exp => let val {exp=tree, ty=ty} = trexp(exp) in (tree, actualType(tenv, ty)) end) args))
                 in if arg_types = formals
                    then {exp=Translate.translateCall(label, funlevel, arg_exps, level, Env.base_venv), ty=result}
                    else (ErrorMsg.error(pos, "invalid function parameters for {" ^ (Symbol.name func) ^ "}"); {exp=Translate.error(), ty=result})
                end
            | trexp(Absyn.OpExp{left, oper, right, pos}) =
                let val {exp=right_tree, ty=right_ty} = trexp(right)
                    val {exp=left_tree, ty=left_ty} = trexp(left)
                    fun intOrString(left_ty,right_ty) =
                        case right_ty of Types.NIL => ErrorMsg.error(pos, "wrong type for binary op")
                                       | _ => checkType(tenv, right_ty, left_ty, pos)
                  in (case oper of Absyn.EqOp => checkType(tenv, left_ty, right_ty, pos)
                                  | Absyn.NeqOp => checkType(tenv, left_ty, right_ty, pos)
                                  | Absyn.LtOp  => intOrString(left_ty, right_ty)
                                  | Absyn.GtOp  => intOrString(left_ty, right_ty)
                                  | Absyn.LeOp  => intOrString(left_ty, right_ty)
                                  | Absyn.GeOp  => intOrString(left_ty, right_ty)
                                  | _ =>  (checkType(tenv, left_ty, Types.INT, pos); checkType(tenv, right_ty, Types.INT, pos)));
                     case right_ty of Types.STRING => (case oper of Absyn.EqOp => {exp=Translate.translateStringEq(left_tree, right_tree), ty=Types.INT}
                                                                  | Absyn.NeqOp => {exp=Translate.translateStringNeq(left_tree, right_tree), ty=Types.INT}
                                                                  | _ => (ErrorMsg.error(pos, "invalid operation on strings"); {exp=Translate.error(), ty=Types.INT}))
                                   |  _ => {exp=Translate.translateOp(left_tree, oper, right_tree), ty=Types.INT}
                end
            | trexp(Absyn.RecordExp{fields, typ, pos}) =
                let val namedTy = case Symbol.look(tenv, typ)
                        of SOME(Types.NAME(_, ref (SOME(ty)))) => ty
                           | SOME _ => (ErrorMsg.error(pos, "invalid record type"); Types.UNIT)
                           | NONE => (ErrorMsg.error(pos, "type not found"); Types.UNIT)
                    val (recordFields, unique) = case actualType(tenv, namedTy)
                        of Types.RECORD(recordFields, unique) => (recordFields, unique)
                           |  _  => (ErrorMsg.error(pos, "invalid record type"); (nil, ref ()))
                    val recordFieldsExist = List.all (fn (symbol, ty) => List.exists (fn (actual_symbol, exp, pos) =>
                       let val {exp=_, ty=actual_ty} = trexp(exp)
                        in if symbol = actual_symbol then if typesMatch(actualType(tenv, ty), actual_ty) then true else (ErrorMsg.error(pos, "record types do not match"); false) else  false
                       end) fields) recordFields
                   val fieldsExistInRecord = List.all (fn (actual_symbol, exp, pos) => List.exists (fn (symbol, ty) =>
                      let val {exp=_, ty=actual_ty} = trexp(exp)
                       in if symbol = actual_symbol then if typesMatch(actualType(tenv, ty), actual_ty) then true else (ErrorMsg.error(pos, "record types do not match"); false) else  false
                      end) recordFields) fields
                    val (field_trees, field_types) = ListPair.unzip (map (fn (name, exp, pos) => let val {exp=tree, ty=ty} = trexp(exp) in (tree, actualType(tenv, ty)) end) fields)
                 in
                    (if recordFieldsExist andalso fieldsExistInRecord then () else ErrorMsg.error(pos, "fields do not match");
                    {exp=Translate.translateRecord(field_trees), ty=Types.RECORD(recordFields, unique)})
                end
            | trexp(Absyn.SeqExp(nil)) = {exp=Translate.translateSeq(nil), ty=Types.UNIT}
            | trexp(Absyn.SeqExp(explist)) =
                let val (exp_trees, exp_types) =
                    ListPair.unzip(map (fn (exp, pos) =>
                                            let val {exp=e, ty=t} = trexp(exp)
                                             in (e,t)
                                            end)
                                        explist)
                 in {exp=Translate.translateSeq(exp_trees), ty=(List.last exp_types)}
                end
            | trexp(Absyn.AssignExp{var, exp, pos}) =
                let val {exp=left_tree, ty=expected_ty} = transVar(level, venv, tenv, var)
                    val {exp=right_tree, ty=actual_ty} = trexp(exp)
                 in if typesMatch(actual_ty, expected_ty)
                    then {exp=Translate.translateAssign(left_tree, right_tree), ty=Types.UNIT}
                    else (ErrorMsg.error(pos, "mismatched assignment"); {exp=Translate.error(), ty=Types.UNIT})
                end
            | trexp(Absyn.IfExp{test, then', else'=NONE, pos}) =
                let val {exp=test_tree, ty=test_ty} = trexp(test)
                    val {exp=then_tree, ty=then_ty} = trexp(then')
                 in checkType(tenv, test_ty, Types.INT, pos);
                    checkType(tenv, then_ty, Types.UNIT, pos);
                    {exp=Translate.translateIf(test_tree, then_tree), ty=Types.UNIT}
                end
            | trexp(Absyn.IfExp{test, then', else'=SOME(else_exp), pos}) =
                let val {exp=test_tree, ty=test_ty} = trexp(test)
                    val {exp=then_tree, ty=then_ty} = trexp(then')
                    val {exp=else_tree, ty=else_ty} = trexp(else_exp)
                 in checkType(tenv, test_ty, Types.INT, pos);
                    if typesMatch(then_ty, else_ty)
                    then {exp=Translate.translateIfElse(test_tree, then_tree, else_tree), ty=then_ty}
                    else (ErrorMsg.error(pos, "then and else types don't match");
                          {exp=Translate.error(), ty=then_ty})
                end
            | trexp(Absyn.WhileExp{test, body, pos}) =
                let val {exp=test_tree, ty=test_ty} = trexp(test)
                    val done = Temp.newlabel()
                    val {exp=body_tree, ty=body_ty} = transExp(level, venv, tenv, body, true, done)
                 in checkType(tenv, test_ty, Types.INT, pos);
                    checkType(tenv, body_ty, Types.UNIT, pos);
                    {exp=Translate.translateLoop(test_tree, body_tree, done), ty=Types.UNIT}
                end
            | trexp(Absyn.ForExp{var, escape, lo, hi, body, pos}) =
                let val limit = Symbol.symbol "1_limit"
                    val varExp = Absyn.SimpleVar(var, pos)
                 in trexp(Absyn.LetExp{decs=Absyn.VarDec{name=var, escape=ref false, typ=SOME(Symbol.symbol "int", pos), init=lo, pos=pos} ::
                                            (Absyn.VarDec{name=limit, escape=ref false, typ=SOME(Symbol.symbol "int", pos), init=hi, pos=pos} :: nil),
                                       body=Absyn.WhileExp{test=Absyn.OpExp{left=Absyn.VarExp(varExp),
                                                                            oper=Absyn.LeOp,
                                                                            right=Absyn.VarExp(Absyn.SimpleVar(limit, pos)),
                                                                            pos=pos},
                                                           body=Absyn.SeqExp([(body,pos),
                                                                              (Absyn.AssignExp{var=varExp,
                                                                                               exp=Absyn.OpExp{left=Absyn.VarExp(varExp), oper=Absyn.PlusOp, right=Absyn.IntExp(1), pos=pos},
                                                                                               pos=pos},
                                                                              pos)]),
                                                            pos=pos},
                                       pos=pos})
                end
            | trexp(Absyn.BreakExp(pos)) = (if inloop
                                            then ()
                                            else ErrorMsg.error(pos, "break not in loop");
                                            {exp=Translate.translateBreak(break), ty=Types.UNIT})
            | trexp(Absyn.LetExp{decs, body, pos}) =
                let val (venv', tenv', exp_trees) =
                        (foldl (fn (dec, (venv, tenv, exp_trees)) =>
                                   let val {venv=venv', tenv=tenv', exp=exp_tree} = transDec(level, venv, tenv, dec, break)
                                    in (venv', tenv', exp_trees @ exp_tree)
                                   end)
                               (venv, tenv, nil)
                               decs)
                    val {exp=body_tree, ty=ty} = transExp(level, venv', tenv', body, inloop, break)
                 in {exp=Translate.translateLet(exp_trees, body_tree), ty=ty}
                end
            | trexp(Absyn.ArrayExp{typ, size, init, pos}) =
                let val namedTy = case Symbol.look(tenv, typ)
                        of SOME(Types.NAME(_, ref (SOME(ty)))) => ty
                           | SOME _ => (ErrorMsg.error(pos, "invalid array type"); Types.UNIT)
                           | NONE => (ErrorMsg.error(pos, "array type not found"); Types.UNIT)
                    val (ty, unique) = case actualType(tenv, namedTy)
                        of Types.ARRAY(ty, unique)=> (ty, unique)
                           |  _  => (ErrorMsg.error(pos, "invalid array type"); (Types.UNIT, ref ()))
                    val {exp=size_tree, ty=size_ty} = trexp(size)
                    val {exp=init_tree, ty=init_ty} = trexp(init)
                 in checkType(tenv, size_ty, Types.INT, pos);
                    checkType(tenv, init_ty, ty, pos);
                    {exp=Translate.translateArray(size_tree, init_tree), ty=Types.ARRAY(ty, unique)}
                end
         in trexp(exp)
        end
end
