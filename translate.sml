signature TRANSLATE =
sig
  type level
  type access

  val outermost: level
  val newLevel: {parent: level, name: Temp.label,
                  formals: bool list} -> level
  val formals: level -> access list
  val allocLocal: level -> bool -> access

  structure Frame : FRAME

  type frag = Frame.frag

  type exp

  val simpleVar: access * level -> exp
  val fieldVar: exp * int -> exp
  val subscriptVar: exp * exp -> exp

  val translateNil: unit -> exp
  val translateInt: int -> exp
  val translateString: string -> exp
  val translateCall: Symbol.symbol * level * exp list * level * 'a Symbol.table -> exp
  val translateOp: exp * Absyn.oper * exp -> exp
  val translateRecord: exp list -> exp
  val translateAssign: exp * exp -> exp
  val translateSeq: exp list -> exp
  val translateIf: exp * exp -> exp
  val translateIfElse: exp * exp * exp -> exp
  val translateLoop: exp * exp * Temp.label  -> exp
  val translateBreak: Temp.label -> exp
  val translateArray: exp * exp -> exp
  val translateLet: exp list * exp -> exp
  val translateStringEq: exp * exp -> exp
  val translateStringNeq: exp * exp -> exp

  val error: unit -> exp

  val procEntryExit: { level: level, body: exp} -> unit
  val getResult: unit -> Frame.frag list
end

structure Translate : TRANSLATE =
struct
  structure Frame = MipsFrame
  type frag = Frame.frag

  datatype level = Level of {frame : Frame.frame , parent : level option, unique: unit ref}
  type access = level * Frame.access
  datatype exp = Ex of Tree.exp
               | Nx of Tree.stm
               | Cx of Temp.label * Temp.label -> Tree.stm

  val outermost : level = Level{frame=Frame.newFrame{name=Temp.namedlabel "main", formals=[true]}, parent=NONE, unique=ref ()}
  val fragList : Frame.frag list ref = ref nil

  fun newLevel{parent, name, formals} = Level{frame = Frame.newFrame({name=name, formals=true :: formals}), parent=SOME(parent), unique=ref ()}

  fun formals(Level{frame, parent, unique}) = (map (fn formal => (Level{frame=frame, parent=parent, unique=unique}, formal)) (tl(Frame.formals(frame))))
  fun allocLocal(Level{frame, parent, unique}) escape = (Level{frame=frame, parent=parent, unique=unique}, Frame.allocLocal(frame)(escape))

  fun getStaticLink((Level{frame, parent, unique}, k), Level{frame=currentFrame, parent=currentParent, unique=currentUnique}) =
      if unique = currentUnique
      then Tree.TEMP(Frame.FP)
      else Tree.MEM(Tree.BINOP(Tree.PLUS,
                               Tree.CONST(case hd(Frame.formals(currentFrame)) of Frame.InFrame(i) => i
                                                                                | _ => (ErrorMsg.impossible("static link cannot be in register"); 0)),
                               getStaticLink((Level{frame=frame, parent=parent, unique=unique},
                                             k),
                                            case currentParent of SOME(level) => level | _ => (ErrorMsg.impossible("could not static link to existing variable");
                                                                                                Level{frame=frame, parent=parent, unique=unique}))))

  fun error() = Ex(Tree.CONST(0))

  val emptyTree = Tree.CONST(0)

  fun seq(nil) = Tree.EXP(emptyTree)
      | seq(first :: nil) = first
      | seq(first :: rest) = Tree.SEQ(first, seq(rest))

  and unEx(Ex(exp)) = exp
      | unEx(Nx(stm)) = Tree.ESEQ(stm, emptyTree)
      | unEx(Cx(func)) =
          let val r = Temp.newtemp()
              val t = Temp.newlabel()
              val f = Temp.newlabel()
           in Tree.ESEQ(seq[Tree.MOVE(Tree.TEMP(r), Tree.CONST(1)),
                            func(t, f),
                            Tree.LABEL(f),
                            Tree.MOVE(Tree.TEMP(r), Tree.CONST(0)),
                            Tree.LABEL(t)],
                        Tree.TEMP(r))
          end

  and unNx(Ex(exp)) = Tree.EXP(exp)
      | unNx(Nx(stm)) = stm
      | unNx(Cx(func)) =
          let val n  = Temp.newlabel()
           in seq[func(n, n),
                  Tree.LABEL(n)]
          end

  and unCx(Ex(exp)) = (fn (t, f) => Tree.CJUMP(Tree.NE, exp, Tree.CONST(0), t, f))
      | unCx(Nx(stm)) = (
          ErrorMsg.impossible("cannot use an nx inside a cx. typechecker should not allow this");
          (fn (t, f) => Tree.EXP(emptyTree)))
      | unCx(Cx(func)) = func

  fun simpleVar(access, currentLevel) = case access of (level, Frame.InFrame(i)) => Ex(Frame.exp(Frame.InFrame(i))(getStaticLink((level, i), currentLevel)))
                                                       | (_, Frame.InReg(k)) => Ex(Tree.TEMP(k))
  fun fieldVar(var_tree, offset) = Ex(Tree.MEM(Tree.BINOP(Tree.PLUS,
                                                          unEx(var_tree),
                                                          Tree.BINOP(Tree.MUL,
                                                                     Tree.CONST(Frame.wordSize),
                                                                     Tree.CONST(offset)))))
  fun subscriptVar(var_tree, exp_tree) = Ex(Tree.MEM(Tree.BINOP(Tree.PLUS,
                                                                Tree.BINOP(Tree.PLUS,
                                                                           unEx(var_tree),
                                                                           Tree.BINOP(Tree.MUL,
                                                                                      Tree.CONST(Frame.wordSize),
                                                                                      unEx(exp_tree))),
                                                                Tree.CONST 4)))

  fun translateNil() = Ex(emptyTree)

  fun translateInt(i) = Ex(Tree.CONST(i))

  fun translateString(lit) =
      let val find = List.find (fn frag => case frag of Frame.STRING(lab, s) => lit = s
                                                      | _ => false)
                               (!fragList)
          val lab = Temp.newlabel()
      in case find of SOME(Frame.STRING(lab, _))  => Ex(Tree.NAME(lab))
                   | _ => (fragList := Frame.STRING(lab, lit) :: !fragList; Ex(Tree.NAME(lab)))
      end

  fun translateCall(func, Level({frame=funframe, parent=funparent, unique=fununique}), args, currentlevel, builtin_env) =
      let fun followSl(Level({frame=frame, parent=fparent, unique}), Level({frame=cframe, parent=SOME(cparent), unique=current})) =
              let val sl = case hd(Frame.formals(cframe)) of Frame.InFrame(i) => i
                                                          | Frame.InReg(_) => (ErrorMsg.impossible "static link cannot be in register"; 0)
                  val Level{frame=_, parent=_, unique=cpunique} = cparent
              in if unique = cpunique
                 then SOME(Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.CONST sl, Tree.TEMP(Frame.FP))))
                 else case followSl(Level({frame=frame, parent=fparent, unique=unique}), cparent) of
                          SOME(tree) => SOME(Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.CONST sl, tree)))
                        | NONE =>  NONE
              end
            | followSl(Level({frame=frame, parent=fparent, unique}), Level({frame=cframe, parent=NONE, unique=current})) = NONE
          val slTree = case funparent of SOME fparent =>
                                      (case followSl(fparent, currentlevel) of SOME tree => tree
                                                                       | NONE => Tree.TEMP Frame.FP)
                                    | NONE => Tree.TEMP Frame.FP
          val isBuiltIn = case Symbol.look(builtin_env, func) of SOME x => true
                                                                | NONE => false
      in if isBuiltIn
         then Ex(Frame.externalCall(Symbol.name func, map unEx args))
         else Ex(Tree.CALL(Tree.NAME(func), 
                        slTree :: (map unEx args)))
      end
  fun translateStringEq(left, right) = Ex(Frame.externalCall("stringEqual", [unEx(left), unEx(right)]))

  fun translateStringNeq(left, right) = Ex(Frame.externalCall("_not", [unEx(translateStringEq(left, right))]))

  fun translateOp(left, oper, right) =
      case oper of Absyn.PlusOp => Ex(Tree.BINOP(Tree.PLUS, unEx(left), unEx(right)))
                   | Absyn.MinusOp => Ex(Tree.BINOP(Tree.MINUS, unEx(left), unEx(right)))
                   | Absyn.TimesOp => Ex(Tree.BINOP(Tree.MUL, unEx(left), unEx(right)))
                   | Absyn.DivideOp => Ex(Tree.BINOP(Tree.DIV, unEx(left), unEx(right)))
                   | Absyn.EqOp => Cx(fn (t, f) => Tree.CJUMP(Tree.EQ, unEx(left), unEx(right), t, f))
                   | Absyn.NeqOp => Cx(fn (t, f) => Tree.CJUMP(Tree.NE, unEx(left), unEx(right), t, f))
                   | Absyn.LtOp => Cx(fn (t, f) => Tree.CJUMP(Tree.LT, unEx(left), unEx(right), t, f))
                   | Absyn.GtOp => Cx(fn (t, f) => Tree.CJUMP(Tree.GT, unEx(left), unEx(right), t, f))
                   | Absyn.LeOp => Cx(fn (t, f) => Tree.CJUMP(Tree.LE, unEx(left), unEx(right), t, f))
                   | Absyn.GeOp => Cx(fn (t, f) => Tree.CJUMP(Tree.GE, unEx(left), unEx(right), t, f))

  fun translateRecord(fields) =
      let val r = Temp.newtemp()
          fun addField(exp, offset) = Tree.MOVE(Tree.MEM(Tree.BINOP(Tree.PLUS,
                                                                    Tree.TEMP(r),
                                                                    Tree.CONST(offset * Frame.wordSize))),
                                                 unEx(exp))
      in Ex(Tree.ESEQ(Tree.SEQ(Tree.MOVE(Tree.TEMP(r),
                                         Frame.externalCall("allocRecord",
                                                   [Tree.CONST(Frame.wordSize * (length fields))])),
                               seq(map (fn ((exp, offset)) => addField(exp, offset))
                                    (ListPair.zip(fields, List.tabulate(length fields, fn x => x))))),
                      Tree.TEMP(r)))
      end

  fun translateSeq(nil) = Nx(Tree.EXP(emptyTree))
      | translateSeq(exp :: nil) = exp
      | translateSeq(exp :: exps) = Ex(Tree.ESEQ(unNx(exp), unEx(translateSeq(exps))))

  fun translateAssign(var, exp) = Nx(Tree.MOVE(unEx(var), unEx(exp)))

  fun translateIf(test, then') =
      let val t = Temp.newlabel()
          val done = Temp.newlabel()
       in Nx(seq[unCx(test)(t, done),
              Tree.LABEL(t),
              unNx(then'),
              Tree.JUMP(Tree.NAME(done), [done]),
              Tree.LABEL(done)])
      end

  fun translateIfElse(test, Nx(then'), Nx(else')) =
          let val t = Temp.newlabel()
              val e = Temp.newlabel()
              val done = Temp.newlabel()
           in Nx(seq[unCx(test)(t, e),
                     Tree.LABEL(t),
                     then',
                     Tree.JUMP(Tree.NAME(done), [done]),
                     Tree.LABEL(e),
                     else',
                     Tree.JUMP(Tree.NAME(done), [done]),
                     Tree.LABEL(done)])
          end
      | translateIfElse(test, Cx(then'), Ex(Tree.CONST(0))) =
          let val z = Temp.newlabel()
           in Cx(fn (t, f) => seq[unCx(test)(z, f),
                                 Tree.LABEL(z),
                                 then'(t, f)])
          end
      | translateIfElse(test, Ex(Tree.CONST(1)), Cx(else')) =
          let val z = Temp.newlabel()
           in Cx(fn (t, f) => seq[unCx(test)(t, z),
                                 Tree.LABEL(z),
                                 else'(t, f)])
          end
      | translateIfElse(test, then', else') =
          let val t = Temp.newlabel()
              val r = Temp.newtemp()
              val e = Temp.newlabel()
              val done = Temp.newlabel()
           in Ex(Tree.ESEQ(seq[unCx(test)(t, e),
                            Tree.LABEL(t),
                            Tree.MOVE(Tree.TEMP(r), unEx(then')),
                            Tree.JUMP(Tree.NAME(done), [done]),
                            Tree.LABEL(e),
                            Tree.MOVE(Tree.TEMP(r), unEx(else')),
                            Tree.JUMP(Tree.NAME(done), [done]),
                            Tree.LABEL(done)],
                        Tree.TEMP(r)))
          end

  fun translateLoop(test, body, done) =
    let val start = Temp.newlabel()
        val doTest = Temp.newlabel()
     in Nx(seq[Tree.JUMP(Tree.NAME(doTest), [doTest]),
            Tree.LABEL(start),
            unNx(body),
            Tree.LABEL(doTest),
            unCx(test)(start, done),
            Tree.LABEL(done)])
    end

  fun translateBreak(label) = Nx(Tree.JUMP(Tree.NAME(label), [label]))

  fun translateArray(size, init) = Ex(Frame.externalCall("initArray", [Tree.BINOP(Tree.PLUS, unEx(size), Tree.CONST 1), unEx(init)]))

  fun translateLet(nil, body) = body
      | translateLet(decs, body) = Ex(Tree.ESEQ(seq((map unNx decs)),
                                                unEx(body)))

  fun procEntryExit{level=Level{frame, parent, unique}, body} =
      fragList := Frame.PROC{body=Frame.procEntryExit1(frame, Tree.MOVE(Tree.TEMP(Frame.RV), unEx(body))),
                             frame=frame}
                  :: !fragList
  fun getResult() = !fragList
end
