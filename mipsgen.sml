structure MipsGen: CODEGEN =
struct
    structure Frame = MipsFrame
    fun codegen frame stm =
        let val ilist = ref (nil: Assem.instr list)
            fun emit x = ilist := x :: !ilist
            fun result(gen) = let val t = Temp.newtemp() in gen t; t end
            fun intToString(i) = if i >= 0 then Int.toString(i) else "-" ^ Int.toString(~i)
      
            fun munchStm(Tree.MOVE(Tree.MEM(Tree.BINOP(Tree.PLUS, e1, Tree.CONST i)), e2)) =
                  emit(Assem.OPER{assem="sw `s0, " ^ intToString(i) ^ "(`s1)\n",
                                  src=[munchExp(e2), munchExp(e1)],
                                  dst=[],
                                  jump=NONE})
              | munchStm(Tree.MOVE(Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.CONST i, e1)), e2)) =
                  emit(Assem.OPER{assem="sw `s0, " ^ intToString(i) ^ "(`s1)\n",
                                  src=[munchExp(e2), munchExp(e1)],
                                  dst=[],
                                  jump=NONE})
              | munchStm(Tree.MOVE(Tree.MEM(e1), e2)) = emit(Assem.OPER{assem="sw `s0, 0(`s1)\n", src=[munchExp(e2), munchExp(e1)], dst=[], jump=NONE})
              | munchStm(Tree.MOVE(Tree.TEMP t, Tree.CALL(Tree.NAME lab, args))) =(
                  emit(Assem.OPER{assem="jal " ^ (Symbol.name lab) ^ "\n", src=munchArgs(args,0), dst=Frame.calldefs, jump=NONE});
                  emit(Assem.MOVE{assem="move `d0 `s0\n", src=Frame.RV, dst=t}))
              | munchStm(Tree.MOVE(Tree.TEMP t1, Tree.TEMP t2)) = emit(Assem.MOVE{assem="move `d0 `s0\n", src=t2, dst=t1})
              | munchStm(Tree.MOVE(Tree.TEMP t, Tree.CONST i)) = emit(Assem.OPER{assem="li `d0, " ^ intToString(i) ^"\n",
                                                                      src=[],
                                                                      dst=[t],
                                                                      jump=NONE})
              | munchStm(Tree.MOVE(Tree.TEMP t, e)) = emit(Assem.MOVE{assem="move `d0, `s0\n",
                                                                      src=munchExp(e),
                                                                      dst=t})
              | munchStm(Tree.CJUMP(oper, e1, e2, lab1, lab2)) =
                  let val opCode = case oper of Tree.EQ => "beq"
                                                | Tree.NE => "bne"
                                                | Tree.LT => "blt"
                                                | Tree.GT => "bgt"
                                                | Tree.LE => "ble"
                                                | Tree.GE => "bge"
                                                | _ => (ErrorMsg.impossible "compiler bug no tiger gives this opcode"; "beq")
                   in emit(Assem.OPER{assem=opCode ^ " `s0, `s1, " ^ (Symbol.name lab1) ^ "\n",
                                      src=[munchExp(e1), munchExp(e2)],
                                      dst=[],
                                      jump=SOME([lab1, lab2])})
                  end
              | munchStm(Tree.JUMP(Tree.NAME lab, labels)) =
                  emit(Assem.OPER{assem="j " ^ (Symbol.name lab) ^ "\n",
                                  src=[],
                                  dst=[],
                                  jump=SOME(labels)})
              | munchStm(Tree.EXP(e)) = (munchExp(e); ())
              | munchStm(Tree.LABEL lab) = emit(Assem.LABEL{assem=(Symbol.name lab) ^ ":\n", lab=lab})
            and munchExp(Tree.MEM(Tree.BINOP(Tree.PLUS, e1, Tree.CONST i))) =
                  result(fn r=> emit(Assem.OPER{assem="lw `d0, " ^ intToString(i) ^ "(`s0)" ^ "\n",
                                                src=[munchExp(e1)],
                                                dst=[r],
                                                jump=NONE}))
              | munchExp(Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.CONST i, e1))) =
                  result(fn r=> emit(Assem.OPER{assem="lw `d0, " ^ intToString(i) ^ "(`s0)" ^ "\n",
                                                src=[munchExp(e1)],
                                                dst=[r],
                                                jump=NONE}))
              | munchExp(Tree.MEM(e)) = result(fn r => emit(Assem.OPER{assem="lw `d0, 0(`s0)\n", src=[munchExp(e)], dst=[r], jump=NONE}))
              | munchExp(Tree.BINOP(Tree.PLUS, e1, Tree.CONST i)) =
                  result(fn r => emit(Assem.OPER{assem="addi `d0, `s0, " ^ intToString(i) ^ "\n",
                                                 src=[munchExp(e1)],
                                                 dst=[r],
                                                 jump=NONE}))
              | munchExp(Tree.BINOP(Tree.PLUS, Tree.CONST i, e1)) = munchExp(Tree.BINOP(Tree.PLUS, e1, Tree.CONST i))
              | munchExp(Tree.BINOP(oper, e1, e2))  =
                  let val opCode = case oper of Tree.PLUS => "add"
                                              | Tree.MINUS => "sub"
                                              | Tree.MUL => "mul"
                                              | Tree.DIV => "div"
                                              | _ => (ErrorMsg.impossible "compiler bug invalid operation code"; "add")
                   in result(fn r => emit(Assem.OPER{assem= opCode ^ " `d0, `s0, `s1\n",
                                                     src=[munchExp(e1), munchExp(e2)],
                                                     dst=[r],
                                                     jump=NONE}))
                  end
              | munchExp(Tree.TEMP t) = t
              | munchExp(Tree.CONST i) = result(fn r => emit(Assem.OPER{assem="li `d0, " ^ intToString(i) ^ "\n",
                                                                        src=[],
                                                                        dst=[r],
                                                                        jump=NONE}))
              | munchExp(Tree.NAME lab) = result(fn r => emit(Assem.OPER{assem="la `d0, " ^ Symbol.name lab ^ "\n", src=[], dst=[r], jump=NONE}))
              | munchExp(Tree.CALL(Tree.NAME lab, args)) = (emit(Assem.OPER{assem="jal " ^ (Symbol.name lab) ^ "\n",
                              src=munchArgs(args,0),
                              dst=Frame.calldefs,
                              jump=NONE}); Frame.RV)
            and munchArgs(nil, i) = nil
                | munchArgs(exp :: exps, i) =
                    if i < 4
                    then
                      let val reg = List.nth(Frame.argregs, i)
                       in emit(Assem.MOVE{assem="move `d0, `s0\n",
                                          src=munchExp(exp),
                                          dst=reg}); reg :: munchArgs(exps, i + 1)
                      end
                    else (emit(Assem.OPER{assem="sw `s0, " ^ intToString (i * ~Frame.wordSize) ^ "($sp)\n",
                                          src=[munchExp(exp)],
                                          dst=[],
                                          jump=NONE}); munchArgs(exps, i + 1))

    in munchStm stm;
       (rev(!ilist))
    end

end
