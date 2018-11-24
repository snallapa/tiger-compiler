structure MipsFrame : FRAME =
struct
  datatype access = InFrame of int | InReg of Temp.temp
  type frame = Temp.label * access list * int ref
  type register = string

  fun externalCall(s, args) =
      Tree.CALL(Tree.NAME(Temp.namedlabel(s)), args)

  val FP = Temp.newtemp()
  val RV = Temp.newtemp()
  val SP = Temp.newtemp()
  val RA = Temp.newtemp()
  val ZERO = Temp.newtemp()
  val GP = Temp.newtemp()
  val specialregs = [(ZERO, "$r0"),
                     (Temp.newtemp(), "$at"),
                     (Temp.newtemp(), "$k0"),
                     (Temp.newtemp(), "$k1"),
                     (GP, "$gp"),
                     (SP, "$sp"),
                     (FP, "$fp"),
                     (RV, "$v0"),
                     (RA, "$ra")]

  val argregs = [Temp.newtemp(),
                 Temp.newtemp(),
                 Temp.newtemp(),
                 Temp.newtemp()]

  val calleesaves = [Temp.newtemp(),
                     Temp.newtemp(),
                     Temp.newtemp(),
                     Temp.newtemp(),
                     Temp.newtemp(),
                     Temp.newtemp(),
                     Temp.newtemp(),
                     Temp.newtemp(),
                     Temp.newtemp(),
                     Temp.newtemp()]

  val callersaves = [Temp.newtemp(),
                     Temp.newtemp(),
                     Temp.newtemp(),
                     Temp.newtemp(),
                     Temp.newtemp(),
                     Temp.newtemp(),
                     Temp.newtemp(),
                     Temp.newtemp()]

  val calldefs = callersaves @ [RV, FP, RA] @ argregs 

  val precolored = calleesaves @ calldefs

  val tempMap = let fun stringify(str, nil, i) = nil
                      | stringify(str, item :: list, i) = (item, str ^ (Int.toString(i))) :: stringify(str, list, i+1)
                    val values = specialregs
                                  @ (stringify ("$a", argregs, 0))
                                  @ (stringify ("$t", calleesaves, 0))
                                  @ (stringify ("$s", callersaves, 0))
                    fun append((temp, name), table) = Temp.Table.enter(table, temp, name)
                 in (foldl append Temp.Table.empty values)
                end

  val wordSize = 4
  fun exp access exp = case access of InReg(reg) => Tree.TEMP(reg)
                                      | InFrame(k) => Tree.MEM(Tree.BINOP(Tree.PLUS, exp, Tree.CONST(k)))

  fun allocFormals((label, accesses, _), nil, _) = (label, rev accesses, ref 1)
     | allocFormals((label, accesses, count), formal :: formals, regParams) =
       if formal orelse regParams >= 4
       then allocFormals((label, InFrame(!count * ~wordSize) :: accesses, ref (!count + 1)), formals, regParams)
       else allocFormals((label, InReg(Temp.newtemp()) :: accesses, ref (!count + 1)), formals, regParams + 1)

  fun newFrame{name, formals} = allocFormals((name, nil, ref 0), formals, 0)
  fun name((label, _, _)) = label
  fun formals((_, accesses, _)) = accesses
  fun allocLocal (_, _, count) escape =
      if escape
      then let val access = InFrame(!count * wordSize)
            in count := !count + 1;
               access
           end
      else InReg(Temp.newtemp())

  fun procEntryExit1(frame, stm) =
      let val calleeTemps = (map (fn _ => Temp.newtemp()) calleesaves)
          val restoreCallees = ListPair.foldlEq (fn (callee, temp, tree) =>
                                                    Tree.SEQ(Tree.MOVE(Tree.TEMP callee, Tree.TEMP temp), tree))
                                                (Tree.MOVE(Tree.TEMP (hd calleesaves), Tree.TEMP (hd calleeTemps)))
                                                (tl calleesaves, tl calleeTemps)
          val RATemp = Temp.newtemp()
          val restoreRATree = Tree.SEQ(Tree.MOVE(Tree.TEMP RA, Tree.TEMP RATemp), restoreCallees)
          val stm' = Tree.SEQ(stm, restoreRATree)
          val argTree = 
              (#2 (foldl (fn (arg, (i, tree)) =>
                             if i >= (length(argregs)) 
                             then (i, tree)
                             else case arg of InReg(reg) => (i + 1, Tree.SEQ(Tree.MOVE(Tree.TEMP reg, Tree.TEMP (List.nth (argregs, i))), tree))
                                            | InFrame(k) => (i + 1, Tree.SEQ(Tree.MOVE(Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.CONST k, Tree.TEMP FP)),
                                                                                       Tree.TEMP (List.nth (argregs, i))),
                                                                             tree)))
                         (0, stm')
                         (formals frame)))
          val saveRATree = Tree.SEQ(Tree.MOVE(Tree.TEMP RATemp, Tree.TEMP RA), argTree)
          val calleesavesTree =
              ListPair.foldlEq (fn (callee, temp, tree) =>
                                   Tree.SEQ(Tree.MOVE(Tree.TEMP temp, Tree.TEMP callee), tree))
                               saveRATree
                               (calleesaves, calleeTemps)
      in calleesavesTree
      end

  fun string(lab,s) = ".data\n" ^ Symbol.name lab ^ ": " ^ ".word  " ^ (Int.toString(size(s)))  ^ "\n .ascii \"" ^ (String.toString s) ^ "\"\n"

  fun procEntryExit2(frame, body) = body @ [Assem.OPER{assem="", src=calleesaves @ [RV, FP], dst=[], jump=SOME([])}]

  fun procEntryExit3((name, locals, count), body) =
      let val fpSpot = (!count) * wordSize
          val frameSize = (count := !count + 1; (!count * wordSize) + 24) (* 6 is the maximum outgoing parameters on a stack. 10 total *)
      in 
          {prolog=".text\n" ^
                  Symbol.name name ^ ":\n" ^
                  "sw $fp " ^ (Int.toString(fpSpot)) ^ "($sp)\n" ^
                  "move $fp $sp\n" ^
                  "addi $sp $sp -" ^ (Int.toString(frameSize)) ^ "\n",
           body=body,
           epilog="move $sp $fp\n" ^
                  "lw $fp " ^ (Int.toString(fpSpot)) ^ "($fp)\n" ^
                  "jr $ra\n"}
      end
  fun getTemp temp = case Temp.Table.look(tempMap, temp) of SOME(reg) => reg
                                                             | NONE => Temp.makestring temp

  datatype frag = PROC of { body: Tree.stm, frame: frame}
                | STRING of Temp.label * string
end
