structure Main = struct
   structure Tr = Translate
   structure Frame = MipsFrame

   fun emitproc out (Frame.PROC{body,frame}) =
       let val _ = print ("emit " ^ (Symbol.name(#1(frame))) ^ "\n")
           (*val _ = Printtree.printtree(out,body)*)
           val stms = Canon.linearize body
           (*val _ = (app (fn s => Printtree.printtree(out,s)) stms; TextIO.output(out, "\n"))*)
           val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
           val instrs = Frame.procEntryExit2(frame, List.concat(map (MipsGen.codegen frame) stms'))
           val format0 = Assem.format(Frame.getTemp)
           (* val _ = ((app (fn i => TextIO.output(out,format0 i)) instrs; TextIO.output(out,"\n"))) *)
           val (instrs', regMap) = RegAlloc.alloc(instrs, frame)
           val {prolog, body, epilog} = Frame.procEntryExit3(frame, instrs')
           val format1 = Assem.format(fn temp => case Temp.Table.look(regMap, temp) of SOME(x) => x)
        in TextIO.output(out, prolog);
           (app (fn i => TextIO.output(out,format1 i)) body);
           TextIO.output(out, epilog);
           TextIO.output(out, "\n")
       end

     | emitproc out (Frame.STRING(lab,s)) = 
        TextIO.output(out, Frame.string(lab,s))

   fun withOpenFile fname f =
       let val out = TextIO.openOut fname
        in (f out before TextIO.closeOut out)
           handle e => (TextIO.closeOut out; raise e)
       end

   fun compile filename =
       let val absyn = Parse.parse filename
           val frags = (FindEscape.prog absyn; SemAnt.transProg absyn)
        in withOpenFile (filename ^ ".s")
              (fn out => (app (emitproc out) frags))
       end
end
