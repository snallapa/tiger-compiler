structure RegAlloc : REG_ALLOC = 
struct
  structure Frame = MipsFrame
  type allocation = Frame.register Temp.Table.table

 (*used to debug the CFG*)
 fun printNode(outstream, node) =
    TextIO.output(outstream, 
                  Graph.nodename node ^ " -> " ^ 
                     (String.concatWith ", " (map Graph.nodename (Graph.succ node))) ^
                     "" ^ "\n")

  fun alloc(instrs, frame) =
      let val format0 = Assem.format(MipsFrame.getTemp)
          val (cfg, _) = MakeGraph.instrs2graph(instrs)
          val Flow.FGRAPH{control, def, use, ismove} = cfg
          (* val _ = map (fn x => printNode(TextIO.stdOut, x)) (rev (Graph.nodes control)) *)
          val (Liveness.IGRAPH{graph, gtemp, tnode, moves}, liveout) = Liveness.interferenceGraph(cfg)
          val nodes = Graph.nodes graph

          val simplifyWorklist : NodeSet.set ref = ref NodeSet.empty
          val freezeWorklist : NodeSet.set ref = ref NodeSet.empty
          val spillWorklist : NodeSet.set ref = ref NodeSet.empty
          val spilledNodes : NodeSet.set ref = ref NodeSet.empty
          val coalescedNodes : NodeSet.set ref = ref NodeSet.empty
          val coloredNodes : NodeSet.set ref = ref NodeSet.empty
          val selectStack : Graph.node list ref = ref nil

          val coalescedMoves : NodePairSet.set ref = ref NodePairSet.empty
          val constrainedMoves : NodePairSet.set ref = ref NodePairSet.empty
          val frozenMoves : NodePairSet.set ref = ref NodePairSet.empty
          val worklistMoves : NodePairSet.set ref = ref (NodePairSet.addList(NodePairSet.empty, moves))
          val activeMoves : NodePairSet.set ref = ref NodePairSet.empty

          fun init(default) = (foldl (fn (node, table) => Graph.Table.enter(table, node, default)) 
                                     Graph.Table.empty 
                                     nodes)

          val adjSet : NodePairSet.set ref = ref NodePairSet.empty
          val adjList : NodeSet.set Graph.Table.table ref = ref (init(NodeSet.empty))
          val degree : int Graph.Table.table ref = ref (init(0))
          val moveList : NodeSet.set Graph.Table.table ref = ref (init(NodeSet.empty))
          val alias : Graph.node Graph.Table.table ref = ref Graph.Table.empty

          val precolored = NodeSet.addList(NodeSet.empty, map tnode MipsFrame.precolored)
          val _ = (NodeSet.app (fn n1 => 
                                  (NodeSet.app (fn n2 => 
                                      if Graph.eq(n1, n2) orelse
                                         List.exists (fn node => Graph.eq(n1, node)) (Graph.adj(n2))
                                      then ()
                                      else Graph.mk_edge({from=n1, to=n2}))
                                  precolored))
                               precolored)
          val initial = List.filter (fn node => not(NodeSet.member(precolored, node))) nodes
          val K = NodeSet.numItems precolored
          val color: MipsFrame.register Graph.Table.table ref =
              ref (NodeSet.foldl (fn (n, table) => Graph.Table.enter(table, n, MipsFrame.getTemp(gtemp(n)))) Graph.Table.empty precolored)

          fun getsome(table, node) = case Graph.Table.look(!table, node) of SOME(x) => x
                                                                          | NONE => ErrorMsg.impossible ((Graph.nodename node) ^ " " ^ (MipsFrame.getTemp (gtemp node)))
          fun member(table, node) = case Graph.Table.look(!table, node) of SOME(_) => true
                                                                         | NONE => false
          fun add(el, reflist) = reflist := NodeSet.add(!reflist, el)
          fun delete(el, reflist) = reflist := NodeSet.delete(!reflist, el)
          fun addPair(el, reflist) = reflist := NodePairSet.add(!reflist, el)
          fun deletePair(el, reflist) = reflist := NodePairSet.delete(!reflist, el)

          fun build() = (
              (app (fn (n1, n2) =>
                       (moveList := Graph.Table.enter(!moveList, n1, NodeSet.add(getsome(moveList, n1), n2));
                        moveList := Graph.Table.enter(!moveList, n2, NodeSet.add(getsome(moveList, n2), n1))))
                   moves);
              (app (fn n1 => (app (fn n2 => addEdge(n1, n2)) (Graph.adj n1))) nodes))

          and addEdge(u, v) =
              if not(NodePairSet.member(!adjSet, (u, v))) andalso not(Graph.eq (u, v))
              then (addPair((u, v), adjSet); addPair((v,u), adjSet);
                    if not(NodeSet.member(precolored, u))
                    then (adjList := Graph.Table.enter(!adjList, u, NodeSet.add(getsome(adjList, u), v));
                          degree := Graph.Table.enter(!degree, u, getsome(degree, u) + 1))
                    else ();
                    if not(NodeSet.member(precolored, v))
                    then (adjList := Graph.Table.enter(!adjList, v, NodeSet.add(getsome(adjList, v), u));
                          degree := Graph.Table.enter(!degree, v, getsome(degree, v) + 1))
                    else ())
              else ()

          fun makeWorklist() = 
              (app (fn node => if getsome(degree, node) >= K
                               then add(node, spillWorklist)
                               else
                                   if moveRelated(node)
                                   then add(node,freezeWorklist)
                                   else add(node, simplifyWorklist))
                   initial)
          and adjacent(n) = NodeSet.difference(getsome(adjList, n), NodeSet.addList(!coalescedNodes, !selectStack))
          and nodeMoves(n) =
              NodePairSet.intersection(NodePairSet.addList(NodePairSet.empty,
                                                           (map (fn n1 => (n, n1)) (NodeSet.listItems(getsome(moveList, n))))),
                                       NodePairSet.union(!activeMoves, !worklistMoves))
          and moveRelated(n) = not(NodePairSet.isEmpty(nodeMoves(n)))
          and simplify() =
              let val n = hd (NodeSet.listItems (!simplifyWorklist))
               in delete(n, simplifyWorklist);
                  selectStack := n :: !selectStack;
                  (NodeSet.app decrementDegree (adjacent(n)))
              end
                               
          and decrementDegree(m) =
              let val d = getsome(degree, m)
               in degree := Graph.Table.enter(!degree, m, d - 1);
                  if d = K
                  then (enableMoves(NodeSet.add(adjacent(m), m)); 
                        if NodeSet.member(!spillWorklist, m) then delete(m, spillWorklist) else ();
                        add(m, if moveRelated(m) then freezeWorklist else simplifyWorklist))
                  else ()
              end

          and enableMoves(nodes : NodeSet.set) =
              NodeSet.app (fn n => NodePairSet.app (fn m => if NodePairSet.member(!activeMoves, m)
                                                            then (deletePair(m, activeMoves);
                                                                 addPair(m, worklistMoves))
                                                            else ())
                                                   (nodeMoves(n)))
                          nodes
                                       
          fun coalesce() =
              let val m = hd (NodePairSet.listItems (!worklistMoves))
                  val (x, y) = m
                  val (u, v) = if NodeSet.member(precolored, y) then (y, x) else (x, y)
              in deletePair(m, worklistMoves);
                 if Graph.eq(u, v)
                 then (addPair(m, coalescedMoves);
                       addWorkList(u))
                 else
                     if NodeSet.member(precolored, v) orelse NodePairSet.member(!adjSet, (u, v))
                     then (addPair(m, constrainedMoves); addWorkList(u); addWorkList(v))
                     else
                         if (NodeSet.member(precolored, u) andalso List.all (fn t => OK(t, u)) (NodeSet.listItems(adjacent(v))))
                            orelse (not(NodeSet.member(precolored, u)) andalso conservative(NodeSet.union(adjacent(u), adjacent(v))))
                         then (addPair(m, coalescedMoves); combine(u,v); addWorkList(u))
                         else addPair(m, activeMoves)
              end
                  
          and addWorkList(u) =
              if not(NodeSet.member(precolored, u)) andalso not(moveRelated(u)) andalso getsome(degree, u) < K
              then
                  ((if NodeSet.member(!freezeWorklist, u) then delete(u, freezeWorklist) else ()); add(u, simplifyWorklist))
              else ()
          and OK(t,r) = getsome(degree, t) < K orelse NodeSet.member(precolored, t) orelse NodePairSet.member(!adjSet, (t, r))
          and conservative(nodes) = (NodeSet.foldl (fn (node, acc) => if getsome(degree, node) >= K then acc + 1 else acc) 0 nodes) < K
          and getAlias(n) = if NodeSet.member(!coalescedNodes, n) then getAlias(getsome(alias, n)) else n
          and combine(u, v) = (
              (if NodeSet.member(!freezeWorklist, v)
               then delete(v, freezeWorklist)
               else
                   if NodeSet.member(!spillWorklist, v)
                   then delete(v, spillWorklist)
                   else ());
              add(v, coalescedNodes);
              alias := Graph.Table.enter(!alias, v, u);
              moveList := Graph.Table.enter(!moveList, u, NodeSet.union(getsome(moveList, u), getsome(moveList, v)));
              enableMoves(NodeSet.singleton(v));
              (NodeSet.app (fn t => (addEdge(t, u); decrementDegree(t))) (adjacent(v)));
              if getsome(degree, u) >= K andalso NodeSet.member(!freezeWorklist, u)
              then (delete(u, freezeWorklist); add(u, spillWorklist))
              else ())
                                  
          fun freeze() =
              let val u = hd (NodeSet.listItems(!freezeWorklist))
              in delete(u, freezeWorklist);
                 add(u, simplifyWorklist);
                 freezeMoves(u)
              end
          and freezeMoves(u) =
              (NodePairSet.app
                   (fn m =>
                       let val (x, y) = m 
                           val v = if Graph.eq(getAlias(y), getAlias(u)) then getAlias(x) else getAlias(y)
                       in deletePair(m, activeMoves);
                          addPair(m, frozenMoves);
                          if NodePairSet.isEmpty(nodeMoves(v)) andalso getsome(degree, v) < K
                          then (delete(v, freezeWorklist); add(v, simplifyWorklist))
                          else ()
                       end)
                   (nodeMoves(u)))

          fun argmax(nil, acc) = acc
            | argmax((node, degree) :: items, (candidate, highestDegree)) = 
                if degree > highestDegree
                then argmax(items, (node, degree))
                else argmax(items, (candidate, highestDegree))

          fun selectSpill() =
              let val candidates = NodeSet.listItems(!spillWorklist)
                  val spillDegrees = (map (fn node => length(Graph.adj node)) candidates)
                  val pairings = ListPair.zipEq(candidates, spillDegrees)
                  val m = #1 (argmax(tl pairings, hd pairings))
               in delete(m, spillWorklist);
                  add(m, simplifyWorklist);
                  freezeMoves(m)
               end

         fun assignColors() = (
             (while not(List.null(!selectStack)) do
                    let val n = hd(!selectStack)
                        val okColors = ref (RegSet.addList(RegSet.empty, (map MipsFrame.getTemp MipsFrame.precolored)))
                     in selectStack := tl (!selectStack);
                        (NodeSet.app (fn w => if (NodeSet.member(NodeSet.union(!coloredNodes, precolored), getAlias(w))
                                                 andalso RegSet.member(!okColors, getsome(color, getAlias(w))))
                                              then okColors := RegSet.delete(!okColors, getsome(color, getAlias(w)))
                                              else ())
                                     (getsome(adjList, n)));
                        if RegSet.isEmpty(!okColors)
                        then add(n, spilledNodes)
                        else (add(n, coloredNodes); color := Graph.Table.enter(!color, n, (hd (RegSet.listItems(!okColors)))))
                    end);
             (NodeSet.app (fn n => if member(color, getAlias(n)) 
                                   then color := Graph.Table.enter(!color, n, getsome(color, getAlias(n))) 
                                   else ()) 
                          (!coalescedNodes)))

         fun replaceTemp(temps, oldTemp, newTemp) = (map (fn t => if t = oldTemp then newTemp else t) temps)

         fun rewriteInstr(Assem.LABEL{assem, lab}, temp, _) = [Assem.LABEL{assem=assem, lab=lab}]
           | rewriteInstr(Assem.MOVE{assem, src, dst}, spilledTemp, Frame.InFrame(loc)) =
              if src = spilledTemp
              then [Assem.OPER{assem="lw `d0, " ^ (Int.toString(loc)) ^ "(`s0)\n", src=[Frame.FP], dst=[dst], jump=NONE}]
              else if dst = spilledTemp
                   then [Assem.OPER{assem="sw `s0, " ^ (Int.toString(loc)) ^ "(`s1)\n", src=[src, Frame.FP], dst=[], jump=NONE}]
                   else [Assem.MOVE{assem=assem, src=src, dst=dst}]
           | rewriteInstr(Assem.OPER{assem, src, dst, jump}, spilledTemp, Frame.InFrame(loc)) =
              let val newTemp = Temp.newtemp()
                  val load = Assem.OPER{assem="lw `d0, " ^ (Int.toString(loc)) ^ "(`s0)\n", src=[Frame.FP], dst=[newTemp], jump=NONE}
                  val store = Assem.OPER{assem="sw `s0, " ^ (Int.toString(loc)) ^ "(`s1)\n", src=[newTemp, Frame.FP], dst=[], jump=NONE}
                  val inSrc = List.exists (fn temp => temp = spilledTemp) src
                  val inDst = List.exists (fn temp => temp = spilledTemp) dst
               in if inSrc andalso inDst
                  then [load, Assem.OPER{assem=assem, 
                                         src=replaceTemp(src, spilledTemp, newTemp), 
                                         dst=replaceTemp(dst, spilledTemp, newTemp),
                                         jump=jump}, store]
                  else if inSrc
                       then [load, Assem.OPER{assem=assem, src=replaceTemp(src, spilledTemp, newTemp), dst=dst, jump=jump}]
                       else if inDst
                            then [Assem.OPER{assem=assem, src=src, dst=replaceTemp(dst, spilledTemp, newTemp), jump=jump}, store]
                            else [Assem.OPER{assem=assem, src=src, dst=dst, jump=jump}]
              end
           | rewriteInstr(_, _, Frame.InReg(r)) = ErrorMsg.impossible("spilled temps should not go in registers")

          fun flatten(l) = foldr (op @) nil l

          fun rewriteProgram() =
              let val locs = (map (fn node => MipsFrame.allocLocal(frame)(true)) (NodeSet.listItems(!spilledNodes)))
                  val spilledTemps = map gtemp (NodeSet.listItems(!spilledNodes))
               in ListPair.foldl (fn (spilledTemp, loc, instrs') => 
                                       flatten(map (fn instr => rewriteInstr(instr, spilledTemp, loc)) instrs'))
                                 instrs
                                 (spilledTemps, locs)
              end

          fun isRedundantMove(Assem.MOVE{assem=_, dst, src}) =
              let val dstColor = getsome(color, tnode(dst))
                  val srcColor = getsome(color, tnode(src))
               in dstColor = srcColor
              end
            | isRedundantMove(_) = false

          fun printColors(nodes) = (app (fn node => print((MipsFrame.getTemp(gtemp(node))) ^ " -> " ^ (getsome(color,node)) ^ "\n")) nodes)
       in build();
          makeWorklist();
          while not(List.all NodeSet.isEmpty [!simplifyWorklist, !freezeWorklist, !spillWorklist] 
                    andalso NodePairSet.isEmpty(!worklistMoves)) do
                if not(NodeSet.isEmpty(!simplifyWorklist))
                then simplify()
                else
                    if not(NodePairSet.isEmpty(!worklistMoves))
                    then coalesce()
                    else
                        if not(NodeSet.isEmpty(!freezeWorklist))
                        then freeze()
                        else
                            if not(NodeSet.isEmpty(!spillWorklist))
                            then selectSpill()
                            else ();
          assignColors();
          (* Liveness.show(TextIO.stdOut, Liveness.IGRAPH{graph=graph, gtemp=gtemp, tnode=tnode, moves=moves}); *)
          (*printColors(nodes); print("\n");*)
          if not(NodeSet.isEmpty(!spilledNodes))
          then alloc(rewriteProgram(), frame)
          else (List.filter (not o isRedundantMove) instrs,
                   (foldl (fn (node, table) =>  Temp.Table.enter(table, gtemp(node), getsome(color, node)))
                          Temp.Table.empty
                          nodes))
      end
end                   
