structure MakeGraph :
sig
    val instrs2graph: Assem.instr list -> Flow.flowgraph * Graph.node list
end =

struct
    fun instrs2graph(instrs) =
        let val labelMap : (Temp.label, Graph.node) HashTable.hash_table = 
                HashTable.mkTable (fn symbol => HashString.hashString(Symbol.name symbol), op =)
                                  (length instrs, Fail "Keyword not found")
            fun handleInstrs(nil) = (Flow.FGRAPH{control=Graph.newGraph(), 
                                                 def=Graph.Table.empty, 
                                                 use=Graph.Table.empty, 
                                                 ismove=Graph.Table.empty}, nil)
              | handleInstrs(Assem.OPER{assem=a, dst, src, jump=NONE} :: instrs) =
                  let val (Flow.FGRAPH{control, def, use, ismove}, nodes) = handleInstrs(instrs)
                      val node = Graph.newNode(control)
                   in Graph.mk_edge{from=node, to=(hd nodes)};
                      (Flow.FGRAPH{control=control,
                                   def=Graph.Table.enter(def, node, dst),
                                   use=Graph.Table.enter(use, node, src),
                                   ismove=Graph.Table.enter(ismove, node, false)},
                       node :: nodes)
                  end
              | handleInstrs(Assem.OPER{assem=a, dst, src, jump=SOME labs} :: instrs) =
                  let val (Flow.FGRAPH{control, def, use, ismove}, nodes) = handleInstrs(instrs)
                      val node = Graph.newNode(control)
                   in (Flow.FGRAPH{control=control,
                                   def=Graph.Table.enter(def, node, dst),
                                   use=Graph.Table.enter(use, node, src),
                                   ismove=Graph.Table.enter(ismove, node, false)},
                       node :: nodes)
                  end
              | handleInstrs(Assem.MOVE{assem=a, dst, src} :: instrs) =
                  let val (Flow.FGRAPH{control, def, use, ismove}, nodes) = handleInstrs(instrs)
                      val node = Graph.newNode(control)
                   in Graph.mk_edge{from=node, to=(hd nodes)};
                      (Flow.FGRAPH{control=control,
                                   def=Graph.Table.enter(def, node, [dst]),
                                   use=Graph.Table.enter(use, node, [src]),
                                   ismove=Graph.Table.enter(ismove, node, true)},
                       node :: nodes)
                  end
              | handleInstrs(Assem.LABEL{assem=a, lab} :: instrs) = 
                  let val (Flow.FGRAPH{control, def, use, ismove}, nodes) = handleInstrs(instrs)
                      val node = if null nodes then Graph.newNode(control) else (hd nodes)
                   in HashTable.insert(labelMap)(lab, node);
                      if null nodes 
                      then (Flow.FGRAPH{control=control,
                                        def=Graph.Table.enter(def, node, []),
                                        use=Graph.Table.enter(use, node, []),
                                        ismove=Graph.Table.enter(ismove, node, false)},
                            node :: nodes)
                      else (Flow.FGRAPH{control=control,
                                        def=def,
                                        use=use,
                                        ismove=ismove}, nodes)
                  end
            val (graph, nodes) = handleInstrs(instrs)
            val instrs' = List.filter (fn (instr) => 
                                          case instr of Assem.LABEL{assem, lab} => false
                                                      | _ => true) 
                                      instrs
            fun handleJumps(Assem.OPER{assem=_, src=_, dst=_, jump=SOME labs}, (node, SOME(nextNode))) = 
                (app (fn lab => case HashTable.find labelMap lab of
                                     SOME toNode => Graph.mk_edge{from=node, to=toNode}
                                     | NONE => ())
                     labs)
              | handleJumps(_, _) = ()
         in (app handleJumps (ListPair.zipEq(instrs',
                              ListPair.zipEq(nodes, (map SOME (tl nodes)) @ [NONE]))));
            (graph, nodes)
        end
end
