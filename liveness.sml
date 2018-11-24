structure Liveness:
 sig
   datatype igraph = 
     IGRAPH of {graph: Graph.graph,
                tnode: Temp.temp -> Graph.node,
                gtemp: Graph.node -> Temp.temp,
                moves: (Graph.node * Graph.node) list} 

    val interferenceGraph : Flow.flowgraph -> igraph * (Graph.node -> Temp.temp list)

    val show : TextIO.outstream * igraph -> unit
 end = 

struct
  type liveSet = unit Temp.Table.table * Temp.temp list
  type liveMap = liveSet Graph.Table.table

  datatype igraph = 
      IGRAPH of {graph: Graph.graph,
                 tnode: Temp.temp -> Graph.node,
                 gtemp: Graph.node -> Temp.temp,
                 moves: (Graph.node * Graph.node) list} 

  fun interferenceGraph(Flow.FGRAPH{control, def, use, ismove}) =
      let fun addEmpty(node, table) = Graph.Table.enter(table, node, (Temp.Table.empty, nil))
          val nodes = rev(Graph.nodes control)
          val startInMap : liveMap = (foldl addEmpty Graph.Table.empty nodes)
          val startOutMap : liveMap = (foldl addEmpty Graph.Table.empty nodes)

          fun getsome (table, node) = case Graph.Table.look(table, node) of SOME x => x
                                                                          | NONE => (ErrorMsg.impossible ("could not find node " ^ (Graph.nodename node)))
          fun membership(table, element) = case Temp.Table.look(table, element) of SOME x => true
                                                                                 | NONE => false

          fun union((tempTable, temps1), (_, temps2)) =
              (foldl (fn (temp, (table, temps)) => if membership(table, temp)
                                                   then (table, temps)
                                                   else (Temp.Table.enter(table, temp, ()), temp :: temps))
                     (tempTable, temps1)
                     temps2)
          fun difference((tempTable, temps1), temps2) =
              let val tempTable2 = (foldl (fn (temp, table) => Temp.Table.enter(table, temp, ())) Temp.Table.empty temps2)
               in (foldl (fn (temp, (table, temps)) => if membership(tempTable2, temp)
                                                       then (table, temps)
                                                       else (Temp.Table.enter(table, temp, ()), temp :: temps))
                         (Temp.Table.empty, nil)
                         temps1)
              end
          fun equalSets((tempTable, temps1), (_, temps2)) =
              (length temps1) = (length temps2) andalso 
              (List.all (fn (temp) => membership(tempTable, temp))
                        (temps1 @ temps2))

          fun findSets(node, inMap, outMap) =
              let val (outSetTable, outSet) = getsome(outMap, node)
                  val useTemps = getsome(use, node)
                  val useTable = (foldl (fn (temp, table) => Temp.Table.enter(table, temp, ())) Temp.Table.empty useTemps)
                  val inSet' = (union ((useTable, useTemps), 
                                      (difference ((outSetTable, outSet), getsome(def, node)))))
                  val outSet' = (foldl union
                                       (Temp.Table.empty, nil) 
                                       (map (fn node => getsome(inMap, node)) (Graph.succ node)))
               in (Graph.Table.enter(inMap, node, inSet'), Graph.Table.enter(outMap, node, outSet'))
              end

          fun recur(inMap, outMap) =
              let val (inMap', outMap') = (foldl (fn (node, (accInMap, accOutMap)) => findSets(node, accInMap, accOutMap))
                                                 (inMap, outMap)
                                                 nodes)
               in if (List.all (fn node => equalSets(getsome(inMap', node), getsome(inMap, node)) andalso
                                           equalSets(getsome(outMap', node), getsome(outMap, node))) nodes)
                  then (inMap', outMap')
                  else recur(inMap', outMap') 
              end

          val (inMap, outMap) = recur(startInMap, startOutMap)

          val igraph = Graph.newGraph()
          val (tnodeTable, gtempTable) = 
              (foldl (fn (node, (tTable, gTable)) => 
                     foldl (fn (d, (tT, gT)) => 
                               (case Temp.Table.look(tT, d) of SOME _ => (tT, gT) 
                                                             | NONE => let val node = Graph.newNode(igraph) 
                                                                        in (Temp.Table.enter(tT, d, node), Graph.Table.enter(gT, node, d)) 
                                                                       end))
                           (tTable, gTable)
                          ((getsome(def, node)) @ MipsFrame.precolored))
                     (Temp.Table.empty, Graph.Table.empty)
                     nodes)

          fun tnode(temp) = case Temp.Table.look(tnodeTable, temp) of SOME x => x
                                                                    | NONE => ErrorMsg.impossible(MipsFrame.getTemp temp)

          fun gtemp(node) = case Graph.Table.look(gtempTable, node) of SOME x => x
                                                                     | NONE => ErrorMsg.impossible("could not find node")

          fun buildMaps(graph, node) =
              let val defs = getsome(def, node)
                  val (_, liveTemps) = getsome(outMap, node)
                  val isMove = getsome(ismove, node)
                  val movedTemp = getsome(use, node)
                  val liveTemps = if isMove
                                  then List.filter (fn temp => temp <> (hd movedTemp)) liveTemps 
                                  else liveTemps
              in app (fn def => app (fn temp => 
                                        let val fromNode = tnode(def)
                                            val toNode = tnode(temp)
                                        in if List.exists (fn node => Graph.eq(fromNode, node)) (Graph.adj(toNode)) orelse
                                              Graph.eq(fromNode, toNode)
                                           then ()
                                           else Graph.mk_edge{from=tnode(def), to=tnode(temp)}
                                        end)
                                    liveTemps)
                     defs
              end
          fun buildMove(nodes) = (foldl (fn (node, moves) => if getsome(ismove, node)
                                                             then (tnode(hd (getsome(def, node))), tnode(hd (getsome(use, node)))) :: moves
                                                             else moves)
                                        nil
                                        nodes)
       in (app (fn node => buildMaps(igraph, node)) nodes);
          (IGRAPH{graph=igraph, 
                  tnode=tnode,
                  gtemp=gtemp,
                  moves=buildMove(nodes)}, 
           fn (node) => #2 (getsome(outMap, node)))
      end

  fun show(outstream, IGRAPH{graph, tnode, gtemp, moves}) =
      let fun say(s) = TextIO.output(outstream, s)
          fun sayln(s) = (say s; say "\n")
          fun printNode(node) = sayln ((MipsFrame.getTemp o gtemp) node ^ 
                                       " -> " ^
                                       (String.concatWith "," (map (MipsFrame.getTemp o gtemp) (Graph.adj(node)))))
          fun printMove(move : (Graph.node * Graph.node)) = sayln (((MipsFrame.getTemp o gtemp) (#1 move)) ^ " <-> " ^ ((MipsFrame.getTemp o gtemp) (#2 move)))
      in (app printNode (Graph.nodes graph)); (app printMove moves)
      end
end
