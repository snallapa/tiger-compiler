structure NodePairKey =
struct
    type ord_key = Graph.node * Graph.node
    val compare =
        (fn ((node1, node2), (nodeA, nodeB)) =>
            case String.compare(Graph.nodename node1, Graph.nodename nodeA) 
                of LESS => LESS
                 | EQUAL => String.compare(Graph.nodename node2, Graph.nodename nodeB)
                 | GREATER => GREATER)
end

structure NodePairSet = ListSetFn(NodePairKey)