structure NodeKey =
struct
    type ord_key = Graph.node
    val compare = (fn (node1, node2) => String.compare(Graph.nodename node1, Graph.nodename node2))
end

structure NodeSet = ListSetFn(NodeKey)