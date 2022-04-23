# time: O(nodes), space: O(nodes)
def get_disjoint_graphs(graph: dict[str: list[str]]) -> list[list[str]]:
    graphs = []
    if not graph:
        return graphs

    keys = set(graph.keys())  # O(nodes)
    while keys:  # O(nodes)
        node = keys.pop()  # O(1)
        visited = set()
        elements = __traverse(graph, node, visited)
        # ^ O(1*) if it removes all keys then the while loop will end.
        keys = keys - elements
        graphs.append(elements)  # O(1*)
    return graphs


# time: O(nodes) worst case, space: O(nodes), visited set and recursive call stack
def __traverse(graph: dict[str: list[str]], node: str, visited: set[str]) -> set[str]:
    if not node or node in visited:  # O(1)
        return visited

    visited.add(node)  # O(1)
    neighbors = graph.get(node, [])  # O(1)
    for neighbor in neighbors:
        __traverse(graph, neighbor, visited)

    return visited


if __name__ == '__main__':
    graph = {
        'A': ['B', 'C'],
        'B': ['A', 'C'],
        'C': ['B', 'D'],
        'D': ['C'],
        'E': ['F'],
        'F': ['E'],
    }
    graphs = get_disjoint_graphs(graph)

    assert len(graphs) == 2
    graphs.sort(key=lambda x: len(x))
    short_graph, long_graph = graphs[0], graphs[1]

    assert short_graph == {'E', 'F'}
    assert long_graph == {'A', 'B', 'C', 'D'}
