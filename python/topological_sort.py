from collections import deque, defaultdict


PENDING = 0
PROCESSED = 1
IN_PROGRESS = -1

"""
This code shows how to get a topological sort of a DAG, using two
implementations:

1. Kahn's algorithm
2. DFS

See: https://en.wikipedia.org/wiki/Topological_sorting#Algorithms

To illustrate how to use this algorithm, I took the following problem from LeetCode:
https://leetcode.com/problems/course-schedule/
"""
class Solution:
    def canFinish(self, num_courses: int, prerequisites: list[list[int]]) -> bool:
        if not prerequisites:
            return True
        self.graph = self.to_graph(num_courses, prerequisites)
        taken_courses, found_cycle = self.get_topological_sort_kahn()
        return not found_cycle and len(taken_courses) == num_courses

    def to_graph(self, num_courses: int, prerequisites: list[(int, int)]) -> dict[int, list[int]]:
        graph = defaultdict(list)
        # Add independent courses
        for i in range(num_courses):
            graph[i]
        # Add courses with prerequisites
        for (course, prereq) in prerequisites:
            graph[course].append(prereq)
        return graph

    # --------------------------------------------------
    # Topological sort Kahn's algorithm
    # --------------------------------------------------

    def get_topological_sort_kahn(self) -> list[int]:
        order = []
        nodes = self.get_nodes_to_edges()
        no_incoming_nodes = self.get_no_incoming_nodes(nodes)
        while no_incoming_nodes:
            node = no_incoming_nodes.pop()
            order.append(node)
            neighbors = self.graph[node]
            for neig in neighbors:
                nodes[neig] -= 1
                if nodes[neig] == 0:
                    no_incoming_nodes.append(neig)

        return order, len(order) != len(self.graph)

    def get_no_incoming_nodes(self, nodes: dict[int, int]) -> list[int]:
        no_incoming_nodes = []
        for node, edges in nodes.items():
            if edges == 0:
                no_incoming_nodes.append(node)

        return no_incoming_nodes

    def get_nodes_to_edges(self) -> dict:
        nodes = defaultdict(int)
        for node, neighbors in self.graph.items():
            nodes[node]
            for neig in neighbors:
                nodes[neig] += 1
        return nodes

    # --------------------------------------------------
    # Topological sort DFS
    # --------------------------------------------------

    def get_topological_sort_dfs(self) -> list:
        order = deque()
        visited = [PENDING] * len(self.graph)
        self.found_cycle = False
        for node in self.graph:
            if self.found_cycle:
                break
            self.dfs(node, order, visited)

        return list(order), self.found_cycle

    def dfs(self, node: int, order: deque[int], visited: list[int]) -> None:
        if visited[node] == PROCESSED:
            return

        if visited[node] == IN_PROGRESS:
            self.found_cycle = True
            return

        visited[node] = IN_PROGRESS
        neighbors = self.graph[node]
        for neig in neighbors:
            self.dfs(neig, order, visited)

        order.appendleft(node)
        visited[node] = PROCESSED
