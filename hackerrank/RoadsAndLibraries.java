import java.io.*;
import java.util.*;

// https://www.hackerrank.com/challenges/torque-and-development/problem
public class RoadsAndLibraries {

    private static final String SPACE = " ";

    /**
     * Run breadth first search beginning by the given node.
     *
     * @param node Node to start BFS.
     * @param graph Graph to run BFS over.
     * @param visitedNodes Nodes previously visited.
     * @return The number of connected nodes starting from the given node as parameter.
     */
    private int bfs(int node, Map<Integer, List<Integer>> graph, Map<Integer, Boolean> visitedNodes) {
        Queue<Integer> pendingNodes = new LinkedList<>();
        pendingNodes.add(node);
        int nodes = 0;
        while (!pendingNodes.isEmpty()) {
            int neighbour = pendingNodes.poll();
            boolean isVisited = visitedNodes.getOrDefault(neighbour, false);
            if (!isVisited) {
                List<Integer> nextNeighbours = graph.getOrDefault(neighbour, null);
                if (nextNeighbours != null) {
                    pendingNodes.addAll(nextNeighbours);
                }
                visitedNodes.put(neighbour, true);
                nodes++;
            }
        }
        return nodes;
    }

    /**
     * A list representing the nodes in each connected subgraph within the full graph.
     *
     * The list returned has the following meanings:
     * 1. list.size(): Connected subgraphs in the graph.
     * 2. list.get(i): The number of nodes in the connected subgraph i.
     *
     * @param graph The full graph to search for connected subgraphs.
     * @return List of ints.
     */
    private List<Integer> getConnectedSubGraphNodes(Map<Integer, List<Integer>> graph) {
        Map<Integer, Boolean> visitedNodes = new HashMap<>();
        List<Integer> connectedParts = new ArrayList();
        for (int node : graph.keySet()) {
            boolean nodeWasVisited = visitedNodes.getOrDefault(node, false);
            if (!nodeWasVisited) {
                int connectedNodes = bfs(node, graph, visitedNodes);
                if (connectedNodes > 0) {
                    connectedParts.add(connectedNodes);
                }
            }
        }
        return connectedParts;
    }

    /**
     * Associate a neighbour to a node in the given graph.
     *
     * @param graph A representation of a graph.
     * @param node A node present in the graph.
     * @param neighbour A neighbour to be associated to the given node in the graph.
     */
    private void addNeighbour(Map<Integer, List<Integer>> graph, int node, int neighbour) {
        List<Integer> neighbours = graph.getOrDefault(node, new ArrayList());
        neighbours.add(neighbour);
        graph.put(node, neighbours);
    }

    /**
     * Add every node to the graph, even if a node is not connected to any other node.
     * This method do not associate nodes with its neighbours.
     *
     * @param numberOfNodes The number of nodes in the graph.
     * @return A graph with all the nodes but no neighbours.
     */
    private Map<Integer, List<Integer>> initializeGraphNoNeighbours(int numberOfNodes) {
        Map<Integer, List<Integer>> graph = new LinkedHashMap<>(numberOfNodes);
        for (int i = 0; i < numberOfNodes; i++) {
            graph.put(i + 1, new ArrayList());
        }
        return graph;
    }

    /**
     * Build the graph from stdin.
     *
     * @param reader BufferedReader object to read data from stdin.
     * @param numberOfNodes Number of nodes in the graph.
     * @param numberOfEdges Number of edges in the graph.
     * @return A representation of a graph.
     * @throws IOException In case of reading from stdin fails.
     */
    private Map<Integer, List<Integer>> getGraph(BufferedReader reader, int numberOfNodes, int numberOfEdges) throws IOException {
        Map<Integer, List<Integer>> graph = initializeGraphNoNeighbours(numberOfNodes);
        for (int j = 0; j < numberOfEdges; j++) {
            String[] split = reader.readLine().split(SPACE);
            int[] nodes = Arrays.stream(split).mapToInt(Integer::parseInt).toArray();
            int node = nodes[0];
            int neighbour = nodes[1];
            addNeighbour(graph, node, neighbour); // One way.
            addNeighbour(graph, neighbour, node); // Backwards.
        }
        return graph;
    }

    /**
     * Return the minimum cost between building libraries or roads.
     *
     * @param libraryCost The cost of building a library.
     * @param roadCost The cost of building a road.
     * @param nodes The number of connected nodes.
     * @return Minimum cost between building libraries or roads.
     */
    private static long getMinCost(int libraryCost, int roadCost, int nodes) {
        if (nodes == 1) {
            return libraryCost;
        }
        int roads = nodes - 1;
        int librariesCost = libraryCost * nodes;
        int roadsCost = roadCost * roads + libraryCost;
        return Math.min(librariesCost, roadsCost);
    }

    public static void main(String[] args) throws IOException {
        RoadsAndLibraries main = new RoadsAndLibraries();
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
            int testCases = Integer.parseInt(reader.readLine());
            for (int i = 0; i < testCases; i++) {
                String[] elements = reader.readLine().split(SPACE);
                assert elements.length == 4;
                int[] input = Arrays.stream(elements).mapToInt(Integer::parseInt).toArray();
                int numberOfNodes = input[0];
                int numberOfEdges = input[1];
                int libraryCost = input[2];
                int roadCost = input[3];
                Map<Integer, List<Integer>> graph = main.getGraph(reader, numberOfNodes, numberOfEdges);
                List<Integer> connectedSubGraphNodes = main.getConnectedSubGraphNodes(graph);
                long minCost = 0;
                for (Integer nodes : connectedSubGraphNodes) {
                    minCost += getMinCost(libraryCost, roadCost, nodes);
                }
                System.out.println(minCost);
            }
        }
    }

}
