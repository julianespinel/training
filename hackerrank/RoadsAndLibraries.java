import java.io.*;
import java.util.*;

// https://www.hackerrank.com/challenges/torque-and-development/problem
public class RoadsAndLibraries {

    private static final String SPACE = " ";

    private class ConnectedPart {

        private int nodes;
        private int roads;

        ConnectedPart(int nodes, int roads) {
            this.nodes = nodes;
            this.roads = roads;
        }

        long getMinCost(int libraryCost, int roadCost) {
            if (nodes == 1) {
                return libraryCost;
            }
            int librariesCost = libraryCost * nodes;
            int roadsCost = roadCost * roads + libraryCost;
            return Math.min(librariesCost, roadsCost);
        }
    }

    private ConnectedPart bfs(int node, Map<Integer, List<Integer>> graph, Map<Integer, Boolean> visitedNodes) {
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
        if (nodes > 0) {
            int roads = nodes - 1;
            return new ConnectedPart(nodes, roads);
        }
        // If all nodes have been visited.
        return null;
    }

    private List<ConnectedPart> getConnectedParts(Map<Integer, List<Integer>> graph) {
        Map<Integer, Boolean> visitedNodes = new HashMap<>();
        List<ConnectedPart> connectedParts = new ArrayList();
        for (int node : graph.keySet()) {
            boolean nodeWasVisited = visitedNodes.getOrDefault(node, false);
            if (!nodeWasVisited) {
                ConnectedPart part = bfs(node, graph, visitedNodes);
                if (part != null) {
                    connectedParts.add(part);
                }
            }
        }
        return connectedParts;
    }

    private void addToGraph(Map<Integer, List<Integer>> graph, int node, int neighbour) {
        List<Integer> neighbours = graph.getOrDefault(node, new ArrayList());
        neighbours.add(neighbour);
        graph.put(node, neighbours);
    }

    private Map<Integer, List<Integer>> initializeGraph(int numberOfNodes) {
        Map<Integer, List<Integer>> graph = new LinkedHashMap<>(numberOfNodes);
        for (int i = 0; i < numberOfNodes; i++) {
            graph.put(i + 1, new ArrayList());
        }
        return graph;
    }

    private Map<Integer, List<Integer>> getGraph(BufferedReader reader, int numberOfNodes, int numberOfEdges) throws IOException {
        Map<Integer, List<Integer>> graph = initializeGraph(numberOfNodes);
        for (int j = 0; j < numberOfEdges; j++) {
            String[] split = reader.readLine().split(SPACE);
            int[] nodes = Arrays.stream(split).mapToInt(Integer::parseInt).toArray();
            int node = nodes[0];
            int neighbour = nodes[1];
            addToGraph(graph, node, neighbour); // One way.
            addToGraph(graph, neighbour, node); // Backwards.
        }
        return graph;
    }

    public static void main(String[] args) throws IOException {
        RoadsAndLibraries main = new RoadsAndLibraries();
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
//        try (BufferedReader reader = new BufferedReader(new FileReader("input03.txt"))) {
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
                List<ConnectedPart> connectedParts = main.getConnectedParts(graph);
                long minCost = 0;
                for (ConnectedPart part : connectedParts) {
                    minCost += part.getMinCost(libraryCost, roadCost);
                }
                System.out.println(minCost);
            }
        }
    }

}
