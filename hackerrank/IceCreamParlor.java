import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

// https://www.hackerrank.com/challenges/ctci-ice-cream-parlor/problem
public class IceCreamParlor {

  private static final String SPACE = " ";
  private static final int SENTINEL = -1;

  private long[] getCosts(BufferedReader reader) throws IOException {
    String[] split = reader.readLine().split(SPACE);
    return Arrays.stream(split).mapToLong(Long::parseLong).toArray();
  }

  private Map<Long, List<Integer>> getFrequencies(long[] costs) {
    Map<Long, List<Integer>> frequencies = new HashMap<>();
    for (int i = 0; i < costs.length; i++) {
      long cost = costs[i];
      List<Integer> indexList = frequencies.getOrDefault(cost, new ArrayList());
      indexList.add(i);
      frequencies.put(cost, indexList);
    }
    return frequencies;
  }

  private int getDifferentIndex(int index, List<Integer> indices) {
    for (int indexFromList : indices) {
      if (indexFromList != index) {
        return indexFromList;
      }
    }
    return SENTINEL;
  }

  private String getIdsToSpendAllBudget(long budget, long[] costs) {
    Map<Long, List<Integer>> frequencies = getFrequencies(costs);
    for (int i = 0; i < costs.length; i++) {
      long cost = costs[i];
      long difference = budget - cost;
      List<Integer> secondCost = frequencies.getOrDefault(difference, null);
      if (secondCost != null) {
        int secondIndex = secondCost.get(0);
        if (cost == difference) {
          secondIndex = getDifferentIndex(i, secondCost);
          if (secondIndex == SENTINEL) {
            continue;
          }
        }
        return (i + 1) + SPACE + (secondIndex + 1);
      }
    }
    throw new IllegalStateException("There is no answer");
  }

  public static void main(String[] args) throws IOException {
    IceCreamParlor parlor = new IceCreamParlor();
    try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
      int testCases = Integer.parseInt(reader.readLine());
      for (int i = 0; i < testCases; i++) {
        long budget = Long.parseLong(reader.readLine());
        reader.readLine(); // Skip the size of the array.
        long[] costs = parlor.getCosts(reader);
        String idsSeparatedBySpace = parlor.getIdsToSpendAllBudget(budget, costs);
        System.out.println(idsSeparatedBySpace);
      }
    }
  }
}
