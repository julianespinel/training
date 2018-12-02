import java.util.*;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

public class MaxArraySum {

  private static final String SPACE = " ";

  private static int[] readNumbers() throws IOException {
    try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
      reader.readLine(); // Skip array length line.
      String[] split = reader.readLine().split(SPACE);
      return Arrays.stream(split).mapToInt(Integer::parseInt).toArray();
    }
  }

  private static List<Integer> findNextIndices(int currentIndex, int[] numbers) {
    List<Integer> nextIndices = new ArrayList<>();
    for (int i = currentIndex + 2; i < numbers.length; i++) {
      if (numbers[i] > 0) {
        nextIndices.add(i);
        int next = i + 1;
        if (next < numbers.length && numbers[next] > 0) {
          nextIndices.add(next);
        }
        return nextIndices;
      }
    }
    return nextIndices;
  }

  private static void addToCache(Map<Integer, List<Long>> cache,
      int key, long value) {
    List<Long> cachedResults = cache.getOrDefault(key, new ArrayList<>());
    if (!cachedResults.contains(value)) {
      cachedResults.add(value);
      cache.put(key, cachedResults);
    }
  }

  private static long getMaxValue(Map<Integer, List<Long>> cache) {
    long maxValue = 0;
    for (List<Long> cachedValues : cache.values()) {
      for (long value : cachedValues) {
        maxValue = (value > maxValue) ? value : maxValue;
      }
    }
    return maxValue;
  }

  private static long getMaxSum(int[] numbers) {
    Map<Integer, List<Long>> cache = new HashMap<>();
    for (int i = 0; i < numbers.length; i++) {
      int currentNumber = numbers[i];
      if (currentNumber <= 0) {
        continue;
      }
      List<Integer> nextIndices = findNextIndices(i, numbers);
      List<Long> cachedResults = cache.getOrDefault(i, null);
      if (cachedResults == null) {
        for (int nextIndex : nextIndices) {
          long value = currentNumber + numbers[nextIndex];
          addToCache(cache, nextIndex, value);
        }
      } else {
        for (int nextIndex : nextIndices) {
          for (long cachedResult : cachedResults) {
            long value = cachedResult + numbers[nextIndex];
            addToCache(cache, nextIndex, value);
          }
        }
      }
    }
    return getMaxValue(cache);
  }

  public static void main(String[] args) throws IOException {
    int[] numbers = readNumbers();
    long maxSum = getMaxSum(numbers);
    System.out.println(maxSum);
  }
}
