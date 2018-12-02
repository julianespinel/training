import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

// https://www.hackerrank.com/challenges/max-array-sum/problem
public class MaxArraySum {

  private static final String SPACE = " ";
  private static final long SENTINEL = -1;

  private static int[] readNumbers() throws IOException {
    try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
      reader.readLine(); // Skip array length line.
      String[] split = reader.readLine().split(SPACE);
      return Arrays.stream(split).mapToInt(Integer::parseInt).toArray();
    }
  }

  /**
   * Because if the restriction of the problem (find the subset of non-adjacent
   * elements with the maximum sum), this method was designed to skip the next
   * element relative to the currentIndex and return the indices of the next
   * two positive elements.
   *
   * @param currentIndex Index of the numbers array we are currently evaluating.
   * @param numbers An array of numbers.
   * @return a List with at most two elements.
   */
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

  /**
   * Adds an element to a map only if the element is greater than the cached
   * element corresponding to the given key.
   *
   * @param cache A map we are using to store previously computed values.
   * @param key A possible key of the cache.
   * @param value A value to be inserted in the cache.
   */
  private static void addToCache(Map<Integer, Long> cache, int key, long value) {
    long cachedValue = cache.getOrDefault(key, SENTINEL);
    if (value > cachedValue) {
      cache.put(key, value);
    }
  }

  private static long getMaxValue(Map<Integer, Long> cache) {
    long maxValue = 0;
    for (long value : cache.values()) {
      maxValue = (value > maxValue) ? value : maxValue;
    }
    return maxValue;
  }

  private static long getMaxSum(int[] numbers) {
    Map<Integer, Long> cache = new HashMap<>();
    for (int i = 0; i < numbers.length; i++) {
      int currentNumber = numbers[i];
      if (currentNumber <= 0) {
        continue;
      }
      List<Integer> nextIndices = findNextIndices(i, numbers);
      long cachedResult = cache.getOrDefault(i, SENTINEL);
      if (cachedResult == SENTINEL) {
        for (int nextIndex : nextIndices) {
          long value = currentNumber + numbers[nextIndex];
          addToCache(cache, nextIndex, value);
        }
      } else {
        for (int nextIndex : nextIndices) {
          long value = cachedResult + numbers[nextIndex];
          addToCache(cache, nextIndex, value);
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
