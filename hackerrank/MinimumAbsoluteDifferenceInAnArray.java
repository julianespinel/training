import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;

// https://www.hackerrank.com/challenges/minimum-absolute-difference-in-an-array/problem
public class MinimumAbsoluteDifferenceInAnArray {

  private static final String SPACE = " ";

  private static long[] readArray() throws IOException {
    try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
      reader.readLine(); // Skip this line.
      String[] split = reader.readLine().split(SPACE);
      return Arrays.stream(split).mapToLong(Long::parseLong).toArray();
    }
  }

  private static long getMinAbsoluteDifference(long[] numbers) {
    Arrays.sort(numbers);
    long minDifference = Long.MAX_VALUE;
    for (int i = 0; i < numbers.length; i++) {
      int nextIndex = i + 1;
      if (nextIndex < numbers.length) {
        long currentNumber = numbers[i];
        long nextNumber = numbers[nextIndex];
        long difference = Math.abs(currentNumber - nextNumber);
        minDifference = (difference < minDifference) ? difference: minDifference;
      }
    }
    return minDifference;
  }

  public static void main(String[] args) throws IOException {
    long[] array = readArray();
    long minDifference = getMinAbsoluteDifference(array);
    System.out.println(minDifference);
  }
}
