import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;

// https://www.hackerrank.com/challenges/mark-and-toys/problem
public class MarkAndToys {

  private static final String SPACE = " ";

  private static long getBudget(BufferedReader reader) throws IOException {
    String[] split = reader.readLine().split(SPACE);
    return Long.parseLong(split[1]);
  }

  private static long[] getPrices(BufferedReader reader) throws IOException {
    String[] split = reader.readLine().split(SPACE);
    return Arrays.stream(split).mapToLong(Long::parseLong).toArray();
  }

  private static int getMaxToys(long[] sortedPrices, long budget) {
    int toys = 0;
    long toPay = 0;
    for (long price : sortedPrices) {
      if (toPay + price > budget) {
        break;
      }
      toPay += price;
      toys += 1;
    }
    return toys;
  }

  public static void main(String[] args) throws IOException {
    try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
      long budget  = getBudget(reader);
      long[] prices = getPrices(reader);
      Arrays.sort(prices);
      int maxToys = getMaxToys(prices, budget);
      System.out.println(maxToys);
    }
  }
}
