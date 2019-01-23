import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

// https://www.hackerrank.com/challenges/fraudulent-activity-notifications/problem
public class FraudulentActivityNotifications {

  private static final String SPACE = " ";

  private static List<Integer> getSpendings(BufferedReader reader) throws IOException {
    String line = reader.readLine();
    String[] split = line.split(SPACE);

    List<Integer> spendings = new ArrayList<>(split.length);
    for (String element : split) {
      spendings.add(Integer.parseInt(element));
    }
    return spendings;
  }

  /**
   * Method created to implement a sorting algorithm.
   *
   * I'm using Collections.sort just to verify my hypothesis that this naive
   * implementation will generate time outs in the test cases.
   */
  private static List<Integer> sort(List<Integer> numbers) {
    Collections.sort(numbers);
    return numbers;
  }

  private static double getMedianInTrailingDays(int day, List<Integer> spendings, int trailingDays) {
    List<Integer> spendingsInTrailingDays = spendings.subList(day - trailingDays, day);
    List<Integer> sortedSpendingsInTrailingDays = sort(spendingsInTrailingDays);
    int midIndex = sortedSpendingsInTrailingDays.size() / 2; // Java rounds downwards.
    int midSpending = sortedSpendingsInTrailingDays.get(midIndex);
    if (sortedSpendingsInTrailingDays.size() % 2 == 0) {
      int midPlusOneSpending = sortedSpendingsInTrailingDays.get(midIndex + 1);
      return (midSpending + midPlusOneSpending) / 2;
    }
    return midSpending;
  }

  private static boolean customerWasNotifiedInDay(int day, List<Integer> spendings, int trailingDays) {
    double median = getMedianInTrailingDays(day, spendings, trailingDays);
    int currentDaySpending = spendings.get(day);
    return currentDaySpending >= (2 * median);
  }

  /**
   * The current time complexity of the implementation is O(n * n log n). Why?
   * 1. I traverse the spendings list from spendings.get(trailingDays) to the end of the list: O(n)
   * 2. In every iteration of the for loop (Step 1), I sort a subList of size equal to trailingDays: O(n log n)
   */
  public static void main(String[] args) throws IOException {
    try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
      String firstLine = reader.readLine();
      String[] split = firstLine.split(SPACE);
      int totalDays = Integer.parseInt(split[0]);
      int trailingDays = Integer.parseInt(split[1]);
      List<Integer> spendings = getSpendings(reader);
      if (spendings.size() != totalDays) {
        throw new IllegalStateException("Spendings array has more elements than it should.");
      }
      int notifications = 0;
      for (int day = trailingDays; day < totalDays; day++) {
        if (customerWasNotifiedInDay(day, spendings, trailingDays)) {
          notifications++;
        }
      }
      System.out.println(notifications);
    }
  }
}
