import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

import java.util.Collection;
import java.util.Stack;

// https://www.hackerrank.com/challenges/candies/problem
public class Candies {

  private static final int SENTINEL = -1;

  private static int readSize(BufferedReader reader) throws IOException {
    String line = reader.readLine();
    return Integer.parseInt(line);
  }

  private static int[] readArray(BufferedReader reader, int size) throws IOException {
    int[] array = new int[size];
    for (int i = 0; i < size; i++) {
      String line = reader.readLine();
      array[i] = Integer.parseInt(line);
    }
    return array;
  }

  private static int incrementAndAddCandies(int candies, Stack<Integer> stack) {
    candies++;
    stack.push(candies);
    return candies;
  }

  private static int getLastElement(Stack<Integer> stack) {
    return (stack.isEmpty()) ? 0 : stack.peek();
  }

  private static void calculateCandiesForLastElementInAscendingSequence(int index, int[] scores, Stack<Integer> stack, int candies) {
    int currentScore = scores[index];
    int previousScore = scores[index - 1];
    if (previousScore < currentScore) {
      incrementAndAddCandies(candies, stack);
    } else if (previousScore == currentScore) {
      candies = getLastElement(stack);
      candies = (candies != 1) ? 1 : candies + 1;
      stack.push(candies);
    } else if (previousScore > currentScore) {
      throw new IllegalStateException("Should never reach this line: calculateCandiesForAscendingSequence");
    }
  }

  private static int calculateCandiesForAscendingSequence(int index, int[] scores, Stack<Integer> stack) {
    int candies = 0;
    while (index < scores.length - 1) {
      int currentScore = scores[index];
      int nextScore = scores[index + 1];
      if (currentScore < nextScore) {
        candies = incrementAndAddCandies(candies, stack);
      }
      if (currentScore == nextScore) {
        candies = getLastElement(stack);
        candies = (candies != 1) ? 1 : candies + 1;
        stack.push(candies);
      }
      if (currentScore > nextScore) {
        // Sequence finishes
        incrementAndAddCandies(candies, stack);
        return index;
      }
      index++;
    }
    calculateCandiesForLastElementInAscendingSequence(index, scores, stack, candies);
    return index;
  }

  private static void calculateCandiesForLastElementInDescendingSequence(int index, int[] scores, Stack<Integer> stack, int candies) {
    int currentScore = scores[index];
    int previousScore = scores[index - 1];
    if (previousScore < currentScore) {
      throw new IllegalStateException("Should never reach this line: calculateCandiesForAscendingSequence");
    } else if (previousScore == currentScore ) {
      candies = getLastElement(stack);
      candies = (candies != 1) ? 1 : candies + 1;
      stack.push(candies);
    } else if (previousScore > currentScore ) {
      incrementAndAddCandies(candies, stack);
    }
  }

  private static int calculateCandiesForDescendingSequence(int index, int[] scores, Stack<Integer> stack) {
    int candies = 0;
    while (index < scores.length - 1) {
      int currentScore = scores[index];
      int nextScore = scores[index + 1];
      if (currentScore < nextScore) {
        // Sequence finishes
        incrementAndAddCandies(candies, stack);
        return index;
      }
      if (currentScore == nextScore) {
        candies = getLastElement(stack);
        candies = (candies != 1) ? 1 : candies + 1;
        stack.push(candies);
      }
      if (currentScore > nextScore) {
        candies = incrementAndAddCandies(candies, stack);
      }
      index++;
    }
    // Check candies for the last student.
    calculateCandiesForLastElementInDescendingSequence(index, scores, stack, candies);
    return index;
  }

  private static int calculateCandiesForLastElement(int i, int[] scores, Stack<Integer> stack) {
    if (scores.length == 1) {
      stack.push(1);
      return i;
    }
    int currentScore = scores[i];
    int previousScore = scores[i - 1];
    if (previousScore == currentScore) {
      int candies = getLastElement(stack);
      candies = (candies != 1) ? 1 : candies + 1;
      stack.push(candies);
    }
    return i;
  }

  private static Collection<Integer> getMinimumCandiesToBuy(int[] scores) {
    Stack<Integer> stack = new Stack<>();
    for (int i = 0; i < scores.length; i++) {
      boolean isLastElement = i == scores.length - 1;
      if (isLastElement) {
        i = calculateCandiesForLastElement(i, scores, stack);
      }
      int currentScore = scores[i];
      int nextScore = scores[i + 1];
      if (currentScore < nextScore) {
        i = calculateCandiesForAscendingSequence(i, scores, stack);
      } else if (currentScore == nextScore) {
        int candies = getLastElement(stack);
        candies = (candies != 1) ? 1 : candies + 1;
        stack.push(candies);
      } else if (currentScore > nextScore) {
        i = calculateCandiesForDescendingSequence(i, scores, stack);
      }
    }
    return stack;
  }

  private static long sum(Collection<Integer> list) {
    long accumulator = 0;
    for (int element : list) {
      accumulator += element;
    }
    return accumulator;
  }

  public static void main(String[] args) throws IOException {
    try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
      int size = readSize(reader);
      int[] scores = readArray(reader, size);
      Collection<Integer> minimumCandies = getMinimumCandiesToBuy(scores);
      System.out.println(sum(minimumCandies));
    }
  }
}
