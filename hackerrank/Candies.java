import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

import java.util.Collection;
import java.util.Stack;

// https://www.hackerrank.com/challenges/candies/problem
public class Candies {

  private static final int SENTINEL = -1;

  //------------------------------------------------------------
  // IO functions
  // -----------------------------------------------------------

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

  //------------------------------------------------------------
  // Functions to calculate minimum candies to buy
  // -----------------------------------------------------------

  private static int incrementAndAddCandies(int candies, Stack<Integer> stack) {
    candies++;
    stack.push(candies);
    return candies;
  }

  private static int getLastElement(Stack<Integer> stack) {
    return (stack.isEmpty()) ? 0 : stack.peek();
  }

  private static int getNextScore(int index, int[] scores) {
    int nextIndex = index + 1;
    return (nextIndex < scores.length) ? scores[nextIndex] : SENTINEL;
  }

  private static int calculateCandiesForAscendingSequence(int index, int[] scores, Stack<Integer> stack) {
    int candies = 0;
    while (index < scores.length) {
      int currentScore = scores[index];
      int nextScore = getNextScore(index, scores);
      boolean isSequenceBroken = currentScore >= nextScore;
      if (isSequenceBroken) {
        incrementAndAddCandies(candies, stack);
        return index; // Sequence finishes
      }
      boolean isSequenceContinued = nextScore == SENTINEL || currentScore < nextScore;
      if (isSequenceContinued) {
        candies = incrementAndAddCandies(candies, stack);
      }
      index++;
    }
    return index;
  }

  private static int calculateCandiesForDescendingSequence(int index, int[] scores, Stack<Integer> stack) {
    int candies = 0;
    while (index < scores.length) {
      int currentScore = scores[index];
      int nextScore = getNextScore(index, scores);
      boolean isSequenceBroken = currentScore <= nextScore;
      if (isSequenceBroken) {
        incrementAndAddCandies(candies, stack);
        return index; // Sequence finishes
      }
      boolean isSequenceContinued = nextScore == SENTINEL || currentScore > nextScore;
      if (isSequenceContinued) {
        candies = incrementAndAddCandies(candies, stack);
      }
      index++;
    }
    return index;
  }

  private static void calculateCandiesForDuplicatedScores(Stack<Integer> stack) {
    int candies = getLastElement(stack);
    candies = (candies != 1) ? 1 : candies + 1;
    stack.push(candies);
  }

  private static void calculateLastCandy(int i, int[] scores, Stack<Integer> stack) {
    int currentScore = scores[i];
    int previousScore = scores[i - 1];
    if (previousScore == currentScore) {
      calculateCandiesForDuplicatedScores(stack);
      return;
    }
    incrementAndAddCandies(1, stack);
  }

  private static Collection<Integer> getMinimumCandiesToBuy(int[] scores) {
    Stack<Integer> stack = new Stack<>();
    if (scores.length == 1) {
      stack.push(1);
      return stack;
    }
    // if scores length is greater than 1
    for (int i = 0; i < scores.length; i++) {
      int currentScore = scores[i];
      int nextScore = getNextScore(i, scores);
      if (nextScore == SENTINEL) {
        calculateLastCandy(i, scores, stack);
        return stack;
      } else if (currentScore < nextScore) {
        i = calculateCandiesForAscendingSequence(i, scores, stack);
      } else if (currentScore == nextScore ) {
        calculateCandiesForDuplicatedScores(stack);
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

  //------------------------------------------------------------
  // Main function
  // -----------------------------------------------------------

  public static void main(String[] args) throws IOException {
    try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
      int size = readSize(reader);
      int[] scores = readArray(reader, size);
      Collection<Integer> minimumCandies = getMinimumCandiesToBuy(scores);
      System.out.println(sum(minimumCandies));
    }
  }
}
