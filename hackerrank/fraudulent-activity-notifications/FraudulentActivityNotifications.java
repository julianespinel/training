import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

import java.util.ArrayList;
import java.util.ArrayDeque;
import java.util.Collections;
import java.util.Queue;
import java.util.List;

// https://www.hackerrank.com/challenges/fraudulent-activity-notifications/problem
public class FraudulentActivityNotifications {

  private class AVLTree {
    private Node root;

    AVLTree(Node root) {
      this.root = root;
    }

    void rebalanceIfNeeded(Node node) {
      int leftHeigth = getHeight(node.getLeft());
      int rightHeigth = getHeight(node.getRight());
      if (Math.abs(leftHeigth - rightHeigth) > 1) {

      }
    }

    void addLeft(Node node, Integer number) {
      Node left = node.getLeft();
      if (left == null) {
        node.setLeft(new Node(number));
      } else {
        addElement(left, number);
      }
      rebalanceIfNeeded(node);
    }

    void addRight(Node node, Integer number) {
      Node right = node.getRight();
      if (right == null) {
        node.setRight(new Node(number));
      } else {
        addElement(right, number);
      }
    }

    void addElement(Node node, Integer number) {
      Integer value = node.getValue();
      if (value == null) {
        node.setValue(number);
      } else if (number < value) {
        addLeft(node, number);
      } else if (number >= value) {
        addRight(node, number);
      }
    }
  }

  private class Node {
    private Integer value;
    private Node left;
    private Node right;

    Node() {
    }

    Node(Integer value) {
      this.value = value;
    }

    Integer getValue() {
      return value;
    }

    void setValue(Integer value) {
      this.value = value;
    }

    Node getLeft() {
      return left;
    }

    void setLeft(Node left) {
      this.left = left;
    }

    Node getRight() {
      return right;
    }

    void setRight(Node right) {
      this.right = right;
    }
  }

  private AVLTree buildAVLTree(List<Integer> list) {
    Node node = new Node();
    for (Integer element : list) {
      node.addElement(element);
    }
    return node;
  }

  private static final String SPACE = " ";

  private List<Integer> getSpendings(BufferedReader reader) throws IOException {
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
  private List<Integer> sort(List<Integer> numbers) {
    Collections.sort(numbers);
    return numbers;
  }

  private double getMedian(List<Integer> sortedListAscending) {
    int midIndex = sortedListAscending.size() / 2; // Java rounds downwards.
    int midSpending = sortedListAscending.get(midIndex);
    if (sortedListAscending.size() % 2 == 0) {
      int midMinusOneSpending = sortedListAscending.get(midIndex - 1);
      return (midSpending + midMinusOneSpending) / 2.0;
    }
    return midSpending;
  }

  private int binarySearchIndexToInsertAt(List<Integer> sortedListAscending, int number, int insertAt) {
    if (sortedListAscending.isEmpty()) {
      return insertAt;
    }
    int size = sortedListAscending.size();
    int midIndex = size / 2;
    int midElement = sortedListAscending.get(midIndex);
    if (number == midElement) {
      return insertAt + midIndex;
    }
    if (number < midElement) {
      List<Integer> lowerList = sortedListAscending.subList(0, midIndex);
      return binarySearchIndexToInsertAt(lowerList, number, insertAt);
    } else {
      List<Integer> upperList = sortedListAscending.subList(midIndex + 1, size);
      return binarySearchIndexToInsertAt(upperList, number, insertAt + midIndex + 1);
    }
  }

  private List<Integer> addElementInOrder(List<Integer> sortedAscending, int number) {
    int insertAt = binarySearchIndexToInsertAt(sortedAscending, number, 0);
    sortedAscending.add(insertAt, number);
    return sortedAscending;
  }

  public void run() {
    try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
      String firstLine = reader.readLine();
      String[] split = firstLine.split(SPACE);
      int totalDays = Integer.parseInt(split[0]);
      int trailingDays = Integer.parseInt(split[1]);
      List<Integer> spendings = getSpendings(reader);
      Queue<Integer> queue = new ArrayDeque<>(spendings);
      if (spendings.size() != totalDays) {
        throw new IllegalStateException("Spendings queue does not have the amount of elements it should.");
      }

      int notifications = 0;

      Node spendingsInTrailingDays = buildAVLTree(spendings.subList(0, trailingDays));
      // List<Integer> spendingsInTrailingDays = new ArrayList<>(spendings.subList(0, trailingDays));
      // List<Integer> sortedSpendingsInTrailingDays = sort(spendingsInTrailingDays);

      /* Used Integer instead of int, so list.remove(object) will remove
       * the object rather than the index.
       * See: https://docs.oracle.com/javase/8/docs/api/java/util/List.html#remove-java.lang.Object-
       */
      Integer elementToRemove = 0;
      for (int day = trailingDays; day < totalDays; day++) {
        // double median = getMedian(sortedSpendingsInTrailingDays);
        double median = getMedianFromBinarySearchTree(sortedSpendingsInTrailingDays);
        int currentDaySpending = spendings.get(day);
        boolean customerShouldBeNotified = currentDaySpending >= (2.0 * median);
        if (customerShouldBeNotified) {
          notifications++;
        }
        elementToRemove = queue.poll();
        // sortedSpendingsInTrailingDays.remove(elementToRemove);
        // sortedSpendingsInTrailingDays = addElementInOrder(sortedSpendingsInTrailingDays, currentDaySpending);
        sortedSpendingsInTrailingDays.replace(elementToRemove, currentDaySpending);
        sortedSpendingsInTrailingDays.rebalance();
      }
      System.out.println(notifications);
    }
  }

  /**
   * The current time complexity of the implementation is O(n^2). Why?
   * 1. I traverse the spendings list from spendings.get(trailingDays) to the end of the list: O(n)
   * 2. In every iteration of the for loop (Step 1), I remove and insert an element from the
   *    sortedSpendingsInTrailingDays list: 2 * O(n) -> O(n).
   */
  public static void main(String[] args) throws IOException {
    FraudulentActivityNotifications program = new FraudulentActivityNotifications();
    program.run();
  }

}
