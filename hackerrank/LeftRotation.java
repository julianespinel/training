import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

import java.util.List;
import java.util.ArrayList;

public class LeftRotation {

  private static final String SPACE = " ";

  private static int readRotations(BufferedReader reader) throws IOException {
    String line = reader.readLine();
    String[] split = line.split(SPACE);
    return Integer.parseInt(split[1]);
  }

  private static List<Integer> readList(BufferedReader reader) throws IOException {
    String line = reader.readLine();
    String[] split = line.split(SPACE);
    List<Integer> numbers = new ArrayList<>();
    for (String element : split) {
      numbers.add(Integer.parseInt(element));
    }
    return numbers;
  }

  private static List<Integer> leftRotate(int leftRotations, List<Integer> numbers) {
    int size = numbers.size();
    if (leftRotations == size) {
      return numbers;
    }
    List<Integer> head = numbers.subList(0, leftRotations);
    List<Integer> tail = numbers.subList(leftRotations, size);
    tail.addAll(head); // this method mutates the 'tail' list.
    if (tail.size() != size) {
      throw new IllegalStateException(
          "The output list size should be the same as the input list size."
      );
    }
    return tail;
  }

  private static void printList(List<Integer> list) {
    StringBuilder builder = new StringBuilder();
    for (Integer number : list) {
      builder.append(number).append(SPACE);
    }
    System.out.println(builder.toString().trim());
  }

  public static void main(String[] args) throws IOException {
    try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
      int leftRotations = readRotations(reader);
      List<Integer> list = readList(reader);
      List<Integer> rotatedList = leftRotate(leftRotations, list);
      printList(rotatedList);
    }
  }
}
