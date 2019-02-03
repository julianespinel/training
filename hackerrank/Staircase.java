import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

// https://www.hackerrank.com/challenges/staircase/problem
public class Staircase {

  private static final String SPACE = " ";
  private static final String LINE_FEED = "\n";

  private static int readInput() throws IOException {
    try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
      String line = reader.readLine();
      return Integer.parseInt(line);
    }
  }

  /**
   * Given a number representing the base of the staircase, return a matrix
   * representing the staircase to be printed in stdout.
   *
   * The complexity of this algorithm is: O(n log n) < complexity < O(n^2)
   *
   * Example: n = 256
   * O(n log n) < complexity < O(n^2)
   * = 256 * 8 < 16_512 < 256 * 256
   * = 2_048 < 16_512 < 65_536
   *
   * @param staircaseBase number representing the base of the staircase.
   * @return matrix representing the staircase to be printed.
   */
  private static String[][] getStaircase(int staircaseBase) {
    String[][] matrix = new String[staircaseBase][staircaseBase];
    int rowCount = 0;
    for (int row = staircaseBase; rowCount < row; row--) {
      int correctedRow = row - 1; // Used to access positions in the matrix.
      for (int column = rowCount; column < row; column++) {
        matrix[correctedRow][column] = "#";
        matrix[column][correctedRow] = "#";
      }
      rowCount++;
    }
    return matrix;
  }

  private static void print(String[][] matrix) {
    StringBuilder builder = new StringBuilder();
    for (String[] row : matrix) {
      for (String cell: row) {
        cell = cell != null ? cell : SPACE;
        builder.append(cell);
      }
      builder.append(LINE_FEED);
    }
    System.out.println(builder.toString());
  }

  public static void main(String[] args) throws IOException {
    int staircaseBase = readInput();
    String[][] staircase = getStaircase(staircaseBase);
    print(staircase);
  }
}
