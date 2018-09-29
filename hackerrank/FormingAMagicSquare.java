import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Class containing the logic to solve the problem "Forming a Magic Square"
 * from HackerRank.
 *
 * See: https://www.hackerrank.com/challenges/magic-square-forming/problem
 */
public class FormingAMagicSquare {

    /**
     * Number of rows we need to read from stdin.
     */
    private static final int ROWS = 3;

    /**
     * Constant to define a single space.
     */
    private static final String SPACE = " ";

    /**
     * Array that holds all the possible 3 x 3 valid magic squares.
     * Every nine elements represents a valid magic square.
     */
    private static final int[] MAGIC_SQUARES_LINEAR = new int[]{
            8,1,6,3,5,7,4,9,2,
            6,1,8,7,5,3,2,9,4,
            4,9,2,3,5,7,8,1,6,
            2,9,4,7,5,3,6,1,8,
            8,3,4,1,5,9,6,7,2,
            4,3,8,9,5,1,2,7,6,
            6,7,2,1,5,9,8,3,4,
            2,7,6,9,5,1,4,3,8
    };

    /**
     * The number of elements in one 3 x 3 magic square = 9.
     */
    private static final int ELEMENTS_IN_MAGIC_SQUARE = 9;

    /**
     * The total possible 3 x 3 magic squares = 8.
     */
    private static final int NUMBER_OF_MAGIC_SQUARES =
            MAGIC_SQUARES_LINEAR.length / ELEMENTS_IN_MAGIC_SQUARE;

    /**
     * Main method, entry point of the program.
     *
     * The complexity of the proposed solution is: O(n)
     *
     * @param args Empty array.
     * @throws IOException Exception thrown when an IO problem happens.
     */
    public static void main(String[] args) throws IOException {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
            int[] flatMatrix = readNumbers(ROWS, reader);
            int minCost = convertToMagicSquareAtMinCost(flatMatrix);
            System.out.println(minCost);
        }
    }

    /**
     * Function used to read the expected input from stdin and return an array
     * of Ints with the numbers read.
     *
     * It reads the number of lines from the terminal indicated by the parameter
     * 'lines'. Then returns an array containing all the elements from stdin.
     *
     * Algorithm complexity: O(n)
     *
     * @param lines The number of lines we should read from stdin.
     * @param reader The BufferedReader object that will read from stdin.
     * @return A one dimensional array containing all the numbers read from stdin.
     * @throws IOException Exception thrown when an IO problem happens.
     */
    private static int[] readNumbers (int lines, BufferedReader reader) throws IOException {
        String line = "";
        for (int i = 0; i < lines; i++) {
            line += " " + reader.readLine();
        }

        String[] stringNumbers = line.trim().split(SPACE);

        int[] numbers = new int[lines*lines];
        for (int i = 0; i < stringNumbers.length; i++) {
            String stringNumber = stringNumbers[i];
            int number = Integer.parseInt(stringNumber);
            numbers[i] = number;
        }
        return numbers;
    }

    /**
     * Returns the minimum cost to convert the given paramater into a valid
     * 3 x 3 magic square.
     *
     * @param flatMatrix an array representing a 3 x 3 matrix.
     * @return minimum cost to convert the given paramater into a valid
     * 3 x 3 magic square.
     */
    private static int convertToMagicSquareAtMinCost(int[] flatMatrix) {
        List<Integer> costs = getMinCostLinear(MAGIC_SQUARES_LINEAR, flatMatrix);
        return Collections.min(costs);
    }

    /**
     * Returns a list of costs. Each element in the list represents the cost to
     * covert the given 'flatMatrix' into a valid 3 x 3 magic square.
     *
     * Algorithm complexity: O(n)
     *
     * @param magicSquares An array containing all valid 3 x 3 magic squares.
     * @param flatMatrix An array representing a 3 x 3 matrix.
     * @return A list of costs.
     */
    private static List<Integer> getMinCostLinear(int[] magicSquares, int[] flatMatrix) {
        List<Integer> costs = new ArrayList<>();
        int cost = 0;
        assert magicSquares.length == flatMatrix.length * NUMBER_OF_MAGIC_SQUARES;
        for (int i = 0; i < magicSquares.length; i++) {
            int flatMatrixIndex = i % ELEMENTS_IN_MAGIC_SQUARE;
            if (i > 0 && flatMatrixIndex == 0) {
                costs.add(cost);
                cost = 0;
            }
            int elementOne = magicSquares[i];
            int elementTwo = flatMatrix[flatMatrixIndex];
            cost += Math.abs(elementOne - elementTwo);
        }
        costs.add(cost);
        return costs;
    }
}