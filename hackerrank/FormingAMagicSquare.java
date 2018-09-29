import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class FormingAMagicSquare {

    private static final int ROWS = 3;
    private static final String SPACE = " ";

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

    private static final int ELEMENTS_IN_MAGIC_SQUARE = 9;
    private static final int NUMBER_OF_MAGIC_SQUARES =
            MAGIC_SQUARES_LINEAR.length / ELEMENTS_IN_MAGIC_SQUARE;

    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        try {
            int[] flatMatrix = readNumbers(ROWS, reader);
            int minCost = convertToMagicSquareAtMinCost(flatMatrix);
            System.out.println(minCost);
        } catch (IOException e) {
            reader.close();
        }
    }

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

    private static int convertToMagicSquareAtMinCost(int[] flatMatrix) {
        List<Integer> costs = getMinCostLinear(MAGIC_SQUARES_LINEAR, flatMatrix);
        return Collections.min(costs);
    }

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
