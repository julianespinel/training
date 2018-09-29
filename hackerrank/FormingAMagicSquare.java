import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;

public class FormingAMagicSquare {

    private static final int ROWS = 3;
    private static final String SPACE = " ";

    private static final int[][] MAGIC_SQUARES = new int[][]{
            {8,1,6,3,5,7,4,9,2},
            {6,1,8,7,5,3,2,9,4},
            {4,9,2,3,5,7,8,1,6},
            {2,9,4,7,5,3,6,1,8},
            {8,3,4,1,5,9,6,7,2},
            {4,3,8,9,5,1,2,7,6},
            {6,7,2,1,5,9,8,3,4},
            {2,7,6,9,5,1,4,3,8}
    };

    private static final int MAGIC_SQUARE_SUM = 45;

    private static int[] readNumbers (int rows, BufferedReader reader) throws IOException {
        String line = "";
        for (int i = 0; i < rows; i++) {
            line += " " + reader.readLine();
        }

        String[] stringNumbers = line.trim().split(SPACE);

        int[] numbers = new int[rows*rows];
        for (int i = 0; i < stringNumbers.length; i++) {
            String stringNumber = stringNumbers[i];
            int number = Integer.parseInt(stringNumber);
            numbers[i] = number;
        }
        return numbers;
    }

    /*
     TODO: is possible to solve this problem in O(n) by making a large 1D
     array of all possible 3x3 magic squares.
    */
    private static int convertToMagicSquareAtMinCost(int[] flatMatrix) {
        int sum = Arrays.stream(flatMatrix).sum();
        return Math.abs(MAGIC_SQUARE_SUM - sum);
    }

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
}