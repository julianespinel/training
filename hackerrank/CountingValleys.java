import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

// https://www.hackerrank.com/challenges/counting-valleys/problem
public class CountingValleys {

    private static final char D = 'D';
    private static final char U = 'U';

    private static String readPath() throws IOException {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
            reader.readLine(); // Ignore this line.
            return reader.readLine();
        }
    }

    private static int countValleys(String path) {
        int seaLevel = 0;
        int valleys = 0;
        for (char c : path.toCharArray()) {
            if (seaLevel == 0 && c == D) {
                valleys += 1;
            }
            seaLevel += (c == U) ? 1: -1;
        }
        return valleys;
    }

    public static void main(String[] args) throws IOException {
        String path = readPath();
        int valleys = countValleys(path);
        System.out.println(valleys);
    }
}
