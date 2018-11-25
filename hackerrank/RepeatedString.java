import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

// https://www.hackerrank.com/challenges/repeated-string/problem
public class RepeatedString {

    private static int getRepetitions(String string, char someChar) {
        int repetitions = 0;
        for (char c : string.toCharArray()) {
            if (c == someChar) {
                repetitions++;
            }
        }
        return repetitions;
    }

    private static int getRemainderRepetitions(String string, long limit, char targetChar) {
        int length = string.length();
        int remainder = Math.toIntExact(limit % length);
        int remainderRepetitions = 0;
        if (remainder > 0) {
            String substring = string.substring(0, remainder);
            remainderRepetitions = getRepetitions(substring, targetChar);
        }
        return remainderRepetitions;
    }

    public static void main(String[] args) throws IOException {
        // Read input
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        String string = reader.readLine();
        long limit = Long.parseLong(reader.readLine());
        // Calculate result
        char targetChar = 'a';
        long repetitions = getRepetitions(string, targetChar);
        long times = (limit / string.length());
        int remainderRepetitions = getRemainderRepetitions(string, limit, targetChar);
        long answer = (repetitions * times) + remainderRepetitions;
        // Print result
        System.out.println(answer);
    }
}
