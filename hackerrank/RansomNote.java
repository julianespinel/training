import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;

// https://www.hackerrank.com/challenges/ctci-ransom-note/problem
public class RansomNote {

    private static final String NO = "No";
    private static final String YES = "Yes";
    private static final String SPACE = " ";

    private static String[] readWords(BufferedReader reader) throws IOException {
        return reader.readLine().split(SPACE);
    }

    private static Map<String, Integer> getFrequencies(String[] words) {
        Map<String, Integer> frequencies = new HashMap<>();
        for (String word : words) {
            int frequency = frequencies.getOrDefault(word, 0);
            frequencies.put(word, frequency + 1);
        }
        return frequencies;
    }

    public static void main(String[] args) throws IOException {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
            reader.readLine(); // Skip first line.
            String[] magazineWords = readWords(reader);
            String[] noteWords = readWords(reader);
            if (noteWords.length > magazineWords.length) {
                System.out.println(NO);
                return;
            }
            Map<String, Integer> magazineFrequencies = getFrequencies(magazineWords);
            Map<String, Integer> noteFrequencies = getFrequencies(noteWords);
            for (String noteWord : noteFrequencies.keySet()) {
                int noteFrequency = noteFrequencies.get(noteWord);
                int magazineFrequency = magazineFrequencies.getOrDefault(noteWord, 0);
                if (noteFrequency > magazineFrequency) {
                    System.out.println(NO);
                    return;
                }
            }
            System.out.println(YES);
        }
    }
}
