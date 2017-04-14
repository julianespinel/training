import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class SparseArrays {

    private static int readNumber(BufferedReader reader) throws IOException {
        String stringNumber = reader.readLine();
        return Integer.parseInt(stringNumber);
    }

    private static Map<String, Integer> readStrings(BufferedReader reader, int numberOfStrings) throws IOException {
        Map<String, Integer> map = new HashMap<>(numberOfStrings);
        for (int i = 0; i < numberOfStrings; i++) {
            String string = reader.readLine();
            Integer occurrences = map.getOrDefault(string, 0);
            occurrences++;
            map.put(string, occurrences);
        }
        return map;
    }

    private static List<String> readQueries(BufferedReader reader, int numberOfQueries) throws IOException {
        List<String> queries = new ArrayList<>(numberOfQueries);
        for (int i = 0; i < numberOfQueries; i++) {
            String query = reader.readLine();
            queries.add(query);
        }
        return queries;
    }

    private static void printStringOccurrences(Map<String, Integer> stringsOccurrences, List<String> queries) {
        for (String query : queries) {
            Integer occurrences = stringsOccurrences.getOrDefault(query, 0);
            System.out.println(occurrences);
        }
    }

    public static void main(String[] args) {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
            int numberOfStrings = readNumber(reader);
            Map<String, Integer> stringsOccurrences = readStrings(reader, numberOfStrings);
            int numberOfQueries = readNumber(reader);
            List<String> queries = readQueries(reader, numberOfQueries);
            printStringOccurrences(stringsOccurrences, queries);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
