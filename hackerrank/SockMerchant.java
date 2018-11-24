import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

// https://www.hackerrank.com/challenges/sock-merchant/problem
public class SockMerchant {

    private static final String SPACE = " ";

    private static int[] getSocksColors() throws IOException {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
            Integer.parseInt(reader.readLine()); // Ignore this input line.
            String[] split = reader.readLine().split(SPACE);
            return Arrays.stream(split).mapToInt(Integer::parseInt).toArray();
        }
    }

    private static Map<Integer, Integer> getSocksByColor(int[] socksColors) {
        Map<Integer, Integer> socksByColor = new HashMap<>();
        for (int color : socksColors) {
            Integer socks = socksByColor.getOrDefault(color, 0);
            socksByColor.put(color, socks + 1);
        }
        return socksByColor;
    }

    private static int getTotalPairs(Map<Integer, Integer> socksByColor) {
        int totalPairs = 0;
        for (Integer socks : socksByColor.values()) {
            totalPairs += (socks / 2);
        }
        return totalPairs;
    }

    private static int getPairsByColor(int[] socksColors) {
        Map<Integer, Integer> socksByColor = getSocksByColor(socksColors);
        return getTotalPairs(socksByColor);
    }

    public static void main(String[] args) throws IOException {
        int[] socksColors = getSocksColors();
        int pairs = getPairsByColor(socksColors);
        System.out.println(pairs);
    }
}
