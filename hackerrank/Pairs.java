import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import java.util.Map;
import java.util.HashMap;

// https://www.hackerrank.com/challenges/pairs/problem
public class Pairs {

    private static final String SPACE = " ";

    private static long[] getArray(int size, BufferedReader reader) throws IOException {
        String line = reader.readLine();
        String[] split = line.split(SPACE);
        long[] numbers = new long[size];
        if (split.length != size) {
            throw new IllegalStateException("The given size and the size of " +
                    "the array given as input does not match.");
        }
        for (int i = 0; i < size; i++) {
            numbers[i] = Long.parseLong(split[i]);
        }
        return numbers;
    }

    /**
     * Given an array of longs, returns a map with each element in the
     * array as a key.
     *
     * Complexity: O(n)
     *
     * @param numbers Array of longs.
     * @return Map with each element in the array as a key.
     */
    private static Map<Long, Boolean> getNumbersMap(long[] numbers) {
        Map<Long, Boolean> map = new HashMap<>();
        for (long number : numbers) {
            map.put(number, true);
        }
        return map;
    }

    /**
     * Get amount of pairs in the array whose difference is equal to the target.
     *
     * Complexity: O(n)
     *
     * @param target Number we need to obtain as a result of the difference
     *               between two numbers in the given array.
     * @param numbers Array we use to find two numbers whose difference is
     *                equal to the target.
     * @return Amount of pairs in the array whose difference is
     *         equal to the target.
     */
    private static int getPairsToReachTarget(long target, long[] numbers) {
        int pairs = 0;
        Map<Long, Boolean> numbersMap = getNumbersMap(numbers);
        for (long number : numbers) {
            long numberToReachTarget = number + target;
            boolean numberExists = numbersMap.getOrDefault(numberToReachTarget, false);
            if (numberExists) {
                pairs++;
            }
        }
        return pairs;
    }

    public static void main(String[] args) throws IOException {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
            String line = reader.readLine();
            String[] split = line.split(SPACE);
            int arraySize = Integer.parseInt(split[0]);
            long target = Long.parseLong(split[1]);
            long[] numbers = getArray(arraySize, reader);
            int pairs = getPairsToReachTarget(target, numbers);
            System.out.println(pairs);
        }
    }
}
