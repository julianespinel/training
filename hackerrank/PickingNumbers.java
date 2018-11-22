import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

// https://www.hackerrank.com/challenges/picking-numbers/problem
public class PickingNumbers {

    private static final String SPACE = " ";
    private static final int SENTINEL = -1;

    private static int[] readInput() throws IOException {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
            reader.readLine();
            String array = reader.readLine();
            String[] chars = array.split(SPACE);
            return Arrays.stream(chars).mapToInt(Integer::parseInt).toArray();
        }
    }

    private static Map<Integer, Integer> getFrequency(int[] numbers) {
        Map<Integer, Integer> frequency = new HashMap<>();
        for (int number : numbers) {
            Integer value = frequency.getOrDefault(number, 0);
            frequency.put(number, value + 1);
        }
        return frequency;
    }

    private static int[] getMaxKeyAndNeighbours(Map<Integer, Integer> frequencies) {
        Integer[] keys = new Integer[frequencies.keySet().size()];
        keys = frequencies.keySet().toArray(keys);

        int maxKey = SENTINEL;
        int maxFrequency = SENTINEL;
        int previousKey = SENTINEL;
        int nextKey = SENTINEL;

        for (int i = 0; i < keys.length; i++) {
            int key = keys[i];
            int frequency = frequencies.get(key);
            if (frequency >= maxFrequency) {
                maxKey = key;
                maxFrequency = frequency;

                int previousKeyIndex = i - 1;
                previousKey = (previousKeyIndex >= 0) ? keys[previousKeyIndex] : SENTINEL;

                int nextKeyIndex = i + 1;
                nextKey = (nextKeyIndex < keys.length) ? keys[nextKeyIndex] : SENTINEL;
            }
        }

        return new int[]{ previousKey, maxKey, nextKey };
    }

    private static int getMaxSubArrayLength(int[] numbers) {
        Arrays.sort(numbers);
        Map<Integer, Integer> frequencies = getFrequency(numbers);
        int[] previousMaxNext = getMaxKeyAndNeighbours(frequencies);
        int previousKey = previousMaxNext[0];
        int maxKey = previousMaxNext[1];
        int nextKey = previousMaxNext[2];

        int previousFrequency = 0;
        if (previousKey != SENTINEL && Math.abs(previousKey - maxKey) <= 1) {
            previousFrequency = frequencies.get(previousKey);
        }

        int nextFrequency = 0;
        if (nextKey != SENTINEL && Math.abs(nextKey - maxKey) <= 1) {
            nextFrequency = frequencies.get(nextKey);
        }

        int otherFrequency = Math.max(previousFrequency, nextFrequency);
        int maxFrequency = frequencies.get(maxKey);
        return maxFrequency + otherFrequency;
    }

    public static void main(String[] args) throws IOException {
        int[] numbers = readInput();
        int maxSubArrayLength = getMaxSubArrayLength(numbers);
        System.out.println(maxSubArrayLength);
    }
}
