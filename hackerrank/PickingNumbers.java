import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.LinkedHashMap;
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

    /**
     * Get frequency of every number in the given array.
     *
     * @param numbers An array with integer numbers.
     * @return a map representing the frequency of each number in the given array.
     */
    private static Map<Integer, Integer> getFrequency(int[] numbers) {
        // Use linked hash map because it preserves order.
        Map<Integer, Integer> frequency = new LinkedHashMap<>();
        for (int number : numbers) {
            Integer value = frequency.getOrDefault(number, 0);
            frequency.put(number, value + 1);
        }
        return frequency;
    }

    private static int getMaxSubArrayLength(int[] inputNumbers) {
        Arrays.sort(inputNumbers);
        Map<Integer, Integer> frequencies = getFrequency(inputNumbers);

        Integer[] numbers = new Integer[frequencies.keySet().size()];
        numbers = frequencies.keySet().toArray(numbers);

        int globalMax = 0;
        for (int i = 0; i < numbers.length; i++) {
            int currentNumber = numbers[i];
            int nextIndex = i + 1;
            // Case when the current number is the longest subArray alone.
            int localMax = frequencies.get(currentNumber);
            int nextNumber = (nextIndex < numbers.length) ? numbers[nextIndex] : SENTINEL;
            // Case when the current number is the longest subArray with another number.
            if (Math.abs(currentNumber - nextNumber) <= 1) {
                localMax = frequencies.get(currentNumber) + frequencies.getOrDefault(nextNumber, 0);
            }
            globalMax = localMax > globalMax ? localMax : globalMax;
        }
        return globalMax;
    }

    public static void main(String[] args) throws IOException {
        int[] numbers = readInput();
        int maxSubArrayLength = getMaxSubArrayLength(numbers);
        System.out.println(maxSubArrayLength);
    }
}
