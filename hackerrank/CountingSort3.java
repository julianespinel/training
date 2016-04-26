import java.io.*;
import java.util.*;

// https://www.hackerrank.com/challenges/countingsort3
public class CountingSort3 {

    private Integer[] getNumbersArray() throws IOException {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        int lines = Integer.parseInt(br.readLine());
        Integer[] numbers = new Integer[lines];
        for (int i = 0; i < lines; i++) {
            String[] splitArray = br.readLine().split(" ");
            numbers[i] = Integer.parseInt(splitArray[0]);
        }
        br.close();
        return numbers;
    }

    private Map<Integer, Integer> getNumberFrequencyMap(Integer[] numbers) {
        int zero = 0;
        Map<Integer, Integer> numberFrequencyMap = new Hashtable<>();
        for (Integer number : numbers) {
            Integer frequency = numberFrequencyMap.getOrDefault(number, zero);
            frequency++;
            numberFrequencyMap.put(number, frequency);
        }
        return numberFrequencyMap;
    }

    private String getCounterNumberFrequencyString(Map<Integer, Integer> numberFrequencyMap) {
        String counterNumberFrequency = "";
        int zero = 0;
        int counter = 0;
        int limit = 100;
        for (int i = 0; i < limit; i++) {
            Integer frequency = numberFrequencyMap.getOrDefault(i, zero);
            counter += frequency;
            counterNumberFrequency += counter + " ";
        }
        return counterNumberFrequency.trim();
    }

    public static void main(String[] args) throws IOException {
        CountingSort3 countingSort3 = new CountingSort3();
        Integer[] numbers = countingSort3.getNumbersArray();
        Map<Integer, Integer> numberFrequencyMap = countingSort3.getNumberFrequencyMap(numbers);
        String counterNumberFrequency = countingSort3.getCounterNumberFrequencyString(numberFrequencyMap);
        System.out.println(counterNumberFrequency);
    }
}