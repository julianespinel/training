import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

// https://www.hackerrank.com/challenges/crush
public class AlgorithmicCrush {

    private static final String SPACE = " ";
    private static final long ZERO = 0;

    private static int getListSize(String line) {
        String[] array = line.split(SPACE);
        int listSize = Integer.parseInt(array[0]);
        return listSize;
    }

    private static List<Long> initializeArray(int listSize) {
        List<Long> numbers = new ArrayList<>(listSize);
        for (int i = 0; i < listSize; i++) {
            numbers.add(ZERO);
        }
        return numbers;
    }

    private static int getOperationsNumber(String line) {
        String[] array = line.split(SPACE);
        int operations = Integer.parseInt(array[1]);
        return operations;
    }

    private class Operation {
        private final int initialIndex;
        private final int finalIndex;
        private final long k;

        public Operation(int initialIndex, int finalIndex, long k) {
            this.initialIndex = initialIndex;
            this.finalIndex = finalIndex;
            this.k = k;
        }

        public int getInitialIndex() {
            return initialIndex;
        }

        public int getFinalIndex() {
            return finalIndex;
        }

        public long getK() {
            return k;
        }
    }

    private Operation getOperation(String line) {
        String[] array = line.split(SPACE);
        int initialIndex = Integer.parseInt(array[0]) - 1;
        int finalIndex = Integer.parseInt(array[1]) - 1;
        long k = Long.parseLong(array[2]);
        return new Operation(initialIndex, finalIndex, k);
    }

    private List<Long> applyOperation(List<Long> numbers, Operation operation) {
        for (int i = operation.initialIndex; i <= operation.finalIndex; i++) {
            Long currentNumber = numbers.get(i);
            long updatedNumber = currentNumber + operation.getK();
            numbers.set(i, updatedNumber);
        }
        return numbers;
    }

    private List<Long> processOperations(BufferedReader reader, int operations, List<Long> numbers) throws IOException {
        for (int i = 0; i < operations; i++) {
            String line = reader.readLine();
            Operation operation = getOperation(line);
            numbers = applyOperation(numbers, operation);
        }
        return numbers;
    }

    private static void printMax(List<Long> numbers) {
        long max = 0;
        for (Long number : numbers) {
            max = number > max? number : max;
        }
        System.out.println(max);
    }

    public static void main(String[] args) {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
            AlgorithmicCrush main = new AlgorithmicCrush();
            String line = reader.readLine();
            int listSize = getListSize(line);
            List<Long> numbers = initializeArray(listSize);
            int operations = getOperationsNumber(line);
            numbers = main.processOperations(reader, operations, numbers);
            printMax(numbers);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
