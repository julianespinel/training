import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

// https://www.hackerrank.com/challenges/crush
public class AlgorithmicCrush {

    private static final String SPACE = " ";
    private static final long ZERO = 0;

    private static int getListSize(String line) {
        String[] array = line.split(SPACE);
        int listSize = Integer.parseInt(array[0]);
        return listSize;
    }

    private static long[] initializeArray(int listSize) {
        long[] numbers = new long[listSize];
        for (int i = 0; i < listSize; i++) {
            numbers[i] = ZERO;
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

    private void applyOperation(long[] numbers, Operation operation) {
        for (int i = operation.initialIndex; i <= operation.finalIndex; i++) {
            long currentNumber = numbers[i];
            long updatedNumber = currentNumber + operation.getK();
            numbers[i] = updatedNumber;
        }
    }

    private long[] processOperations(BufferedReader reader, int operations, long[] numbers) throws IOException {
        for (int i = 0; i < operations; i++) {
            String line = reader.readLine();
            Operation operation = getOperation(line);
            applyOperation(numbers, operation);
        }
        return numbers;
    }

    private static void printMax(long[] numbers) {
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
            int operations = getOperationsNumber(line);
            int listSize = getListSize(line);
            long[] numbers = initializeArray(listSize);
            main.processOperations(reader, operations, numbers);
            printMax(numbers);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

