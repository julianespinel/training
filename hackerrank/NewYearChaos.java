import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

// https://www.hackerrank.com/challenges/new-year-chaos/problem
public class NewYearChaos {

    private static final String SPACE = " ";
    private static final String TOO_CHAOTIC = "Too chaotic";

    private static List<int[]> readQueues() throws IOException {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
            int testCases = Integer.parseInt(reader.readLine());
            List<int[]> queues = new ArrayList<>(testCases);
            for (int i = 0; i < testCases; i++) {
                reader.readLine(); // Ignore this line
                String[] split = reader.readLine().split(SPACE);
                int[] queue = Arrays.stream(split).mapToInt(Integer::parseInt).toArray();
                queues.add(queue);
            }
            return queues;
        }
    }

    private static void checkQueueIsPossible(int[] queue) {
        for (int i = 0; i < queue.length; i++) {
            int correctedIndex = i + 1;
            int currentPosition = queue[i];
            int positionDifference = currentPosition - correctedIndex;
            if (positionDifference > 2) {
                throw new IllegalStateException(TOO_CHAOTIC);
            }
        }
    }

    private static int getMinSwitches(int[] queue) {
        int minSwitches = 0;
        for (int i = queue.length - 1; i >= 0; i--) {
            int initialPosition = queue[i];
            // if someone pass me it should be maximum in my initial position - 2.
            int maxBribedPosition = (initialPosition - 2 > 0) ? initialPosition - 2: 0;
            for (int j = maxBribedPosition; j < i; j++) {
                int first = queue[j];
                boolean wasBrived = first > initialPosition;
                minSwitches += wasBrived ? 1: 0;
            }
        }
        return minSwitches;
    }

    public static void main(String[] args) throws IOException {
        List<int[]> queues = readQueues();
        for (int[] queue : queues) {
            try {
                checkQueueIsPossible(queue);
                int minSwitches = getMinSwitches(queue);
                System.out.println(minSwitches);
            } catch (IllegalStateException e) {
                System.out.println(e.getMessage());
            }
        }
    }
}
