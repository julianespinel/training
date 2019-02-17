import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

import java.util.*;

// https://www.hackerrank.com/challenges/candies/problem
public class Candies2 {

    private static final long SENTINEL = -1;

    //------------------------------------------------------------
    // IO functions
    // -----------------------------------------------------------

    private static int readSize(BufferedReader reader) throws IOException {
        String line = reader.readLine();
        return Integer.parseInt(line);
    }

    private static long[] readArray(BufferedReader reader, int size) throws IOException {
        long[] array = new long[size];
        for (int i = 0; i < size; i++) {
            String line = reader.readLine();
            array[i] = Long.parseLong(line);
        }
        return array;
    }

    //------------------------------------------------------------
    // Functions to calculate minimum candies to buy
    // -----------------------------------------------------------

    private static long getLastElement(Stack<Long> stack) {
        return (stack.isEmpty()) ? 0 : stack.peek();
    }

    private static long getNextScore(int index, long[] scores) {
        int nextIndex = index + 1;
        return (nextIndex < scores.length) ? scores[nextIndex] : SENTINEL;
    }

    private static long[] reverseArray(long[] array) {
        int length = array.length;
        long[] reversedArray = new long[length];
        for (int i = 0; i < length; i++) {
            reversedArray[i] = array[length - i - 1];
        }
        return reversedArray;
    }

    private static Stack<Long> calculateCandiesForDuplicatedElements(Stack<Long> stack) {
        long candies = getLastElement(stack);
        candies = (candies != 1) ? 1 : candies + 1;
        stack.push(candies);
        return stack;
    }

    private static Stack<Long> calculateCandiesForLastElement(int index, long[] scores, Stack<Long> stack, long candies) {
        long previousScore = scores[index - 1];
        long currentScore = scores[index];
        if (previousScore == currentScore) {
            return calculateCandiesForDuplicatedElements(stack);
        }
        candies++;
        stack.push(candies);
        return stack;
    }

    private static Stack<Long> getMinCandies(long[] scores) {
        Stack<Long> stack = new Stack<>();
        long candies = 0;
        for (int i = 0; i < scores.length; i++) {
            long currentScore = scores[i];
            long nextScore = getNextScore(i, scores);
            if (nextScore == SENTINEL) {
                return calculateCandiesForLastElement(i, scores, stack, candies);
            }
            if (currentScore < nextScore) {
                candies++;
                stack.push(candies);
            }
            if (currentScore == nextScore) {
                stack = calculateCandiesForDuplicatedElements(stack);
            }
            if (currentScore > nextScore) {
                // If is in a sequence, then candies++ else 0.
                candies = (candies > 0) ? candies + 1 : 0;
                stack.push(candies);
                candies = 0;

            }
        }
        return stack;
    }

    private static long getCandiesToBuy(long[] scores) {
        Stack<Long> firstStack = getMinCandies(scores);
        long[] reversedScores = reverseArray(scores);
        Stack<Long> secondStack = getMinCandies(reversedScores);
        if (firstStack.size() != secondStack.size()) {
            throw new IllegalStateException("Both stacks must be the same size.");
        }

        long sum = 0;
        int secondStackSize = secondStack.size();
        for (int i = 0; i < firstStack.size(); i++) {
            long firstElement = firstStack.get(i);
            // Get elements from second stack backwards.
            long secondElement = secondStack.get(secondStackSize - i - 1);
            sum += Math.max(firstElement, secondElement);
        }
        return sum;
    }

    //------------------------------------------------------------
    // Main function
    // -----------------------------------------------------------

    public static void main(String[] args) throws IOException {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
            int size = readSize(reader);
            long[] scores = readArray(reader, size);
            long candiesToBuy = getCandiesToBuy(scores);
            System.out.println(candiesToBuy);
        }
    }
}
