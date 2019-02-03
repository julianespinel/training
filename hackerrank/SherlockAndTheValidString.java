import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

import java.util.Collection;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;

// https://www.hackerrank.com/challenges/sherlock-and-valid-string/problem
public class SherlockAndTheValidString {

  private static final String YES = "YES";
  private static final String NO = "NO";

  private static String readInput() throws IOException {
    try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
      String input = reader.readLine();
      return input;
    }
  }

  private static Map<Character, Integer> getCharFrequencies(String string) {
    Map<Character, Integer> charFrequencies = new HashMap<>();
    for (Character element : string.toCharArray()) {
      int charFrequency = charFrequencies.getOrDefault(element, 0) + 1;
      charFrequencies.put(element, charFrequency);
    }
    return charFrequencies;
  }

  /**
   * Return a map containing the frequencies of the values given as argument.
   */
  private static Map<Integer, Integer> getFrequencies(Collection<Integer> numbers) {
    Map<Integer, Integer> frequencies = new HashMap<>();
    for (Integer charFrequency : numbers) {
      int frequency = frequencies.getOrDefault(charFrequency, 0) + 1;
      frequencies.put(charFrequency, frequency);
    }
    return frequencies;
  }

  private static int getKeyWithMinValue(Map<Integer, Integer> map) {
    int minValue = Integer.MAX_VALUE;
    int minKey = 0;
    for (Integer key : map.keySet()) {
      int value = map.get(key);
      if (value < minValue) {
        minValue = value;
        minKey = key;
      }
    }
    return minKey;
  }

  private static char getKeyWithValue(Map<Character, Integer> map, int value) {
    for (Character key : map.keySet()) {
      if (value == map.get(key)) {
        return key;
      }
    }
    throw new IllegalStateException("The given map does not have the value: " + value);
  }

  /**
   * Remove one character given the frequencies of the input string.
   *
   * How we determine which character to remove?
   *
   * Because we can only remove one character, if we have a chance to make
   * the string a valid one, it would be by removing a character with less
   * repetition frequency.
   *
   * Example:
   * string: "aabbc"
   *
   * charFrequencies: { a: 2, b: 2, c: 1 }
   * frequencies: { 2: 2, 1: 1 }
   *
   * 2 is the frequency of two different characters: 'a' and 'b'.
   * 1 is the frequency of only one character: 'c'.
   *
   * In this case by removing 'c' we have the valid string: "aabb".
   *
   * @param charFrequencies frequencies of the string given as input of the program.
   * @return a set of unique frequencies after removing one character.
   */
  private static Set<Integer> removeOneCharacter(Map<Character, Integer> charFrequencies) {
    Map<Integer, Integer> frequencies = getFrequencies(charFrequencies.values());
    int frequencyValue = getKeyWithMinValue(frequencies);
    char key = getKeyWithValue(charFrequencies, frequencyValue);
    int frequency = charFrequencies.get(key);
    charFrequencies.put(key, frequency - 1);
    Set<Integer> uniqueFrequencies = new HashSet<>(charFrequencies.values());
    // Zero is not a valid frequency, so we remove it.
    uniqueFrequencies.remove(0);
    return uniqueFrequencies;
  }

  private static boolean isValid(String string) {
    Map<Character, Integer> charFrequencies = getCharFrequencies(string);
    Set<Integer> uniqueFrequencies = new HashSet<>(charFrequencies.values());

    if (uniqueFrequencies.size() <= 1) {
      return true;
    }
    if (uniqueFrequencies.size() > 2) {
      return false;
    }

    // If frequencies.size() == 2
    uniqueFrequencies = removeOneCharacter(charFrequencies);
    return uniqueFrequencies.size() == 1;
  }

  private static void printResult(boolean bool) {
    String result = bool ? YES : NO;
    System.out.println(result);
  }

  public static void main(String[] args) throws IOException {
    String string = readInput();
    boolean isValidString = isValid(string);
    printResult(isValidString);
  }
}
