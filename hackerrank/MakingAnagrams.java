import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;

// https://www.hackerrank.com/challenges/ctci-making-anagrams/problem
public class MakingAnagrams {

  private static Map<Character, Integer> getCharacterFrequency(String word) {
    Map<Character, Integer> frequency = new HashMap<Character, Integer>();
    for (char character : word.toCharArray()) {
      int value = frequency.getOrDefault(character, 0);
      frequency.put(character, value + 1);
    }
    return frequency;
  }

  private static Character[] getArrayFromKeys(Map<Character, Integer> map) {
    Character[] chars = new Character[map.keySet().size()];
    return map.keySet().toArray(chars);
  }

  private static int getDeletionsToFormAnagram(String firstWord, String secondWord) {
    int deletions = 0;
    Map<Character, Integer> firstFrequencies = getCharacterFrequency(firstWord);
    Map<Character, Integer> secondFrequencies = getCharacterFrequency(secondWord);
    Character[] firstChars = getArrayFromKeys(firstFrequencies);
    for (char character : firstChars) {
      int firstValue = firstFrequencies.get(character);
      int secondValue = secondFrequencies.getOrDefault(character, 0);
      int difference = Math.abs(firstValue - secondValue);
      if (secondValue != 0) {
        secondFrequencies.remove(character);
      }
      deletions += difference;
    }
    Character[] secondChars = getArrayFromKeys(secondFrequencies);
    for (char character : secondChars) {
      deletions += secondFrequencies.get(character);
    }
    return deletions;
  }

  public static void main(String[] args) throws IOException {
    try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
      String firstWord = reader.readLine();
      String secondWord = reader.readLine();
      int deletions = getDeletionsToFormAnagram(firstWord, secondWord);
      System.out.println(deletions);
    }
  }
}
