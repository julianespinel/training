import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import java.util.Map;
import java.util.HashMap;
import java.util.Map;
import java.util.Map;
import java.util.Map;

// https://www.hackerrank.com/challenges/two-strings/problem
public class TwoStrings {

  private static final String YES = "YES";
  private static final String NO = "NO";

  private static Map<Character, Boolean> getMap(String string) {
    Map<Character, Boolean> map = new HashMap();
    for (Character aChar : string.toCharArray()) {
      map.put(aChar, true);
    }
    return map;
  }

  private static String hasSubString(String firstLine, String secondLine) {
    Map<Character, Boolean> chars = getMap(firstLine);
    for (Character aChar : secondLine.toCharArray()) {
      if (chars.getOrDefault(aChar, false)) {
        return YES;
      }
    }
    return NO;
  }

  public static void main(String[] args) throws IOException {
    try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
      int cases = Integer.parseInt(reader.readLine());
      while (cases > 0) {
        String firstLine = reader.readLine();
        String secondLine = reader.readLine();
        String hasSubString = hasSubString(firstLine, secondLine);
        System.out.println(hasSubString);
        cases--;
      }
    }
  }
}
