import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

// https://www.hackerrank.com/challenges/balanced-brackets/problem
public class BalancedBrackets {

  private static final String NO = "NO";
  private static final String YES = "YES";

  private static final Map<Character, Character> brackets;
  static {
    brackets = new HashMap<>();
    brackets.put('}', '{');
    brackets.put(')', '(');
    brackets.put(']', '[');
  }

  private static String isBalanced(String string) {
    if (string.length() % 2 != 0) {
      return NO;
    }
    Stack<Character> stack = new Stack<>();
    for (char c : string.toCharArray()) {
      Character openingChar = brackets.getOrDefault(c, null);
      boolean isOpeningChar = openingChar == null;
      if (isOpeningChar) {
        stack.push(c);
      } else {
        if (!stack.empty() && stack.peek() == openingChar) {
          stack.pop();
        } else {
          return NO;
        }
      }
    }
    String answer = (stack.empty()) ? YES : NO;
    return answer;
  }

  public static void main(String[] args) throws IOException {
    try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
      int testCases = Integer.parseInt(reader.readLine());
      for (int i = 0; i < testCases; i++) {
        String string = reader.readLine();
        System.out.println(isBalanced(string));
      }
    }
  }
}
