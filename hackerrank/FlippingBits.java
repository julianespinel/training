import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

// https://www.hackerrank.com/challenges/flipping-bits/problem
public class FlippingBits {

  private static String addLeadingZeros(String bits) {
    while (bits.length() < 32) {
      bits = "0" + bits;
    }
    return bits;
  }

  private static String toBits(long number) {
    String bits = Long.toBinaryString(number);
    return addLeadingZeros(bits);
  }

  private static String flipBits(String bits) {
    String flippedBits = "";
    for (char bit : bits.toCharArray()) {
      char flipped = (bit == '0') ? '1' : '0';
      flippedBits += flipped;
    }
    return flippedBits;
  }

  private static long toUnsignedNumber(String bits) {
    return Long.parseUnsignedLong(bits, 2);
  }

  public static void main(String[] args) throws IOException {
    try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
      int cases = Integer.parseInt(reader.readLine());
      for (int i = 0; i < cases; i++) {
        /*
         * Why do we use long instead of int? Because in Java there are signed integers only.
         * So if we get 2^31 + 1 as input, that value is greater than Integer.MAX_VALUE.
         */
        long inputCase = Long.parseLong(reader.readLine());
        String bits = toBits(inputCase);
        String flippedBits = flipBits(bits);
        long result = toUnsignedNumber(flippedBits);
        System.out.println(result);
      }
    }
  }
}
