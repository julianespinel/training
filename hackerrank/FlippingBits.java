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
    StringBuilder builder = new StringBuilder();
    long remainder = number % 2;
    long result = Math.floorDiv(number, 2);
    builder.append(remainder);
    while (result != 0) {
      remainder = result % 2;
      result = Math.floorDiv(result, 2);
      builder.append(remainder);
    }
    String bits = builder.reverse().toString();
    return addLeadingZeros(bits);
  }

//  /*
//   * Method to go from a number to an array of 32 bits using the Java API.
//   */
//  private static String toBits(long number) {
//    String bits = Long.toBinaryString(number);
//    return addLeadingZeros(bits);
//  }

  private static String flipBits(String bits) {
    String flippedBits = "";
    for (char bit : bits.toCharArray()) {
      char flipped = (bit == '0') ? '1' : '0';
      flippedBits += flipped;
    }
    return flippedBits;
  }

  private static long toUnsignedNumber(String bits) {
    long unsignedNumber = 0;
    char[] chars = bits.toCharArray();
    for (int i = 0; i < chars.length; i++) {
      if (chars[i] == '1') {
        /*
         * We use (chars.length - 1), because the conversion is zero-indexed.
         * 32 bits From 31 to 0.
         */
        unsignedNumber += Math.pow(2, chars.length - 1 - i);
      }
    }
    return unsignedNumber;
  }

//  /*
//   * Method to go from an array of bits to an unsigned number
//   * using the Java API.
//   */
//  private static long toUnsignedNumber(String bits) {
//    return Long.parseUnsignedLong(bits, 2);
//  }

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
