import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class FibonacciNumbers {

  private static int fibonacci(int number) {
    List<Integer> cache = new ArrayList<>();
    cache.add(0); // fib(0) = 0
    cache.add(1); // fib(1) = 1
    if (number >= 0 && number < 2) {
      return cache.get(number);
    }
    // populate cache
    for (int i = 2; i < number; i++) {
      int fibonacciNumber = cache.get(i - 1) + cache.get(i - 2);
      cache.add(fibonacciNumber);
    }
    // return fibonacci of the given number
    return cache.get(number - 1) + cache.get(number - 2);
  }

  public static void main(String[] args) {
    Scanner scanner = new Scanner(System.in);
    int number = scanner.nextInt();
    scanner.close();
    int fibonacciNumber = fibonacci(number);
    System.out.println(fibonacciNumber);
  }
}
