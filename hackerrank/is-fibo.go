package main

import "os"
import "fmt"
import "log"
import "math"
import "bufio"
import "strconv"

func getTestCases() []int {
	testCases := []int{}
	line := bufio.NewScanner(os.Stdin)
	numberOfCases, err := strconv.Atoi(line.Text())
	if numberOfCases >= 0 {
		for i := 0; i < numberOfCases && line.Scan(); i++ {
			number, _ := strconv.Atoi(line.Text())
			testCases = append(testCases, number)
		}
	} else {
		log.Println("else", err)
	}
	return testCases
}

func isPerfectSquare(number int) bool {
	floatNumber := math.Sqrt(float64(number))
	intNumber := float64(math.Trunc(floatNumber))
	isPerfectSquare := (intNumber == floatNumber)
	return isPerfectSquare
}

func isFibonacciNumber(number int) bool {
	fiveByXSquare := 5 * number * number;
	isFibonacciNumber := isPerfectSquare(fiveByXSquare + 4) || isPerfectSquare(fiveByXSquare - 4);
	return isFibonacciNumber
}

func main() {

	// https://www.hackerrank.com/challenges/is-fibo

	// testCases := getTestCases();
	testCases := []int{5,7,8}
	for _, element := range testCases {
		log.Println("element", element)
		if (isFibonacciNumber(element)) {
			fmt.Println("IsFibo")
		} else {
			fmt.Println("IsNotFibo")
		}
	}
}
