package main

import "fmt"
import "math"
import "math/big"

func getTestCases() []int {
	testCases := []int{}
	var numberOfLines int
	fmt.Scanf("%d", &numberOfLines)

	if numberOfLines > 0 {
		for i := 0; i < numberOfLines; i++ {
			var testCase int
			fmt.Scanf("%d", &testCase)
			testCases = append(testCases, testCase)
		}
	}	
	return testCases
}

func isPerfectSquare(number int) bool {
	floatNumber := math.Sqrt(float64(number))
	// intNumber := float64(math.Trunc(floatNumber))
	intNumber := big.NewFloat(floatNumber)
	// isPerfectSquare := (intNumber == floatNumber)
	isPerfectSquare := intNumber.IsInt()
	fmt.Println(number)
	fmt.Println(floatNumber)
	fmt.Println(intNumber)
	fmt.Println(isPerfectSquare)
	return isPerfectSquare
}

func isFibonacciNumber(number int) bool {
	fiveByXSquare := 5 * number * number
	isFibonacciNumber := isPerfectSquare(fiveByXSquare+4) || isPerfectSquare(fiveByXSquare-4)
	return isFibonacciNumber
}

func main() {

	// https://www.hackerrank.com/challenges/is-fibo	
	// testCases := getTestCases()
	testCases := []int{7778742049}
	// testCases := []int{5,7,8}
	
	for _, element := range testCases {
		if isFibonacciNumber(element) {
			fmt.Println("IsFibo")
		} else {
			fmt.Println("IsNotFibo")
		}
	}
}
