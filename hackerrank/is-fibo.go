package main

import "fmt"
import "math"

func getTestCases() []float64 {
	testCases := []float64{}
	var numberOfLines int
	fmt.Scanf("%d", &numberOfLines)

	if numberOfLines > 0 {
		for i := 0; i < numberOfLines; i++ {
			var testCase float64
			fmt.Scanf("%f", &testCase)
			testCases = append(testCases, testCase)
		}
	}

	// fmt.Println(numberOfLines)
	// fmt.Println(testCases)

	return testCases
}

func isPerfectSquare(number float64) bool {
	floatNumber := math.Sqrt(float64(number))
	// intNumber := big.NewFloat(floatNumber) // It seems like this line only works in go 1.5
	// isPerfectSquare := intNumber.IsInt()
	intNumber := float64(math.Trunc(floatNumber))
	isPerfectSquare := (intNumber == floatNumber)
	// fmt.Println(number)
	// fmt.Println(floatNumber)
	// fmt.Println(intNumber)
	// fmt.Println(isPerfectSquare)
	return isPerfectSquare
}

func isFibonacciNumber(number float64) bool {
	fiveByXSquare := float64(5 * number * number)
	isFibonacciNumber := (isPerfectSquare(fiveByXSquare+4) || isPerfectSquare(fiveByXSquare-4))
	return isFibonacciNumber
}

func main() {

	// https://www.hackerrank.com/challenges/is-fibo	
	testCases := getTestCases()
	// testCases := []float64{7778742049}
	// testCases := []float64{5,7,8}
	for _, element := range testCases {
		if isFibonacciNumber(element) {
			fmt.Println("IsFibo")
		} else {
			fmt.Println("IsNotFibo")
		}
	}
}
