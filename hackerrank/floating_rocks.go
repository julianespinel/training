// link: https://www.hackerrank.com/challenges/harry-potter-and-the-floating-rocks
package main

import (
	"bufio"
	"fmt"
	"math"
	"math/big"
	"os"
	"strconv"
	"strings"
)

type Point struct {
	x float64
	y float64
}

func getNumbersFromStrings(stringArray []string) []float64 {
	numbers := []float64{}
	for i := 0; i < len(stringArray); i++ {
		number, _ := strconv.ParseFloat(stringArray[i], 64)
		numbers = append(numbers, number)
	}
	return numbers
}

func getP1AndP2(line string) (Point, Point) {
	stringArray := strings.Split(line, " ")
	if len(stringArray) < 4 {
		panic("the stringArray length is less than four" + line)
	}
	numbers := getNumbersFromStrings(stringArray)
	p1 := Point{x: numbers[0], y: numbers[1]}
	p2 := Point{x: numbers[2], y: numbers[3]}
	return p1, p2
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	scanner.Scan()
	line := scanner.Text()
	testCases, _ := strconv.Atoi(line)
	if testCases > 0 {
		bigOne := big.NewInt(1)
		for i := 0; i < testCases && scanner.Scan(); i++ {
			line = scanner.Text()
			p1, p2 := getP1AndP2(line)
			dx := math.Abs(p2.x - p1.x)
			dy := math.Abs(p2.y - p1.y)
			numberOfRocks := float64(0)
			if dx == dy && dy == 0 {
				numberOfRocks = 0
			} else if dx == 0 {
				numberOfRocks = dy - 1
			} else if dy == 0 {
				numberOfRocks = dx - 1
			} else {
				bigDx := big.NewInt(int64(dx))
				bigDy := big.NewInt(int64(dy))
				gcdPartial := big.NewInt(0).GCD(nil, nil, bigDx, bigDy)
				gcd := gcdPartial.Sub(gcdPartial, bigOne)
				numberOfRocks = float64(gcd.Int64())
			}
			fmt.Println(numberOfRocks)
		}
	} else {
		panic("the number of test cases is <= 0")
	}
}
