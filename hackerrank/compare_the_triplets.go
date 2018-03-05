/* link: https://www.hackerrank.com/challenges/compare-the-triplets/problem
 * How to run:
 * 1. go build compare_the_triplets.go
 * 2. ./compare_the_triplets
 *    5 6 7
 *    3 6 10
 */

package main

import (
	"fmt"
	"bufio"
	"errors"
	"os"
	"strings"
	"strconv"
)

const space = " "

func getLineFromStdin(scanner *bufio.Scanner) string {
	scanner.Scan()
	line := scanner.Text()
	if scanner.Err() != nil {
		panic(scanner.Err())
	}
	return line
}

func getTriplet(line string) ([]int, error) {
	stringArray := strings.Split(line, space)
	length := len(stringArray)
	intArray := make([]int, length)
	if length != 3 {
		return intArray, errors.New("Input line should have at least three numbers")
	}
	for i, str := range stringArray {
		number, err := strconv.Atoi(str)
		if err != nil {
			return intArray, err
		}
		intArray[i] = number
	}
	return intArray, nil
}

func compareTriplets(firstTriplet []int, secondTriplet []int) (int, int) {
	firstScore := 0
	secondScore := 0
	for i, firstItem := range firstTriplet {
		secondItem := secondTriplet[i]
		if firstItem > secondItem {
			firstScore++
		} else if firstItem < secondItem {
			secondScore++
		}
	}
	return firstScore, secondScore
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	firstLine := getLineFromStdin(scanner)
	secondLine := getLineFromStdin(scanner)
	aliceTriplet, err := getTriplet(firstLine)
	if err != nil {
		panic(err)
	}
	bobTriplet, err := getTriplet(secondLine)
	if err != nil {
		panic(err)
	}
	aliceScore, bobScore := compareTriplets(aliceTriplet, bobTriplet)
	fmt.Println(aliceScore, bobScore)
}
