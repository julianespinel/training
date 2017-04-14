// https://www.hackerrank.com/challenges/array-left-rotation
package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

const space = " "

func checkError(err error) {
	if err != nil {
		log.Panic(err)
	}
}

func getArraySizeAndRotations(scanner *bufio.Scanner) (int, int) {
	scanner.Scan()
	arraySizeString := scanner.Text()
	arraySize, err := strconv.Atoi(arraySizeString)
	checkError(err)
	scanner.Scan()
	rotationsString := scanner.Text()
	rotations, err := strconv.Atoi(rotationsString)
	checkError(err)
	return arraySize, rotations
}

func getInitialArray(scanner *bufio.Scanner, arraySize int) []string {
	array := []string{}
	for len(array) < arraySize && scanner.Scan() {
		number := scanner.Text()
		array = append(array, number)
	}
	return array
}

func rotateArray(rotations int, array []string) []string {
	head := array[rotations:]
	tail := array[:rotations]
	rotatedArray := append(head, tail...)
	return rotatedArray
}

func printArray(array []string) {
	line := strings.Join(array, space)
	fmt.Println(line)
}

func main() {
	// Get inputs
	scanner := bufio.NewScanner(os.Stdin)
	scanner.Split(bufio.ScanWords)
	arraySize, rotations := getArraySizeAndRotations(scanner)
	initialArray := getInitialArray(scanner, arraySize)
	// perform left rotations
	finalArray := rotateArray(rotations, initialArray)
	// print final array
	printArray(finalArray)
}
