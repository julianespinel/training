#!/bin/python3

# https://www.hackerrank.com/challenges/mini-max-sum/problem

"""
The algorithm used to solve this problem is the following:
1. Get the minimum element of the array
2. Get the maximum element of the array
3. Get the sum of all the elements in the array
4. Calculate the min sum -> sum(arr) - max_element
5. Calculate the max sum -> sum(arr) - min_element

The complexity analysis of the proposed algorithm is O(1). Why?

Because the input of the problem is fixed, an array of 5 elements.
According to the book Introduction to Algorithms:

> When we look at input sizes large enough to make only the order of growth of
> the running time relevant, we are studying the asymptotic efficiency of
> algorithms. That is, we are concerned with how the running time of an algorithm
> increases with the size of the input in the limit, as the size of the input
> increases without bound. Usually, an algorithm that is asymptotically more
> efficient will be the best choice for all but very small inputs.

"3 Growth of Functions." Introduction to Algorithms, by Thomas H. Cormen,
MIT Press, 2009.

In short, asymptotic analysis of a problem with a fixed input can be
simplified as O(1).

For a longer explanation please see: https://stackoverflow.com/a/2027842/2420718
"""

def solve(arr):
    min_element = min(arr)
    max_element = max(arr)
    sum_arr = sum(arr)
    return (sum_arr - max_element, sum_arr - min_element)

if __name__ == '__main__':
    arr = list(map(int, input().rstrip().split()))
    (min_sum, max_sum) = solve(arr)
    print(f'{min_sum} {max_sum}')
