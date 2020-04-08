"""
# Problem statement

https://leetcode.com/explore/interview/card/top-interview-questions-easy/127/strings/879/

## Algorithm description

Traverse the given input until the middle of it.
Swap the extremes of the input until reach the middle of it.

Example:

input = "abcde"
len(input) = 5
middle = ceil(5 / 3) = 3

ii = initial_index
fi = final_index

i = 0, fi = 4, "abcde"
i = 1, fi = 3, "ebcda"
i = 2, fi = 2, "edcba"

This works for odd and event inputs.

### Cases

I considered the following cases:

1. empty: "" -> ""
1. one: "a" -> "a"
1. String length is odd: "abc" -> "cba"
1. String length is even: "abcd" -> "dcba"

### Examples:

"abcde" -> l: 5 -> int(5 / 2) = 3 ->
i = 0 < 3
ii
0, 5 - 1 - 0 = 4
1, 5 - 1 - 1 = 3
2, 5 - 1 - 2 = 2

"abcd" -> l: 4 -> int(4 / 2) = 2 ->
i = 0 < 2
ii
0, 4 - 1 - 0 = 3
1, 4 - 1 - 1 = 2


## Complexity

### Time

1. Traverse the given input until the middle of it: O(n)
1. Swap elements of the input: O(1)

Total = O(n) + O(1) = O(n)

### Space

Only simple variables were created: O(1)

## To improve

I prefer to avoid mutating data structures, so I would preferred to create
a new array to store the answer and the return it.

I mutated the given input because that was a constraint given in the problem
statement:
> Do not allocate extra space for another array, you must do this by modifying the input array in-place with O(1) extra memory.

"""

import math

class Solution:

    def reverseString(self, string: [str]) -> None:
        """
        Do not return anything, modify s in-place instead.
        """
        length = len(string)
        if length <= 1:
            return
        limit = math.ceil(length / 2)
        for index in range(limit):
            final_index = length - 1 - index
            string[index], string[final_index] = string[final_index], string[index]


if __name__ == "__main__":
    solution = Solution()
    string = ["H","a","n","n","a","h"]
    solution.reverseString(string)
    print(string)
