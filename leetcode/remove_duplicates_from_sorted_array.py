"""
Please read this as markdown:

# Algorithm description

Problem statement: https://leetcode.com/explore/interview/card/top-interview-questions-easy/92/array/727/

The algorithm implemented is as follows:
1. Check for two base cases:
    1. When `nums` is empty
    2. When `nums` length is 1
    In both cases return the length of the list because there cannot be
    repeated elements.
2. Third case: is the length of `nums` is greater than 1:
    Traverse the `nums` list comparing the current element vs the previous
    element. If those elements are different, move the current element to the
    next in-order position `target_index`.

    `target_index` is the variable we use to keep track of the latest
    index that holds ordered, unique elements.

# Complexity

## Time

The time complexity is O(n). Why?

* Travers: we traverse the given `nums` list only once -> O(n)
* Swap: the `swap` method complexity is O(1).
* Simple operation: the other operations are just boolean comparations,
  counter increases and variable assignments -> O(1).

So we have: O(n) (traverse) * O(1) (swap) * O(1) (simple operations) = O(n)

## Space

The space complexity is O(n). Why?

The only data structure we used to solve the problem was the list to hold
the given input `nums`. -> O(n)

## To improve

The problem statement required to:
> Do not allocate extra space for another array, you must do this by modifying
  the input array in-place with O(1) extra memory.

If this constraint was not in place, I would used a dictionary to get the
unique elements present in `nums` and then would have returned the keys of the
dictionary in ascending order.

The time complexity would have been the same, the space complexity would have
been O(n) (given input) + O(n) (dictionary) ~= O(n). But the maintainability
of the code would have improved because the algorithm would be easier to
understand and modify by others.

"""
class Solution:


    def switch(self, source_index: int, target_index: int, nums: [int]) -> None:
        nums[source_index], nums[target_index] = nums[target_index], nums[source_index]


    def removeDuplicates(self, nums: [int]) -> int:
        if len(nums) <= 1:
            return len(nums)
        target_index = 0
        previous_number = nums[0]
        for current_index, current_number in enumerate(nums):
            if previous_number != current_number:
                target_index += 1
                self.switch(current_index, target_index, nums)
            previous_number = current_number
        unique_length = target_index + 1
        return unique_length


if __name__ == "__main__":
    s = Solution()
    r = s.removeDuplicates([0,0,1,1,1,2,2,3,3,4])
    print(f'result: {r}')
