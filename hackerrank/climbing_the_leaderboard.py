'''
https://www.hackerrank.com/challenges/climbing-the-leaderboard

# Climbing the leaderboard

The function that solves the problem is:

```python
get_positions_per_score(ranks, scores)
```

The solution uses `deque` instead of lists. Why?
To have O(1) in appends and pops from either side of the deque.
See: https://docs.python.org/3/library/collections.html#collections.deque

The complexity of this solution is:
- Time: O(ranks) + O(scores) = O(max(ranks,scores))
- Space: O(ranks) + O(scores) = O(max(ranks,scores))
'''

import sys
from collections import deque


def read_values() -> list[int]:
    sys.stdin.readline()  # skip
    ranks_str = sys.stdin.readline()
    strings = ranks_str.split(' ')
    return list(map(int, strings))


def remove_duplicates(ranks: list[int]) -> list[int]:
    '''
    Given a list sorted in descending order (ranks),
    remove the duplicates of the list.

    Time complexity: O(n)
    Why? See: https://stackoverflow.com/a/7961390/2420718
    '''
    return list(dict.fromkeys(ranks))


def get_positions_per_score(ranks: list[int], scores: list[int]) -> deque[int]:
    '''
    Return the position of each score in the ranks list
    using a zero-based index.

    ranks: list sorted in **descending** order
    scores: list sorted in **descending** order

    Time complexity: O(scores) + O(ranks)
    '''
    positions = deque()  # why a deque? to make all appends O(1)
    ranks_index = 0  # O(1)
    scores_index = 0  # O(1)

    scores_size = len(scores)  # O(1)
    ranks_size = len(ranks)  # O(1)

    # O(scores) + O(ranks)
    while (scores_index < scores_size) and (ranks_index < ranks_size):
        score = scores[scores_index]  # O(1)
        rank = ranks[ranks_index]  # O(1)
        if score >= rank:  # O(1)
            positions.append(ranks_index)  # O(1)
            scores_index += 1  # O(1)
        else:
            ranks_index += 1  # O(1)

    # add missing scores
    while scores_index < scores_size:  # O(scores) in the worst case
        positions.append(ranks_index)  # O(1)
        scores_index += 1  # O(1)

    positions.reverse()  # O(scores)
    return positions


def print_positions(positions: deque[int]) -> None:
    '''
    Print the given positions.

    Add +1 to each element because the response must be
    one-based index instead of zero-based index.
    '''
    for position in positions:
        print(position + 1)


if __name__ == '__main__':
    ranks_input = read_values()
    scores = read_values()

    ranks = remove_duplicates(ranks_input)  # O(ranks)
    scores.reverse()  # O(scores)

    positions = get_positions_per_score(ranks, scores)  # O(ranks) + O(scores)
    # O(scores), because len(positions) == len(scores)
    print_positions(positions)
