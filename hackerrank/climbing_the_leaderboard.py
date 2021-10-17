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


def move_to_next_rank(index: int, ranks: deque[int], score: int) -> tuple[int, deque[int]]:
    '''
    Remove the elements from the ranks that are < than the given score.
    When an element is removed the index is decreased.

    Return the tuple (index, ranks)
    '''
    if not ranks:
        return index, ranks

    rank = ranks.pop()  # O(1)
    index -= 1
    while (ranks and score > rank):  # worst case: O(ranks)
        rank = ranks.pop()  # O(1)
        index -= 1

    # should put back the element that ended the while
    if not (score > rank):
        ranks.append(rank)  # O(1)
        index += 1

    return index, ranks


def get_positions_per_score(ranks: deque[int], scores: deque[int]) -> deque[int]:
    '''
    Return the position of each score in the ranks.

    Time complexity: O(scores) + O(ranks)
    '''
    positions = deque()  # why a deque? so all appends are O(1)
    index = len(ranks) - 1  # O(1)

    for score in scores:  # O(scores)
        # O(ranks) in the worst case, however we guarantee that we
        # traverse ranks only once. Therefore, this is not a nested loop
        index, ranks = move_to_next_rank(index, ranks, score)

        if not ranks:
            positions.append(0)  # O(1)
            continue

        rank = ranks[-1]  # get last element, # O(1)

        if score < rank:
            positions.append(index + 1)  # O(1)
        elif score == rank:
            positions.append(index)  # O(1)

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
    scores_input = read_values()

    ranks_list = remove_duplicates(ranks_input)  # O(ranks)
    ranks = deque(ranks_list)  # O(ranks)
    scores = deque(scores_input)  # O(scores)

    positions = get_positions_per_score(ranks, scores)  # O(ranks) + O(scores)
    print_positions(positions)  # O(positions)
