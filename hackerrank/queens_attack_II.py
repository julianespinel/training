# https://www.hackerrank.com/challenges/queens-attack-2/problem

from enum import Enum, auto

class Position:

    def __init__(self, row, col):
        self.row = row
        self.col = col

    def id(self):
        """Runtime: O(row + col)"""
        return str(self.row) + str(self.col)


class Direction(Enum):
    """Enum for cardinal directions"""
    N = auto()
    NE = auto()
    E = auto()
    SE = auto()
    S = auto()
    SW = auto()
    W = auto()
    NW = auto()


# Functions to count steps in each cardinal direction
# All of the 8 following fuctions have this runtime in the
# worst case: O(side)

def count_steps_north(queen, obstacles, side):
    steps = 0
    row = queen.row + 1 # We don't need to check the queen's position
    col = queen.col
    for i in range(row, side):
        pos = Position(i, col)
        if (pos.id() in obstacles):
            break
        steps += 1
    return steps


def count_steps_north_east(queen, obstacles, side):
    steps = 0
    row = queen.row + 1
    col = queen.col + 1
    rows = list(range(row, side))
    cols = list(range(col, side))
    path = list(zip(rows, cols))
    for r, c in path:
        pos = Position(r, c)
        if (pos.id() in obstacles):
            break
        steps += 1
    return steps


def count_steps_east(queen, obstacles, side):
    steps = 0
    row = queen.row
    col = queen.col + 1
    for i in range(col, side):
        pos = Position(row, i)
        if (pos.id() in obstacles):
            break
        steps += 1
    return steps


def count_steps_south_east(queen, obstacles, side):
    steps = 0
    row = queen.row - 1
    col = queen.col + 1
    rows = list(range(row, -1, -1))
    cols = list(range(col, side))
    path = list(zip(rows, cols))
    for r, c in path:
        pos = Position(r, c)
        if (pos.id() in obstacles):
            break
        steps += 1
    return steps


def count_steps_south(queen, obstacles, side):
    steps = 0
    row = queen.row - 1
    col = queen.col
    for i in range(row, -1, -1):
        pos = Position(i, col)
        if (pos.id() in obstacles):
            break
        steps += 1
    return steps


def count_steps_south_west(queen, obstacles, side):
    steps = 0
    row = queen.row - 1
    col = queen.col - 1
    rows = list(range(row, -1, -1))
    cols = list(range(col, -1, -1))
    path = list(zip(rows, cols))
    for r, c in path:
        pos = Position(r, c)
        if (pos.id() in obstacles):
            break
        steps += 1
    return steps


def count_steps_west(queen, obstacles, side):
    steps = 0
    row = queen.row
    col = queen.col - 1
    for i in range(col, -1, -1):
        pos = Position(row, i)
        if (pos.id() in obstacles):
            break
        steps += 1
    return steps


def count_steps_north_west(queen, obstacles, side):
    steps = 0
    row = queen.row + 1
    col = queen.col - 1
    rows = list(range(row, side))
    cols = list(range(col, -1, -1))
    path = list(zip(rows, cols))
    for r, c in path:
        pos = Position(r, c)
        if (pos.id() in obstacles):
            break
        steps += 1
    return steps


def read_side_and_obstacles_size():
    """Function to read the first line of the input"""
    line = input().split()
    side = int(line[0])
    obstacles_size = int(line[1])
    return (side, obstacles_size)


def read_queen_position():
    """Function to read the second line of the input"""
    line = input().split()
    row = int(line[0]) - 1 # Make it zero indexed
    col = int(line[1]) - 1 # Make it zero indexed
    return Position(row, col)


def read_obstacles(obstacles_size):
    """
    Function to read the second line of the input.
    It returns a dictionary of position.id() -> Position
    """
    obstacles_dict = {}
    for i in range(obstacles_size):
        line = input().split()
        row = int(line[0]) - 1 # Make it zero indexed
        col = int(line[1]) - 1 # Make it zero indexed
        pos = Position(row, col)
        obstacles_dict[pos.id()] = pos
    return obstacles_dict


# Dictonary to map cardinal direction and its respective function to
# calculate the possible steps of th queen.
directions_dict = {
    Direction.N: count_steps_north,
    Direction.NE: count_steps_north_east,
    Direction.E: count_steps_east,
    Direction.SE: count_steps_south_east,
    Direction.S: count_steps_south,
    Direction.SW: count_steps_south_west,
    Direction.W: count_steps_west,
    Direction.NW: count_steps_north_west
}


def steps_in_direction(direction, queen, obstacles, side):
    """
    Returns the possible steps in the  given direction.
    The runtime of this function is O(side)
    """
    count_steps_function = directions_dict[direction]   # O(1)
    return count_steps_function(queen, obstacles, side) # O(side)


def solve(side, queen, obstacles):
    """
    Solve the problem by counting the possible steps of the queen in each
    direction.

    The total runtime complexity is:
    O(directions) * O(side)
    -> directions is always 8, therefore:
    O(8) * O(side)
    -> Drop the constants in complexity analysis, therefore:
    O(side)
    """
    steps = 0
    for direction in list(Direction):
        steps += steps_in_direction(direction, queen, obstacles, side)
    return steps


if __name__ == "__main__":
    (side, obstacles_size) = read_side_and_obstacles_size()
    queen = read_queen_position()
    obstacles = read_obstacles(obstacles_size)
    steps = solve(side, queen, obstacles)
    print(steps)
