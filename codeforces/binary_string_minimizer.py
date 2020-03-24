class Case:
    def __init__(self, size, moves, string):
        self.size = size
        self.moves = moves
        self.string = string


SPACE = ' '
ZERO = '0'
ONE = '1'


def get_case():
    line_elements = input().split(SPACE)
    size = int(line_elements[0])
    moves = int(line_elements[1])
    string = list(input())
    return Case(size, moves, string)


def swap(index, next_zero_index, string):
    string[index], string[next_zero_index] = string[next_zero_index], string[index]
    return string


def solve(case):
    next_zero_index = 0
    remaining_moves = case.moves
    string = case.string
    for index in range(case.size):
        if remaining_moves == 0:
            break

        current_char = string[index]
        if index == 0:
            if current_char == ZERO:
                next_zero_index += 1
            continue

        previous_char = string[index - 1]
        need_to_move = current_char == ZERO and previous_char == ONE
        if need_to_move:
            desired_moves = index - next_zero_index
            allowed_moves = min(remaining_moves, desired_moves)
            target_index = next_zero_index + abs(desired_moves - allowed_moves)
            string = swap(index, target_index, string)
            remaining_moves -= allowed_moves
            next_zero_index += 1
        elif current_char == ZERO:
            next_zero_index += 1
    return string


if __name__ == "__main__":
    number_of_cases = int(input())
    for i in range(number_of_cases):
        case = get_case()
        solvedList = solve(case)
        solution = "".join(solvedList)
        print(solution)
