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


def get_zero_indices(string):
    zero_indices = []
    for index, char in enumerate(string):
        if char == ZERO:
            zero_indices.append(index)
    return zero_indices


def get_zero_indices_after_moves(initial_zero_indices, ideal_indices, moves):
    final_zero_indices = []
    if len(initial_zero_indices) != len(ideal_indices):
        raise ValueError('initial_zero_indices and ideal_indices should be of '
                         'the same length')
    for i, zero_index in enumerate(initial_zero_indices):
        if moves < 0:
            raise ValueError('moves should be >= 0')
        if moves == 0:
            final_zero_indices.extend(initial_zero_indices[i:])
            return final_zero_indices
        desired_moves = zero_index - ideal_indices[i]
        allowed_moves = min(moves, desired_moves)
        new_zero_index = zero_index - allowed_moves
        final_zero_indices.append(new_zero_index)
        moves = moves - allowed_moves
    return final_zero_indices


def add_zeros(ones, zero_indices):
    for index in zero_indices:
        ones[index] = ZERO
    return ones


def solve(case):
    initial_zero_indices = get_zero_indices(case.string)
    ideal_zero_indices = list(range((len(initial_zero_indices))))
    final_zero_indices = get_zero_indices_after_moves(
        initial_zero_indices, ideal_zero_indices, case.moves)
    ones = [ONE] * case.size
    response = add_zeros(ones, final_zero_indices)
    return response


if __name__ == "__main__":
    number_of_cases = int(input())
    for i in range(number_of_cases):
        case = get_case()
        solvedList = solve(case)
        solution = "".join(solvedList)
        print(solution)
