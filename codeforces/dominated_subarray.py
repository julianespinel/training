def read_first_line():
    return int(input())


def read_cases(number_of_cases):
    cases = []
    for i in range(number_of_cases):
        line = input()
        if i % 2 == 1:
            case = [int(string) for string in line.strip().split(' ')]
            cases.append(case)
    return cases


def updateHistory(index, number, history):
    if not number in history:
        history[number] = { "latestIndex": index, "minDifference": None }
        return

    value = history[number]
    latestIndex = value["latestIndex"]
    minDifference = value["minDifference"]
    minimum = index - latestIndex

    if not minDifference or minimum < minDifference:
        history[number] = { "latestIndex": index, "minDifference": minimum }
        return

    # Update index, preserve minDifference
    history[number] = { "latestIndex": index, "minDifference": minDifference }


def solve(case):
    history = {}
    for index, number in enumerate(case):
        updateHistory(index, number, history)

    mins = []
    for value in history.values():
        minDifference = value["minDifference"]
        if minDifference:
            mins.append(minDifference)

    if len(mins) == 0:
        return -1

    return min(mins) + 1


if __name__ == "__main__":
    test_cases = read_first_line()
    lines_per_case = 2
    cases = read_cases(test_cases * lines_per_case)
    for case in cases:
        solution = solve(case)
        print(solution)
