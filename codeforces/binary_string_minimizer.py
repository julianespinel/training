class Case:
    def __init__(self, size, moves, string):
        self.size = size
        self.moves = moves
        self.string = string


SPACE = " "
ZERO = '0'


def getCase():
    lineElements = input().split(SPACE)
    size = int(lineElements[0])
    moves = int(lineElements[1])
    string = input()
    return Case(size, moves, string)


def getAllowedMoves(remainingMoves, desiredMoves):
    hasEnoughMoves = (remainingMoves - desiredMoves >= 0)
    return desiredMoves if hasEnoughMoves else remainingMoves


def move(index, allowedMoves, string):
    targetIndex = index - allowedMoves
    return string[:targetIndex] + string[index] + string[targetIndex:index] + string[index + 1:]


def solve(case):
    nextZeroIndex = 0
    remainingMoves = case.moves
    string = case.string
    for index in range(case.size):
        if  remainingMoves == 0:
            break
        char = string[index]
        if index == 0:
            if char == ZERO:
                nextZeroIndex += 1
            continue
        previousChar = string[index - 1]
        needToMove = char == ZERO and previousChar != ZERO
        if needToMove:
            desiredMoves = index - nextZeroIndex
            allowedMoves = getAllowedMoves(remainingMoves, desiredMoves)
            string = move(index, allowedMoves, string)
            remainingMoves -= allowedMoves
            nextZeroIndex += 1
        elif char == ZERO:
            nextZeroIndex += 1
    return string


if __name__ == "__main__":
    number_of_cases = int(input())
    for i in range(number_of_cases):
        case = getCase()
        solution = solve(case)
        print(solution)

