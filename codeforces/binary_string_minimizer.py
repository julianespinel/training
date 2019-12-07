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
    string = list(input())
    return Case(size, moves, string)


def getAllowedMoves(remainingMoves, desiredMoves):
    hasEnoughMoves = (remainingMoves - desiredMoves >= 0)
    return desiredMoves if hasEnoughMoves else remainingMoves


def swap(index, nextZeroIndex, string):
    string[index], string[nextZeroIndex] = string[nextZeroIndex], string[index]
    return string


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
            targetIndex = nextZeroIndex + abs(desiredMoves - allowedMoves)
            string = swap(index, targetIndex, string)
            remainingMoves -= allowedMoves
            nextZeroIndex += 1
        elif char == ZERO:
            nextZeroIndex += 1
    return string


if __name__ == "__main__":
    number_of_cases = int(input())
    for i in range(number_of_cases):
        case = getCase()
        solvedList = solve(case)
        solution = "".join(solvedList)
        print(solution)

