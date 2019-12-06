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


def abc(remainingMoves, desiredMoves):
    difference = remainingMoves - desiredMoves
    return 0 if difference >= 0 else abs(difference)


def move(index, targetIndex, string):
    print(f"index: {index}, ti: {targetIndex}, string: {string}")
    return string[:targetIndex] + string[index] + string[targetIndex:index] + string[index + 1:]


def solve(case):
    nextZeroIndex = 0
    remainingMoves = case.moves
    string = case.string
    for index in range(case.size):
        if  remainingMoves == 0:
            break
        if index == 0:
            continue
        char = string[index]
        previousChar = string[index - 1]
        needToMove = char == ZERO and previousChar != ZERO
        if needToMove:
            desiredMoves = index - nextZeroIndex
            allowedMoves = getAllowedMoves(remainingMoves, desiredMoves)
            # Calculate targetIndex using nextZeroIndex and allowedMoves
            targetIndex = nextZeroIndex + abc(remainingMoves, desiredMoves)
            print(f"nzi: {nextZeroIndex}, remainingmoves: {remainingMoves}, desiredmoves: {desiredMoves}")
            string = move(index, targetIndex, string)
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

