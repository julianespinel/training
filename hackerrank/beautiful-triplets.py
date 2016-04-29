# https://www.hackerrank.com/contests/world-codesprint-april/challenges/beautiful-triplets

firstLine = [int(c_temp) for c_temp in input().strip().split(' ')]
numbers = [int(c_temp) for c_temp in input().strip().split(' ')]

numbersLength = firstLine[0]
d = firstLine[1]

numbersTable = {}
for number in numbers:
    numbersTable[number] = 1

def matchEquation(ai, ak, d):
    return ((ak - ai) / 2) == d

def getAj(ai, ak):
    return ((ak + ai) / 2)

def numberExistsInTable(number, table):
    value = table.get(number, 0)
    return (value != 0)

beautifulTriples = 0
i = 0
while i < numbersLength:
    ai = numbers[i]
    k = i + 1
    while k < numbersLength:
        ak = numbers[k]
        if matchEquation(ai, ak, d):
            aj = getAj(ai, ak)
            if numberExistsInTable(aj, numbersTable):
                beautifulTriples += 1
        k += 1
    i += 1

print(beautifulTriples)
