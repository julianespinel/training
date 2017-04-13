# https://www.hackerrank.com/contests/world-codesprint-april/challenges/jumping-on-the-clouds

#!/bin/python3

numberOfClouds = int(input().strip())
clouds = [int(c_temp) for c_temp in input().strip().split(' ')]

def isThunderCloud(cloud):
    return (cloud == 1)

steps = 0
index = 0
while index < numberOfClouds:
    plusOne = index + 1
    plusTwo = index + 2
    if (plusOne < numberOfClouds) and (plusTwo < numberOfClouds):
        plusOneCloud = clouds[plusOne]
        plusTwoCloud = clouds[plusTwo]
        index = plusTwo if not isThunderCloud(plusTwoCloud) else plusOne
        steps += 1
    elif plusOne == numberOfClouds:
        index = numberOfClouds
    else: # If plusTwo >= numberOfClouds, then plusOneCloud = cn-1, so the next step is the end.
        steps += 1
        index = numberOfClouds
print(steps)
