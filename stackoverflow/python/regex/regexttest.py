import re

REGEX = 'Linear regression is done. value: [+-]?([0-9]+\.?[0-9]*|\.[0-9]+)'

if __name__ == '__main__':
    numbers_in_text = []
    with open('sample.txt', 'r') as file:
        for line in file:
            numbers_in_line = re.findall(REGEX, line)
            numbers_in_text.extend(numbers_in_line)
    
    print(numbers_in_text)
    assert 6 == len(numbers_in_text), 'It is not reading all the numbers'


"""
Source: https://stackoverflow.com/questions/72983758/how-to-use-regex-to-search-for-some-numbers-in-python/72984146#72984146
"""
