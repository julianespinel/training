class reversor:
    def __init__(self, value):
        self.value = value

    def __eq__(self, other):
        return self.value == other.value

    def __lt__(self, other):
        """
        Inverted it to be able to sort in descending order.
        """
        return self.value >= other.value


tuples = [(3, 'x'), (2, 'y'), (1, 'a'), (1, 'z')]

tuples.sort(key=lambda x: (x[0], x[1]))
assert tuples == [(1, 'a'), (1, 'z'), (2, 'y'),(3, 'x')], "Error 1: 0 asc, 1 asc"

tuples.sort(key=lambda x: (x[0], reversor(x[1])))
assert tuples == [(1, 'z'), (1, 'a'), (2, 'y'),(3, 'x')], "Error 2: 0 asc, 1 desc"

# The following approach works for a single char string.
tuples.sort(key=lambda x: (x[0], -ord(x[1])))
assert tuples == [(1, 'z'), (1, 'a'), (2, 'y'), (3, 'x')], "Error 3: 0 asc, 1 desc"
