class Node:
    """Represents a node in a linked list"""

    def __init__(self, value):
        self.value = value
        self.previous = None
        self.next = None

    def prepend(self, node):
        self.previous = node

    def append(self, node):
        self.next = node


class Stack:
    """
    Represents a stack.
    This implementation of the stack is based on a linked list.
    Unit tests in the file: stack_tests.py

    Complexity analysis of the stack operations:

    Time:
    * Push: O(1), add an element at the top of the stack
    * Pop: O(1), remove an element from the top of the stack
    * Search: O(n), traverse the stack until you find the required element.
                    In the worst case scenario you need to traverse the whole stack, therefore the time
                    complexity is O(n)

    Space:
    The space required to hold the stack in memory is O(n).
    """

    def __init__(self):
        self.size = 0
        self.current = None

    def push(self, value):
        new_node = Node(value)
        if self.current:
            self.current, old_node = new_node, self.current
            self.current.append(old_node)
        else:
            self.current = new_node
        self.size += 1

    def pop(self):
        if self.size == 0:
            return None

        result = self.current
        self.current = result.next
        self.size -= 1
        return result.value
