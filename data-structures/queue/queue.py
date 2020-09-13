class Node:
    """Represents a node in a linked list"""

    def __init__(self, value):
        self.value = value
        self.next = None

    def append(self, node):
        self.next = node


class Queue:
    """
    Represents a queue.
    This implementation of the queue is based on a linked list.
    Unit tests in the file: queue_tests.py

    Complexity analysis of the queue operations:

    Time:
    * Enqueue: O(1), add an element at the end of the queue
    * Dequeue: O(1), remove the first element of the queue
    * Search: O(n), traverse the queue until you find the required element.
                    In the worst case scenario you need to traverse the whole queue, therefore the time
                    complexity is O(n)

    Space:
    The space required to hold the queue in memory is O(n).
    """

    def __init__(self):
        self.size = 0
        self.first = None
        self.last = None

    def __is_empty(self):
        return self.first is None

    def enqueue(self, value):
        new_node = Node(value)
        if self.__is_empty():
            self.first = new_node
            self.last = new_node
        else:
            self.last.append(new_node)
            self.last = new_node
        self.size += 1

    def dequeue(self):
        if self.__is_empty():
            return None
        current_first = self.first
        self.first = current_first.next
        self.size -= 1
        return current_first.value
