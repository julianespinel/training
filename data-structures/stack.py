class Node:

    def __init__(self, value):
        self.value = value
        self.previous = None
        self.next = None

    def prepend(self, node):
        self.previous = node

    def append(self, node):
        self.next = node


class Stack:

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
