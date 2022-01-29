from collections import deque
from typing import Deque


class Node:

    def __init__(self, value, left=None, right=None):
        self.value = value
        self.left = left
        self.right = right


def pre_order_recursive(root: Node) -> list:
    """
    Returns the elements of the tree traversed pre-order (NLR)
    :param root: The root of the tree
    :return: List with the nodes of the tree in pre-order
    """
    if not root:
        return []
    elements = [root.value]
    elements.extend(pre_order_recursive(root.left))
    elements.extend(pre_order_recursive(root.right))
    return elements


def pre_order_iterative(root: Node) -> list:
    """
    Returns the elements of the tree traversed pre-order (NLR)
    :param root: The root of the tree
    :return: List with the nodes of the tree in pre-order
    """
    if not root:
        return []

    elements = [root.value]
    stack = deque()
    stack = __append_right_left(stack, root)
    while stack:
        node = stack.pop()
        elements.append(node.value)
        stack = __append_right_left(stack, node)

    return elements


def in_order(root: Node) -> list:
    """
    Returns the elements of the tree traversed in-order (LNR)
    :param root: The root of the tree
    :return: List with the nodes of the tree in-order
    """
    elements = []
    if not root:
        return elements

    elements.extend(in_order(root.left))
    elements.append(root.value)
    elements.extend(in_order(root.right))
    return elements


def post_order(root: Node) -> list:
    """
    Returns the elements of the tree traversed in post-order (LRN)
    :param root: The root of the tree
    :return: List with the nodes of the tree in post-order
    """
    elements = []
    if not root:
        return elements

    elements.extend(post_order(root.left))
    elements.extend(post_order(root.right))
    elements.append(root.value)
    return elements


def breadth_first_recursive(root: Node) -> list:
    """
    Returns the elements of the tree traversed in breadth-first order.
    The implementation of this algorithm is recursive
    :param root: The root of the tree
    :return: List with the nodes of the tree in breadth-first order
    """
    elements = []
    queue = deque()
    if root.value:
        elements.append(root.value)
        queue = __append_left_right(queue, root)
    return __breadth_first(queue, elements)


def breadth_first_iterative(root: Node) -> list:
    """
    Returns the elements of the tree traversed in breadth-first order.
    The implementation of this algorithm is iterative
    :param root: The root of the tree
    :return: List with the nodes of the tree in breadth-first order
    """
    if not root:
        return []

    elements = [root.value]

    queue = deque()
    queue = __append_left_right(queue, root)
    while queue:
        node = queue.popleft()
        elements.append(node.value)
        queue = __append_left_right(queue, node)

    return elements


# -----------------------------------------------------------------------------
# Private functions
# -----------------------------------------------------------------------------

def __append_right_left(stack: Deque[Node], root: Node) -> Deque[Node]:
    if root.right:
        stack.append(root.right)
    if root.left:
        stack.append(root.left)
    return stack


def __append_left_right(queue: Deque[Node], node: Node) -> Deque[Node]:
    """
    Appends node children to the queue and returns the queue.
    :param queue: The queue to append the node's children
    :param node: Parent of the children
    :return: queue if the children of the node
    """
    if node.left:
        queue.append(node.left)
    if node.right:
        queue.append(node.right)
    return queue


def __breadth_first(queue: Deque[Node], elements: list) -> list:
    if len(queue) == 0:
        return elements

    node = queue.popleft()
    if node.value:
        elements.append(node.value)
        queue = __append_left_right(queue, node)
        return __breadth_first(queue, elements)
