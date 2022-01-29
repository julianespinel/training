from collections import deque
from typing import Deque


class Node:

    def __init__(self, value, left=None, right=None):
        self.value = value
        self.left = left
        self.right = right


def pre_order(root: Node, elements: list) -> list:
    """
    Returns the elements of the tree traversed pre-order (NLR)
    :param root: The root of the tree
    :param elements: List to accumulate the result
    :return: List with the nodes of the tree in pre-order
    """
    elements.append(root.value)
    if root.left:
        elements = pre_order(root.left, elements)
    if root.right:
        elements = pre_order(root.right, elements)

    return elements


def in_order(root: Node, elements: list) -> list:
    """
    Returns the elements of the tree traversed in-order (LNR)
    :param root: The root of the tree
    :param elements: List to accumulate the result
    :return: List with the nodes of the tree in-order
    """
    if root.left:
        elements = in_order(root.left, elements)

    elements.append(root.value)

    if root.right:
        elements = in_order(root.right, elements)

    return elements


def post_order(root: Node, elements: list) -> list:
    """
    Returns the elements of the tree traversed in post-order (LRN)
    :param root: The root of the tree
    :param elements: List to accumulate the result
    :return: List with the nodes of the tree in post-order
    """
    if root.left:
        elements = post_order(root.left, elements)

    if root.right:
        elements = post_order(root.right, elements)

    elements.append(root.value)
    return elements


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
    queue = __append_children(queue, root)
    while queue:
        node = queue.popleft()
        elements.append(node.value)
        queue = __append_children(queue, node)

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
        queue = __append_children(queue, root)
    return __breadth_first(queue, elements)


def __append_children(queue, node):
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
        queue = __append_children(queue, node)
        return __breadth_first(queue, elements)
