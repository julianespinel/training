import unittest

import tree
from tree import Node


def get_test_tree():
    a = Node('a')
    c = Node('c')
    e = Node('e')
    d = Node('d', c, e)
    b = Node('b', a, d)

    h = Node('h')
    i = Node('i', h)
    g = Node('g', None, i)

    root = Node('f', b, g)
    return root


class TestTree(unittest.TestCase):

    def test_traverse_pre_order(self):
        root = get_test_tree()
        recursive_result = tree.pre_order_recursive(root, [])
        self.assertEqual(recursive_result, ['f', 'b', 'a', 'd', 'c', 'e', 'g', 'i', 'h'])

        iterative_result = tree.pre_order_iterative(root)
        self.assertEqual(iterative_result, ['f', 'b', 'a', 'd', 'c', 'e', 'g', 'i', 'h'])

    def test_traverse_in_order(self):
        root = get_test_tree()
        result = tree.in_order(root, [])
        self.assertEqual(result, ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'])

    def test_traverse_post_order(self):
        root = get_test_tree()
        result = tree.post_order(root, [])
        self.assertEqual(result, ['a', 'c', 'e', 'd', 'b', 'h', 'i', 'g', 'f'])

    def test_traverse_breadth_first(self):
        root = get_test_tree()
        recursive_result = tree.breadth_first_recursive(root)
        self.assertEqual(recursive_result, ['f', 'b', 'g', 'a', 'd', 'i', 'c', 'e', 'h'])

        iterative_result = tree.breadth_first_iterative(root)
        self.assertEqual(iterative_result, ['f', 'b', 'g', 'a', 'd', 'i', 'c', 'e', 'h'])
