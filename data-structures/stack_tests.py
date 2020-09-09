import unittest
from stack import Stack

class TestStringMethods(unittest.TestCase):

    def test_push_empty_stack(self):
        stack = Stack()
        self.assertEqual(stack.size, 0, "Stack should be empty")
        stack.push(1)
        self.assertEqual(stack.size, 1, "Stack should contain one element")

    def test_pop_empty_stack(self):
        stack = Stack()
        self.assertEqual(stack.size, 0, "Stack should be empty")
        self.assertEqual(stack.pop(), None, "Element should be None")

    def test_pop_non_empty_stack(self):
        stack = Stack()
        stack.push(1)
        stack.push(2)
        stack.push(3)
        stack.push(4)
        stack.push(5)

        self.assertEqual(stack.size, 5)

        # Verify LIFO: last in first out
        self.assertEqual(stack.pop(), 5)
        self.assertEquals(stack.size, 4)

        self.assertEqual(stack.pop(), 4)
        self.assertEquals(stack.size, 3)

        self.assertEqual(stack.pop(), 3)
        self.assertEquals(stack.size, 2)

        self.assertEqual(stack.pop(), 2)
        self.assertEquals(stack.size, 1)

        self.assertEqual(stack.pop(), 1)
        self.assertEqual(stack.size, 0)

    def test_stack_size_never_goes_below_zero(self):
        stack = Stack()
        self.assertEqual(stack.size, 0, "Stack should be empty")
        self.assertEqual(stack.pop(), None, "Element should be None")
        self.assertEqual(stack.pop(), None, "Element should be None")
        self.assertEqual(stack.pop(), None, "Element should be None")
        self.assertEqual(stack.size, 0, "Stack should be empty")

    def test_stack_with_chars(self):
        stack = Stack()
        stack.push('a')
        stack.push('b')
        stack.push('c')
        self.assertEquals(stack.size, 3)

        self.assertEquals(stack.pop(), 'c')
        self.assertEquals(stack.pop(), 'b')
        self.assertEquals(stack.pop(), 'a')
        self.assertEquals(stack.size, 0)

    def test_supports_multiple_types(self):
        stack = Stack()
        stack.push(1)
        stack.push('b')
        self.assertEquals(stack.size, 2)

        self.assertEquals(stack.pop(), 'b')
        self.assertEquals(stack.pop(), 1)
        self.assertEquals(stack.size, 0)



if __name__ == '__main__':
    unittest.main()
