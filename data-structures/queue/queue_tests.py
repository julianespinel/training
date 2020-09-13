import unittest
from queue import Queue


class TestStringMethods(unittest.TestCase):

    def test_enqueue_to_empty_queue(self):
        queue = Queue()
        self.assertEqual(queue.size, 0, "Queue should be empty")
        queue.enqueue(1)
        self.assertEqual(queue.size, 1, "Queue should contain one element")

    def test_enqueue_non_empty_queue(self):
        queue = Queue()
        queue.enqueue(1)
        queue.enqueue(2)
        queue.enqueue(3)
        queue.enqueue(4)
        queue.enqueue(5)
        self.assertEqual(queue.size, 5, "Queue should contain one element")

    def test_dequeue_empty_queue(self):
        queue = Queue()
        self.assertEqual(queue.size, 0, "Queue should be empty")
        element = queue.dequeue()
        self.assertIsNone(element)
        self.assertEqual(queue.size, 0, "Queue should be empty")

        queue.dequeue()
        queue.dequeue()
        queue.dequeue()
        self.assertEqual(queue.size, 0, "Queue should be empty")

    def test_dequeue_non_empty_queue(self):
        queue = Queue()
        queue.enqueue(1)
        queue.enqueue(2)
        queue.enqueue(3)
        queue.enqueue(4)
        queue.enqueue(5)
        self.assertEqual(queue.size, 5, "Queue should contain one element")

        # Test FIFO (first in first out)
        one = queue.dequeue()
        self.assertEqual(one, 1, "Should be 1")
        self.assertEqual(queue.size, 5 - 1, "Queue size should decrease")

        two = queue.dequeue()
        self.assertEqual(two, 2, "Should be 2")
        self.assertEqual(queue.size, 5 - 2, "Queue size should decrease")

        three = queue.dequeue()
        self.assertEqual(three, 3, "Should be 3")
        self.assertEqual(queue.size, 5 - 3, "Queue size should decrease")

        four = queue.dequeue()
        self.assertEqual(four, 4, "Should be 4")
        self.assertEqual(queue.size, 5 - 4, "Queue size should decrease")

        five = queue.dequeue()
        self.assertEqual(five, 5, "Should be 5")
        self.assertEqual(queue.size, 5 - 5, "Queue size should decrease")

        none = queue.dequeue()
        self.assertIsNone(none, "Queue should be empty")
        self.assertEqual(queue.size, 0, "Queue should be empty")


if __name__ == '__main__':
    unittest.main()
