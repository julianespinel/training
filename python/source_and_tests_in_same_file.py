import unittest


class Solution:

    def add(self, a: int, b: int) -> int:
        return a + b


class TestSolution(unittest.TestCase):

	@classmethod
	def setUpClass(cls):
		pass

	def setUp(self):
		self.solution = Solution()

	def tearDown(self):
		pass

	@classmethod
	def tearDownClass(cls):
		pass

	def test_add_identity(self):
		# Arrange
		number = 123
		identity = 0
		# Act
		result = self.solution.add(number, identity)
		# Assert
		self.assertEqual(result, number)

	def test_add_one_plus_two_equals_three(self):
		self.assertEqual(self.solution.add(1, 2), 3)

	def test_add_negative_numbers(self):
		self.assertEqual(self.solution.add(-1, -2), -3)


if __name__ == '__main__':
    unittest.main()
