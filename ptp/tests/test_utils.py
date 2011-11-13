
import unittest

from base.utils import topological_ordering, multiset

class TestUtils(unittest.TestCase):


    def test_topological_ordering(self):
        self.assertEqual(topological_ordering([], lambda a, b: a < b), [])
        self.assertEqual(topological_ordering([3,2,1,1,0,2], lambda a, b: a < b), [0,1,1,2,2,3])
        self.assertEqual(topological_ordering([1], lambda a, b: a <= b), None)

        s1 = set([0, 1])
        s2 = set([0, 1, 2])
        s3 = set()
        s4 = set([1,2])
        s5 = set([0])
        s6 = set([3])
        s7 = set([0, 1])
        self.assertEqual(topological_ordering([s1,s2,s3,s4,s5,s6,s7], lambda a, b: a.issubset(b) and a != b),
                         [s3,s4,s5,s6,s1,s7,s2])

    def test_multiset(self):
        self.assertEqual(multiset([]), {})
        self.assertEqual(multiset([1,2,3,4,3,3,1]), {1:2, 2:1, 3:3, 4:1})
if __name__ == "__main__":
    unittest.main()
