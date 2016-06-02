
import unittest

import distrotest.BasicTest as BasicTest

class test_BasicTest(unittest.TestCase):

    def test_works_with_good_args(self):

        description = 'Test RMSE'
        test_type = 'full-array'
        threshold = 1.0E-6

        bt = BasicTest.BasicTest(descr=description, test_type=test_type,
                                 threshold=threshold)

        self.assertEqual(bt.get_descr(), description)
        self.assertEqual(bt.get_test_type(), test_type)
        self.assertEqual(bt.get_threshold(), threshold)

    def test_raises_exception_if_no_description(self):

        test_type = 'full-array'
        threshold = 1.0E-6

        with self.assertRaises(Exception):
            bt = BasicTest.BasicTest(test_type=test_type,
                                     threshold=threshold)

    def test_raises_exception_if_no_test_type(self):

        description = 'Test RMSE'
        threshold = 1.0E-6

        with self.assertRaises(Exception):
            bt = BasicTest.BasicTest(descr=description,
                                     threshold=threshold)

    def test_raises_exception_if_no_threshold(self):

        description = 'Test RMSE'
        test_type = 'full-array'

        with self.assertRaises(Exception):
            bt = BasicTest.BasicTest(descr=description,
                                     test_type=test_type)

    def test_raises_exception_if_non_numeric_threshold(self):

        description = 'Test RMSE'
        test_type = 'full-array'
        threshold = 'abc'

        with self.assertRaises(Exception):
            bt = BasicTest.BasicTest(descr=description,
                                     test_type=test_type,
                                     threshold=threshold)








if __name__ == '__main__':
    unittest.main()
