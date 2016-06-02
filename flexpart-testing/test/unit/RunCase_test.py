# -*- coding: utf-8 -*-
"""
Created on Tue Aug 11 20:56:45 2015

@author: morton
"""

import os
import shutil
import tempfile
import unittest

import distrotest.BasicTest as BasicTest
import distrotest.RunCase as RunCase

class test_RunCase(unittest.TestCase):

    @classmethod
    def setUpClass(cls):

        """Create temp directories and BasicTest lists for testing"""
        
        # Create a BasicTest list of two tests
        cls._basic_test_list = []

        description = 'Test 1'
        test_type = 'blahblah'
        threshold = 1.0E-6
        bt = BasicTest.BasicTest(descr=description, test_type=test_type,
                                 threshold=threshold)
        cls._basic_test_list.append(bt)
        
        description = 'Test 2'
        test_type = 'blehbleh'
        threshold = 1.0E+6
        bt = BasicTest.BasicTest(descr=description, test_type=test_type,
                                 threshold=threshold)
        cls._basic_test_list.append(bt)                                 
            
        # Create a couple of temporary test directories - won't do antying
        # in them, just want to test that routines find or not find them
        cls._tmp_case_dir = tempfile.mkdtemp(prefix='casedir_', dir='/tmp')                            
        cls._tmp_control_data_dir = tempfile.mkdtemp(prefix='controldatadir_', 
                                                     dir='/tmp')      
        # Create an output dir in the test case dir
        os.mkdir(cls._tmp_case_dir + '/output')
        
        pass


    def test_works_with_good_args(self):

        description = 'Test Backwards, ECMWF, blah blah'
        case_dir = self._tmp_case_dir
        control_data_dir = self._tmp_control_data_dir
        test_list = self._basic_test_list

        rc = RunCase.RunCase(descr=description, case_dir=case_dir,
                             control_data_dir=control_data_dir,
                             test_list=test_list)
                             
                                     
        self.assertEqual(rc.get_descr(), description)
        self.assertTrue(os.path.isdir(rc.get_case_dir()))
        self.assertTrue(os.path.isdir(rc.get_control_data_dir()))
        
        # Get the BasicTests and insure they are correct instances
        bt_list = rc.get_test_list()
        self.assertTrue(isinstance(bt_list[0], BasicTest.BasicTest))
        
        
    def test_raises_exception_if_bad_case_dir(self):

        description = 'Test Backwards, ECMWF, blah blah'
        case_dir = self._tmp_case_dir + 'zzz'   # Bad dir
        control_data_dir = self._tmp_control_data_dir
        test_list = self._basic_test_list

        with self.assertRaises(Exception):
            rc = RunCase.RunCase(descr=description, case_dir=case_dir,
                                 control_data_dir=control_data_dir,
                                 test_list=test_list)


    def test_raises_exception_if_bad_control_data_dir(self):

        description = 'Test Backwards, ECMWF, blah blah'
        case_dir = self._tmp_case_dir
        control_data_dir = self._tmp_control_data_dir + 'zzz' # Bad dir
        test_list = self._basic_test_list

        with self.assertRaises(Exception):
            rc = RunCase.RunCase(descr=description, case_dir=case_dir,
                                 control_data_dir=control_data_dir,
                                 test_list=test_list)


    def test_defaults_correctly_if_no_control_data_dir(self):

        description = 'Test Backwards, ECMWF, blah blah'
        case_dir = self._tmp_case_dir
        test_list = self._basic_test_list

        rc = RunCase.RunCase(descr=description, case_dir=case_dir,
                             test_list=test_list)


        self.assertEqual(case_dir + '/output', rc.get_control_data_dir() )

    def test_returns_empty_basic_test_list_if_no_basic_test_list_arg(self):

        description = 'Test Backwards, ECMWF, blah blah'
        case_dir = self._tmp_case_dir
        control_data_dir = self._tmp_control_data_dir

        rc = RunCase.RunCase(descr=description, case_dir=case_dir,
                             control_data_dir=control_data_dir)

        # Testing that returned list is empty (or False)
        self.assertFalse(rc.get_test_list() )

    @classmethod
    def tearDownClass(cls):
        """Remove the temp directories created in these tests"""

        if os.path.isdir(cls._tmp_case_dir):
            shutil.rmtree(cls._tmp_case_dir)

        if os.path.isdir(cls._tmp_control_data_dir):
            shutil.rmtree(cls._tmp_control_data_dir)


if __name__ == '__main__':
    unittest.main()
