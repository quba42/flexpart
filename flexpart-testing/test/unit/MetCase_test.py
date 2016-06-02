# -*- coding: utf-8 -*-
"""
Created on Wed Aug 12 00:34:57 2015

@author: morton
Don Morton
Boreal Scientific Computing LLC, Fairbanks, Alaska, USA
Don.Morton@borealscicomp.com
"""

import os
import shutil
import tempfile
import unittest

import distrotest.BasicTest as BasicTest
import distrotest.RunCase as RunCase
import distrotest.MetCase as MetCase

class test_MetCase(unittest.TestCase):

    @classmethod
    def setUpClass(cls):

        """Create temp directories and RunCase lists for testing"""
        
        # Create a RunCase list of two tests.  Note, will just use /tmp as
        # the case_dir and control_data_dir, since I'm not doing anything 
        # with it in this test
        cls._run_case_list = []

        description = 'Run Case 1'
        case_dir = '/tmp'
        control_data_dir = '/tmp'

        rc = RunCase.RunCase(descr=description, case_dir=case_dir,
                                 control_data_dir=control_data_dir)
        cls._run_case_list.append(rc)

        description = 'Run Case 2'
        case_dir = '/tmp'
        control_data_dir = '/tmp'

        rc = RunCase.RunCase(descr=description, case_dir=case_dir,
                                 control_data_dir=control_data_dir)
        cls._run_case_list.append(rc)
                           
            
        # Create a temporary test directory - won't do anything
        # in them, just want to test that routines find or not find them
        cls._metfile_dir = tempfile.mkdtemp(prefix='metfiledir_', dir='/tmp')                            
        

    def test_works_with_good_args(self):

        description = 'MetCase Test 1'
        metfile_dir = self._metfile_dir
        run_case_list = self._run_case_list

        mc = MetCase.MetCase(descr=description, metfile_dir=metfile_dir,
                             run_case_list=run_case_list)
                             
                                     
        self.assertEqual(mc.get_descr(), description)
        self.assertTrue(os.path.isdir(mc.get_metfile_dir()))
        
        # Get the BasicTests and insure they are correct instances
        rc_list = mc.get_run_case_list()
        self.assertTrue(isinstance(rc_list[0], RunCase.RunCase))
        self.assertTrue(isinstance(rc_list[1], RunCase.RunCase))        


    def test_raises_exception_if_no_metfile_dir_arg(self):

        description = 'MetCase Test'
        run_case_list = self._run_case_list

        with self.assertRaises(Exception):
            mc = MetCase.MetCase(descr=description, 
                                 run_case_list=run_case_list)

      
    def test_raises_exception_if_bad_metfile_dir(self):

        description = 'MetCase Test'
        metfile_dir = self._metfile_dir + 'zzz'   # Bad dir
        run_case_list = self._run_case_list

        with self.assertRaises(Exception):
            mc = MetCase.MetCase(descr=description, metfile_dir=metfile_dir,
                                 run_case_list=run_case_list)


    def test_returns_empty_run_case_list_if_no_run_case_list_arg(self):

        description = 'MetCase Test'
        metfile_dir = self._metfile_dir

        mc = MetCase.MetCase(descr=description, metfile_dir=metfile_dir)


        # Testing that returned list is empty (or False)
        self.assertFalse(mc.get_run_case_list() )



    @classmethod
    def tearDownClass(cls):
        """Remove the temp directories created in these tests"""

        if os.path.isdir(cls._metfile_dir):
            shutil.rmtree(cls._metfile_dir)


if __name__ == '__main__':
    unittest.main()
