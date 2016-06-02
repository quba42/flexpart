# -*- coding: utf-8 -*-
"""
Created on Fri Sep  4 19:18:40 2015

@author: morton

Don Morton
Boreal Scientific Computing LLC, Fairbanks, Alaska, USA
Don.Morton@borealscicomp.com
http://www.borealscicomp.com/
"""

import os
import shutil
import tempfile
import unittest

import flextest.OutputCompare as OutputCompare

class test_OutputCompare(unittest.TestCase):
    

    @classmethod
    def setUpClass(cls):
        
        """Create temp directories for output and control"""
        
        cls._output_data_dir = tempfile.mkdtemp(prefix='outdata_', 
                                                dir='/tmp')
        cls._control_output_data_dir = tempfile.mkdtemp(prefix='controloutdata_', 
                                                dir='/tmp')

        cls._oc = OutputCompare.OutputCompare(output_dir=cls._output_data_dir,
                                         control_output_dir=cls._control_output_data_dir)      


    def test_good_init(self):
        
        oc = OutputCompare.OutputCompare(output_dir=self._output_data_dir,
                                         control_output_dir = self._control_output_data_dir)
    
        self.assertTrue(isinstance(oc, OutputCompare.OutputCompare))
   
    def test_init_raises_exception_with_bad_output_dir(self):
        
        with self.assertRaises(Exception):
            oc = OutputCompare.OutputCompare(
                    output_dir=self._output_data_dir + 'zzz',
                    control_output_dir = self._control_output_data_dir)           
  
    
    def test_init_raises_exception_with_missing_output_dir(self):
        with self.assertRaises(Exception):
            oc = OutputCompare.OutputCompare(
                    control_output_dir = self._control_output_data_dir) 
    
    def test_init_raises_exception_with_bad_control_output_dir(self):

        with self.assertRaises(Exception):
            oc = OutputCompare.OutputCompare(
                    output_dir=self._output_data_dir,
                    control_output_dir = self._control_output_data_dir + 'zzz')    

    
    def test_init_raises_exception_with_missing_control_output_dir(self):
  
        with self.assertRaises(Exception):
            oc = OutputCompare.OutputCompare(
                    output_dir=self._output_data_dir)


    def test_query_test_types_returns_list(self):
        
        tt = self._oc.query_test_types()
        
        self.assertTrue(isinstance(tt, list))
        
    def test_calculate_test_minus_control_raises_exception_on_bad_test_type(self):

        with self.assertRaises(Exception):
            err = self._oc.calculate_test_minus_control('xxzzxxzzxxzz')

    @classmethod
    def tearDownClass(cls):
        """Remove the temp directories created in these tests"""

        if os.path.isdir(cls._output_data_dir):
            shutil.rmtree(cls._output_data_dir)

        if os.path.isdir(cls._control_output_data_dir):
            shutil.rmtree(cls._control_output_data_dir)


    

if __name__ == '__main__':
    unittest.main()
