# -*- coding: utf-8 -*-
"""
Created on Tue Aug 11 19:35:59 2015

@author: morton
"""


import os

import distrotest.BasicTest as BasicTest

class RunCase(object):
    
    
    """
    RunCase.py - container that stores properties of a particular run, including 
    description, directory of case files, and a list of BasicTest objects to be 
    applied.  A Case corresponds to a specific simulation (e.g. forward or backward,
    specific set of inputs, specific domain, species, etc.)
    """

    def __init__(self, descr=None, case_dir=None, 
                 control_data_dir=None, test_list=None):

        """
        descr : (optional) Description string
        case_dir : directory where case configuration files is located
        control_data_dir : (optional).  Alternate location of control output
        data.  If not defined, then defaults to <case_dir>/output
        test_list : (optional) List of BasicTest objects.  Defaults to None.
        """
    
        if descr:
            self._description = descr 
    
        if case_dir:
            if os.path.isdir(case_dir):
                self._case_dir = case_dir
            else:
                raise Exception('Cannot find case_dir: ' + case_dir)
        else:
            raise Exception('case_dir not defined')   

    
        if control_data_dir:
            self._control_data_dir = control_data_dir
        else:
            self._control_data_dir = self._case_dir + '/output'
                
        # Default to the "output" dir of the case directory
        #print control_data_dir, self._control_data_dir        
        if not os.path.isdir(self._control_data_dir):
            raise Exception('control_data_dir not valid: ' + self._control_data_dir)   
            
        # Make sure all elements of test_list are indeed BasicTest objects
        if test_list:
            for the_test in test_list:
                if not isinstance(the_test, (BasicTest.BasicTest)):
                    raise Exception('item in test_list not a BasicTest obj')
            self._test_list = test_list
        else:
            self._test_list = []
            

    def get_descr(self):
        return self._description

    def get_case_dir(self):
        return self._case_dir 
        
    def get_control_data_dir(self):
        return self._control_data_dir


    def get_test_list(self):
        return self._test_list





         
        
        

    
    
    