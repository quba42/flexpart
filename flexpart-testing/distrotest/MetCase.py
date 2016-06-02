# -*- coding: utf-8 -*-
"""
Created on Wed Aug 12 00:25:32 2015

@author: morton
Don Morton
Boreal Scientific Computing LLC, Fairbanks, Alaska, USA
Don.Morton@borealscicomp.com

@contributors
Christian Maurer
Delia Arnold
ZAMG, Vienna, Austria
christian.maurer@zamg.ac.at
delia.arnold-arias@zamg.ac.at

"""

import os

import distrotest.BasicTest as BasicTest
import distrotest.RunCase as RunCase 

class MetCase(object):
    
    
    """
    MetCase.py - container that stores properties of a particular set of met data,
    which may be used for one or more RunCases.  Container includes  
    description, directory of met files, and a list of 
    RunCase objects to be applied.  
    """

    def __init__(self, descr=None, metfile_dir=None, 
                 metnestfile_dir=None, run_case_list=None):

        """
        descr : Description string
        metfile_dir : directory where met files are located
        run_case_list : (optional) List of RunCase objects.  Defaults to empty.
        metnestfile_dir : directory where possible nested met files are located
        """
    
        if descr:
            self._description = descr 
    
        if metfile_dir:
            if os.path.isdir(metfile_dir):
                self._metfile_dir = metfile_dir
            else:
                raise Exception('Cannot find metfile_dir: ' + metfile_dir)
        else:
            raise Exception('metfile_dir not defined')   
       
        if metnestfile_dir:
            if os.path.isdir(metnestfile_dir):
                self._metnestfile_dir = metnestfile_dir
            else:
                raise Exception('Cannot find a metnestfile_dir: ' + metnestfile_dir)
        # Make sure all elements of test_list are indeed BasicTest objects
        if run_case_list:
            for the_case in run_case_list:
                if not isinstance(the_case, (RunCase.RunCase)):
                    raise ValueError('item in run_case_list not a RunCase obj')
            self._run_case_list = run_case_list
        else:
            self._run_case_list = []
            

    def get_descr(self):
        return self._description

    def get_metfile_dir(self):
        return self._metfile_dir 

    def get_metnestfile_dir(self):
        return self._metnestfile_dir
        

    def get_run_case_list(self):
        return self._run_case_list

