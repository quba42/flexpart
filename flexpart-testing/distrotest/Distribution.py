# -*- coding: utf-8 -*-
"""
Created on Wed Aug 12 00:25:32 2015

@author: morton

Don Morton
Boreal Scientific Computing LLC, Fairbanks, Alaska, USA
Don.Morton@borealscicomp.com
"""

import os

import distrotest.BasicTest as BasicTest
import distrotest.RunCase as RunCase 
import distrotest.MetCase as MetCase

class Distribution(object):
    
    
    """
    Distribution() - container that stores properties of a particular FLEXPART
    distribution test, which will contain one or more MetCase objects, which
    will contain one or more RunCase objects, which will contain one or more 
    BasicTest objects.
    Container includes description, path to distribution, 
    path to par_mod.f90 file, and a list of MetCase objects to be applied. 
    """

    def __init__(self, descr=None, distro_path=None, 
                 parmod_path=None,
                 exec_name=None, met_case_list=None):

        """
        descr : Description string
        distro_path : path to source code directory
        parmod_path : (option) Path to par_mod.f90.  If no arg, uses the one
        in the distribution
        exec_name : name of executable
        met_case_list : (optional) List of MetCase objects.  Defaults to empty
        list.
        """
    
        if descr:
            self._description = descr 
    
        if distro_path:
            if os.path.isdir(distro_path):
                self._distro_path = distro_path
            else:
                raise Exception('Cannot find distro_path: ' + distro_path)
        else:
            raise Exception('distro_path not defined')   

        # If parmod_path is not defined, then par_mod.f90 is assumed to be in the
        # source distribution
        if parmod_path:
            if os.path.isfile(parmod_path):
                self._parmod_path = parmod_path
            else:
                raise Exception('Cannot find parmod_path: ' + parmod_path)
        else:
            self._parmod_path = self._distro_path + '/par_mod.f90'
            if not os.path.isfile(self._parmod_path):
                 raise Exception('Cannot find parmod_path in distribution: ' + self._parmod_path)               

            
        # Make sure all elements of met_case_list are indeed MetCase objects
        if met_case_list:
            for the_case in met_case_list:
                if not isinstance(the_case, (MetCase.MetCase)):
                    raise Exception('item in met_case_list not a MetCase obj')
            self._met_case_list = met_case_list
        else:
            self._met_case_list = []
            
            
        if exec_name:
            self._exec_name = exec_name
        else:
            raise Exception("No executable name provided...")
            

    def get_descr(self):
        return self._description

    def get_distro_path(self):
        return self._distro_path 
        
    def get_parmod_path(self):
        return self._parmod_path
        
    def get_exec_name(self):
        return self._exec_name

    def get_met_case_list(self):
        return self._met_case_list

