# -*- coding: utf-8 -*-
"""
Created on Fri Sep  4 18:19:06 2015

@author: morton

Don Morton
Boreal Scientific Computing LLC, Fairbanks, Alaska, USA
Don.Morton@borealscicomp.com
http://www.borealscicomp.com/
"""

import os

import flextest.flexread.FlexpartOutput as FlexpartOutput
import flextest.FlexpartErrors as FlexpartErrors


class OutputCompare(object):
    


    _VALID_TESTS = [
                    'mother_all_vars_rmse',
                    'nest_all_vars_rmse',
                    'mother_all_vars_maxabserr',
                    'nest_all_vars_maxabserr'
                    ]
                

    def __init__(self, output_dir=None, control_output_dir=None):
        
        if output_dir:
            if os.path.isdir(output_dir):
                self._output_dir = output_dir
            else:
                raise Exception('Unable to find output_dir: ' + output_dir)                
        else:
            raise Exception('No output_dir defined')
            
        if control_output_dir:
            if os.path.isdir(control_output_dir):
                self._control_output_dir = control_output_dir
            else:
                raise Exception('Unable to find control_output_dir: ' + control_output_dir)                
        else:
            raise Exception('No control_output_dir defined')



                
    def query_test_types(self):
        
        """ Return list of the valid test types """
        return self._VALID_TESTS
        
        
    def calculate_test_minus_control(self, test_type=None):
        
    
        if test_type not in self._VALID_TESTS:
            raise Exception('Invalid test_type: ' + str(test_type))
            
        else:
    
            if test_type in ['nest_all_vars_rmse', 'nest_all_vars_maxabserr']:
                nest = True
            else:
                nest = False
    
    
        # Create FlexpartOutput objects
        control_output = FlexpartOutput.FlexpartOutput(
                           output_dir=self._control_output_dir,
                           nest=nest)
        test_output = FlexpartOutput.FlexpartOutput(
                           output_dir=self._output_dir,
                           nest=nest)
        # Create FlexpartError object    
        error_object = FlexpartErrors.FlexpartErrors(control=control_output,
                                                     test=test_output)
            
    
        # Select and perform the test, returning the error
    
        if test_type in ['mother_all_vars_rmse', 'nest_all_vars_rmse']:
            err_val = error_object.rmse()
        elif test_type in ['mother_all_vars_maxabserr', 'nest_all_vars_maxabserr']:
            err_val = error_object.max_absolute_error()
        else:
            err_val = None
            
        return err_val
    
    