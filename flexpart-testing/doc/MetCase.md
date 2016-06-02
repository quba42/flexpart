
# MetCase


## Overview

This is a class that serves as a container for information associated with a particular set of met data.  In particular, it stores the location of the met data as well as a list of zero or more RunCases.  Each RunCase is an object that depicts a simulation that is meant to run from this particular met data.

An example of its use (from a unittest):

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
	





The pydoc representation:

	    class MetCase(__builtin__.object)
	     |  MetCase.py - container that stores properties of a particular set of met data,
	     |  which may be used for one or more RunCases.  Container includes  
	     |  description, directory of met files, and a list of 
	     |  RunCase objects to be applied.
	     |  
	     |  Methods defined here:
	     |  
	     |  __init__(self, descr=None, metfile_dir=None, run_case_list=None)
	     |      descr : Description string
	     |      metfile_dir : directory where met files are located
	     |      run_case_list : (optional) List of RunCase objects.  Defaults to empty.
	     |  
	     |  get_descr(self)
	     |  
	     |  get_metfile_dir(self)
	     |  
	     |  get_run_case_list(self)
	



## Issues

* None to report at this time
	




