
# RunCase


## Overview

This is a class that serves as a container for properties of a particular simulation, including description, directory of case files, and a list of BasicTest objects to be applied.  A RunCase corresponds to a a specific simulation (e.g. forward or backward) with specific set of inputs, domain, species, and other parameters.


An example of its use (from a unittest):

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


An example of its use in a testing environment:


	            list_all_cases.append(case_rundir) # add the case_dir into a list for cleaning
	            control_data_dir = os.path.realpath(the_run_case.get_control_data_dir())
	
	            basic_test_list = the_run_case.get_test_list()
	            run_success = False
	
	            print; print '============================'
	            print 'Case Test ' + str(the_descr) + '...'
	            print 'Case template directory: ' + case_dir
	            print 'Case run directory: ' + case_rundir
	            print 'Control data directory: ' + control_data_dir
	            print 'Met file dir: ' + the_metfile_dir
	            print 'Executable: ' + flexpart_executable
	            print 'Number of basic tests: ' + str(len(basic_test_list))
	            print '============================'; print
	
	
	            # Create the case object
	            case_obj = FlexpartCase.FlexpartCase(
	                             src_dir=case_dir,
	                             dest_dir=case_rundir,
	                             met_dir=the_metfile_dir,
	                             flexpart_exe=flexpart_executable
	                                                 )
	
	            # Run the case
	            run_val = case_obj.run()
	
	            # Test for success
	            run_success = case_obj.success()
	            print 'run_success: ' + str(run_success)
	            print 'Execution time: %7.2E seconds' % \
	                  (case_obj.execution_time_seconds())
	            if not run_success:
	                all_success = False # to know wheter any of the tests failed
	                print 'run test failed'
	                print 'The test distribution is located in: ' + case_rundir
	                print 'The FLEXPART executable being used is: ' + flexpart_executable
	                print 'You should try to go there and see if you can find error by running by hand'
	                print 'There is a file named stdout.txt in there which might give a clue'
	
	
	
The pydoc representation:


	    class RunCase(__builtin__.object)
	     |  RunCase.py - container that stores properties of a particular run, including 
	     |  description, directory of case files, and a list of BasicTest objects to be 
	     |  applied.  A Case corresponds to a specific simulation (e.g. forward or backward,
	     |  specific set of inputs, specific domain, species, etc.)
	     |  
	     |  Methods defined here:
	     |  
	     |  __init__(self, descr=None, case_dir=None, control_data_dir=None, test_list=None)
	     |      descr : (optional) Description string
	     |      case_dir : directory where case configuration files is located
	     |      control_data_dir : (optional).  Alternate location of control output
	     |      data.  If not defined, then defaults to <case_dir>/output
	     |      test_list : (optional) List of BasicTest objects.  Defaults to None.
	     |  
	     |  get_case_dir(self)
	     |  
	     |  get_control_data_dir(self)
	     |  
	     |  get_descr(self)
	     |  
	     |  get_test_list(self)
	



## Issues

* None to report at this time
	




