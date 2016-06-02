
# BasicTest


## Overview

This is a class that serves as a container for properties of a particular test, including description, type of test, and the threshold that determines pass/fail of the test.   



An example of its use (from a unittest):

	        description = 'Test RMSE'
	        test_type = 'full-array'
	        threshold = 1.0E-6
	
	        bt = BasicTest.BasicTest(descr=description, test_type=test_type,
	                                 threshold=threshold)
	
	        self.assertEqual(bt.get_descr(), description)
	        self.assertEqual(bt.get_test_type(), test_type)
	        self.assertEqual(bt.get_threshold(), threshold)

An example of it being used in a testing environment to compare a recent run with a control run:

	            if run_success:
	
	
	                output_compare = OutputCompare.OutputCompare(output_dir=case_rundir + '/output',
	                                                control_output_dir=control_data_dir)
	
	                #print output_compare.query_test_types()
	
	
	                for the_basic_test in basic_test_list:
	
	                    the_descr = the_basic_test.get_descr()
	                    test_type = the_basic_test.get_test_type()
	                    threshold = the_basic_test.get_threshold()
	
	
	                    print; print '-----------------------'
	                    print 'Basic Test'
	                    print 'Description: ' + the_descr
	                    print 'Test type: ' + test_type
	                    print 'Threshold: %7.1E' % (threshold)
	
	
	                    the_error = output_compare.calculate_test_minus_control(test_type=test_type)
	
	                    print 'Test performed.  Error = %7.1E' % (the_error)
	                    if the_error > threshold:
	                        all_success = False
	                        print 'Test failed...'
	                        print '    Test data is in: ' + case_rundir + '/output'
	                        print '    Control data is in: ' + control_data_dir
	                    else:
	                        print 'Test passed'
	



The pydoc representation:

	    class BasicTest(__builtin__.object)
	     |  Container for BasicTest properties
	     |  
	     |  Methods defined here:
	     |  
	     |  __init__(self, descr=None, test_type=None, threshold=None)
	     |  
	     |  get_descr(self)
	     |  
	     |  get_test_type(self)
	     |  
	     |  get_threshold(self)
	


## Issues

* None to report at this time.  Again, this is just a container.
	




