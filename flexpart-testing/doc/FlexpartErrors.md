
# FlexpartErrors


## Overview

This is a class intended to provide comparisons between two FLEXPART outputs, presumably a control and a test.  It is assumed that both outputs come from the same problem domain so that, essentially, output structure is identical.  Users may compare specified horizontal slices and volumes or sub-volumes, at a single timestep or over a time series.  Users may also select deposition fields, releases, species and ageclass.  The routines necessarily have a number of parameter options, but one can use these with minimal options by accepting default parameters.  The following is a simple example of usage.  It starts with the creation of *FlexpartOutput* objects for the control and test outputs, and then creating a *FlexpartErrors* object from these.  The object can then be used to perform a large number of comparisons between the two datasets.

Before running these sample Python programs, it's necessary to add the flextest directory to your PYTHONPATH - for example

	$ export PYTHONPATH=~/git/ctbto/flexpart-testing/flextest


This first sample code shows output comparisons using a test and control set of FLEXPART outputs:

		import FlexpartErrors
		import flexread.FlexpartOutput as FlexpartOutput
		
		# Location of the control and test Flexpart output directories
		CONTROL = 'output_ecmwf'
		TEST = 'output_ecmwf_unified'
		
		# Create flexpart output objects for control and test
		control_obj = FlexpartOutput.FlexpartOutput(output_dir=CONTROL)
		test_obj = FlexpartOutput.FlexpartOutput(output_dir=TEST)
		
		# Create an errors object for comparing the two output objects
		errors_obj = FlexpartErrors.FlexpartErrors(control=control_obj,
		                                           test=test_obj)
		
		# Calculate the RMSE for the default slice - horizontal slice at level 1,
		# for the approximately "midway" timestep
		RMSE = errors_obj.rmse()
		print 'RMSE for level 1: ' + str(RMSE)
		
		# Calculate max error for the full timeseries of the full volume
		maxerr = errors_obj.max_error(volume=True, timeseries=True)
		print 'Max error for full volume, full timeseries: ' + str(maxerr)
	

	
The FlexpartErrors class expects the following arguments

* *control* - a *FlexpartOutput* object containing data from a control output
* *test* - a *FlexpartOutput* object containing data from a test output

>Please note that a *FlexpartOutput* object is not able to store data for both a mother domain and a nest.  In order to compare nests, it would be necessary to create additional  *FlexpartOutput* objects with the **nest=True** flag

It is important to note that deposition grids and concentration grids are compared separately.  This next example is lengthy, but tries to illustrate ways that various grids can be extracted and compared.

	import FlexpartErrors
	import flexread.FlexpartOutput as FlexpartOutput
	
	# Location of the control and test Flexpart output directories
	CONTROL = 'fp9.02/rundir/output'
	TEST = 'fpwo8/rundir/output'
	
	# Create flexpart output object for the control case
	control_obj = FlexpartOutput.FlexpartOutput(output_dir=CONTROL)
	
	# Demonstration of extracting a default horizontal slice
	hslice = control_obj.get_horiz_slice()
	print 'hslice: ', hslice
	print hslice.shape
	
	# Demonstration of extracting a default dry deposition slice
	dslice = control_obj.get_deposition(depo_type='dry')
	print 'dslice: ', dslice
	print dslice.shape
	print dslice.sum()
	
	# Demonstration of extracting the full timeseries of dry deposition slices
	dtslice = control_obj.get_deposition_timeseries(depo_type='dry')
	print 'dtslice: ', dtslice
	print dtslice.shape
	print dtslice.sum()
	
	# Demonstration of extracting a default wet deposition slice
	wslice = control_obj.get_deposition(depo_type='wet')
	print 'wslice: ', wslice
	print wslice.shape
	print wslice.sum()
	
	
	# Create flexpart output object for the test case
	test_obj = FlexpartOutput.FlexpartOutput(output_dir=TEST)
	
	# Demonstration of extracting a default dry deposition slice
	dslice = test_obj.get_deposition(depo_type='dry')
	print 'dslice: ', dslice
	print dslice.shape
	print dslice.sum()
	
	# Demonstration of extracting a default wet deposition slice
	wslice = test_obj.get_deposition(depo_type='wet')
	print 'wslice: ', wslice
	print wslice.shape
	print wslice.sum()
	
	# Create an errors object for comparing the two output objects
	errors_obj = FlexpartErrors.FlexpartErrors(control=control_obj,
	                                           test=test_obj)
	
	
	# Note that errors_obj (as well as control_obj and test_obj, above) contain
	# EVERYTHING - concentrations, wet/dry depositions, etc.
	
	
	# Looking at diff grid for dry depo
	diff_grid = errors_obj.get_diff_grid(dry=True)
	print 'dry depo diff grid...'
	print diff_grid
	print diff_grid.shape
	print 'sum: ', diff_grid.sum()
	
	# Looking at diff grid for wet depo
	diff_grid = errors_obj.get_diff_grid(wet=True)
	print 'wet depo diff grid...'
	print diff_grid
	print diff_grid.shape
	print 'sum: ', diff_grid.sum()
	
	# Calculate the RMSE for the default conc slice - horizontal slice at level 1,
	# for the approximately "midway" timestep
	RMSE = errors_obj.rmse()
	print 'RMSE for level 1: ' + str(RMSE)
	
	# Calculate max error for the full timeseries of the full concentration volume
	maxerr = errors_obj.max_error(volume=True, timeseries=True)
	print 'Max error for full volume, full timeseries: ' + str(maxerr)
	
	# Calculate max error for the full timeseries of the dry deposition
	maxerr = errors_obj.max_error(timeseries=True, dry=True)
	print 'Max error for wet depo, full timeseries: ' + str(maxerr)
	
	# Calculate max error for the full timeseries of the wet deposition
	maxerr = errors_obj.max_error(timeseries=True, wet=True)
	print 'Max error for wet depo, full timeseries: ' + str(maxerr)
	






The pydoc representation:

		    class FlexpartErrors(__builtin__.object)
		     |  A class that takes two FlexpartOutput objects as input and provides
		     |  methods for calculation of errors on slices, volumes and timeseries.
		     |  
		     |  The following arguments are included in most of the calls to methods,
		     |  so are described here rather than in each method.  There are a variety
		     |  of combinations, which might get confusing, as not all arguments are
		     |  used in all cases.
		     |  
		     |  timestamp : If it has a value, and if this is not a timeseries, then 
		     |  the indicated timestamp is used
		     |  
		     |  timestamp_list : if timeseries is True, then this will be used to provide
		     |  the list of timestamps to use.  
		     |  
		     |  level : specifies the level to use for a slice.  Indexed from 1.
		     |  
		     |  level_list : if volume is True, then this will be used to provide the
		     |  list of levels to use.
		     |  
		     |  species : species number to use.  Indexed from 1
		     |  
		     |  release : release number to use.  Indexed from 1
		     |  
		     |  age_class : age_class number to use.  Indexed from 1
		     |  
		     |  wet : if True, use the wet deposition.  If dry is also True, the returned value will be None
		     | 
		     |  
		     |  dry : if True, use the dry deposition.  If wet is also True, the returned value will be None
		     |  
		     |  timeseries : if True, then it will use a timeseries as defined in 
		     |  timestamp_list.  If timestamp_list is None, then a timeseries of
		     |  all available timestamps will be used
		     |  
		     |  volume : if True, then it will use a volume as defined in level_list.
		     |  If level_list is None, then a volume of all available levels will be
		     |  used.
		     |  
		     |  Methods defined here:
		     |  
		     |  __init__(self, control=None, test=None)
		     |      Constructor
		     |      control: a FlexpartOutput object to be used as control data
		     |      test: a FlexpartOutput object to be used as test data
		     |  
		     |  get_diff_grid(self, timestamp=None, timestamp_list=None, level=1, level_list=None, species=1, release=1, age_class=1, wet=False, dry=False, timeseries=False, volume=False)
		     |      Gets the difference grid as specified by the parameters.
		     |      
		     |      Note the True/False values of the last parameters:
		     |  
		     |  max_absolute_error(self, timestamp=None, timestamp_list=None, level=1, level_list=None, species=1, release=1, age_class=1, wet=False, dry=False, timeseries=False, volume=False)
		     |      Returns the max absolute error of the test and control grid, as
		     |      defined by the parameters
		     |  
		     |  max_error(self, timestamp=None, timestamp_list=None, level=1, level_list=None, species=1, release=1, age_class=1, wet=False, dry=False, timeseries=False, volume=False)
		     |      Returns the max magnitude error of the test and control grid, as
		     |      defined by the parameters
		     |  
		     |  mean_absolute_error(self, timestamp=None, timestamp_list=None, level=1, level_list=None, species=1, release=1, age_class=1, wet=False, dry=False, timeseries=False, volume=False)
		     |      Returns the mean absolute error of the test and control grid, as
		     |      defined by the parameters
		     |  
		     |  mean_bias(self, timestamp=None, timestamp_list=None, level=1, level_list=None, species=1, release=1, age_class=1, wet=False, dry=False, timeseries=False, volume=False)
		     |      Returns the mean of the biases of the test and control grid, as
		     |      defined by the parameters
		     |  
		     |  rmse(self, timestamp=None, timestamp_list=None, level=1, level_list=None, species=1, release=1, age_class=1, wet=False, dry=False, timeseries=False, volume=False)
		     |      Returns the max absolute error of the test and control grid, as
		     |      defined by the parameters
		




## Issues

* If wet or dry deposition is specified in the arguments, and there is no deposition in the dataset, I think this may crash
* There is now checking whether both dry and wet deposition arguments are set to True.  If this is the case, the returned grid will be None, and something will end up crashing.  
* See the issues in [FlexpartOutput](FlexpartOutput.md) that describe the ability to create customised timeseries and volumes - this flexibility allows the user to get into trouble if not careful
	




