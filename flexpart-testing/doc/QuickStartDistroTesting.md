
#  Quick Start Notes for Distribution Testing System


These are quick and dirty notes that describe the system meant to "quick test" (or, in a longer time frame, "comprehensively test") one or more FLEXPART distributions on real data and test cases.

## Quick Start

First, here is the cookbook approach to running a set of small, comprehenstive tests on the current FLEXPART distribution.  These are run to insure that your current distribution works correctly, whether you recently downloaded it and want to test it in your environment, or whether you recent made some changes and want to make sure you didn't break anything.

In short, you will set the **PYTHONPATH** environment variable to the root location of the testing routines, which is directory *flexpart-testing*.   You will make sure that you have a working *Makefile* customized for your environment, and then in the directory *flexpart-testing/check/*, you will run the Python program *run_selected_cases.py*, giving it two arguments

* The location of the makefile you want to use
* The location of a file that contains the list of test cases you want to run

Setting up the PYTHONPATH, and invoking the test command would look something like the following


***
		
		
	$ export PYTHONPATH=~/fp/flexpart-testing
	$ python run_selected_cases.py -f list_simple_cases.lst -m ~/makefile.gfortran
			 
	[INFO]      The following cases will be executed: 
			             -) test_ecmwf_06hr_1p0deg_tinymetgrid_1species_1emission_fwd.xml
			             -) test_ecmwf_06hr_1p0deg_tinymetgrid_1species_1emission_bwd.xml
			             -) test_ncep_02hr_1p0deg_1species_1emission_fwd_bwd.xml
	[INFO]      Start execution of the test cases
			             ... the time invested on running
			                 these tests is variable, from 2
			                 minutes to 30 with all cases
			 ----------------------------------
			
			
		EXECUTING TEST :  test_ecmwf_06hr_1p0deg_tinymetgrid_1species_1emission_fwd.xml
		.
		.
		.
		
***

and the final lines of a successful run of tests would look something like

***

	.
	.
	.
	Test performed.  Error = 0.0E+00
	Test passed
	-----------------------
	
	All tests passed, erasing temporary directories
	/tmp/distrotest_36074af1-fca6-485c-ad93-36b486c6092b
	/tmp/caserun_11519fd4-5255-4176-8e25-3c6455a514de
	/tmp/caserun_75bf076d-e1c7-4b1d-bba6-8409820bb4ec
	
	 ******************************************** 
	
	[INFO]      -------------- SUMMARY-----------------
	[INFO]      Failing compilations:  []
	[INFO]      Failing runs:  []
	[INFO]      Failing cases:  []
	[INFO]      Cases raising exceptions:  []
	
	
	[INFO]      Time invested in running the tests: 
	            134.037215948  seconds
	
***



There is also a larger set of test cases in file list_complex_cases.lst, which may be run something like




	$ python run_selected_cases.py -f list_complex_cases.lst -m ~/makefile.gfortran



The path to the Makefile is necessary when running the check, and should be located outside the distribution, because many people will have widely varying Makefiles so, at least for the time being, it's impossible to put a "one-makefile-fits-all" into the source code distribution, and you'll never know if a working Makefile got overwritten in the distribution by somebody else.   

## Brief description of XML specification file

The XML specification file represents a test of a single FLEXPART distribution with a specific *par_mod.f90*.  When specifying the distribution, it's necessary to specify where the source code is (relative to the directory in which *check.py* is being run), the full path to the *par_mod.f90* that will be used for compiling, and the name of the expected executable.  

The path to *par_mod.f90* specification is optional and, if not present, the one that's in the distribution will be used.  It's recommended, however, that the user have their own, again, because you'll never know what got uploaded in the latest distribution.  Note that in my current distribution, the XML files reference the distribution that's in the repository.  It's set up this way so that changes to the source code can be easily tested over and over again.

A single *distribution* will have one or more *metcase* sections.  In this way, we can test a distribution with a number of different datasets.  A *metcase* section is defined by a path to the directory of met files, which **must contain a valid AVAILABLE file with local pathnames**.   

Note that in the current setup, using *generic1.xml*, the following tests are in directory *flexpart-testing/case_data/ecmwf_tiny/* which is a small ECMWF "patch" domain over Austria, intended for quick testing.  Inside this directory is the *met_data* directory.
A single *metcase* will have one or more *runcase* sections.  A *runcase* is a specific simulation defined by a specific set of FLEXPART configurations files.  In this

In the current setup for the simplest tests, the example *runcase* is set up in *flexpart-testing/case_data/ecmwf_tiny/complex_fwd/*, which contains the run directory

Finally, a *runcase* may contain one or more *basictest* sections, which compare the output of the *runcase* with a pre-specified expected control output for the simul

* mother_all_vars_maxabserr
* mother_all_vars_rmse
* nest_all_vars_maxabserr
* nest_all_vars_rmse

These four tests will compare the entire temporal and spatial domain (including all releases, all species, etc.) of the recent simulation and the control data.  It will not, currently, check deposition grids, but lower level methods in the testing environment will do that.

