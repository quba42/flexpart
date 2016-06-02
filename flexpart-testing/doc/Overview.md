
# Overview of Flexpart Testing System

This document, as well as many that it points to, is rather technical and might be overwhelming to a general user who simple wants guidance on using the environment.

**If you are merely trying to test your current FLEXPART distribution and want to attempt a "quick start" set of instructions for doing so, please see [Quick Start Distribution Testing](QuickStartDistroTesting.md)**

**If you are wanting to test the differences between two FLEXPART runs (for example, a control and a test run), you might want to jump to [FlexpartErrors](FlexpartErrors.md) for some sample code.**

***

The original vision of the testing environment was to make sure that FLEXPART works correctly when initially installed.  Just as many distributions are installed via

	make
	make test
	make install

we wanted a way to quickly verify that a new FLEXPART installation was successful and worked correctly on test data.  Further, we wanted to deploy Test Driven Development (TDD) methods, and needed a way to easily test that the FLEXPART system still worked correctly after code changes were made.  The ideal - and this still needs to be achieved - is to have a suite of tests that are automatically performed and validated before any changes can become part of a distribution.

From this original vision came a set of requirements

* The testing environment requires a core foundation of low-level tools for comparing FLEXPART output in a variety of ways.  The development of these tools will be useful to many who want to analyse FLEXPART runs against each other for whatever reasonThe testing environment requires a core foundation of low-level tools for comparing FLEXPART output in a variety of ways.  The development of these tools will be useful to many who want to analyse FLEXPART runs against each other for whatever reason

* The overall testing environment, like the "make; make test" paradigm requires a structured ability to test that code compiles and to test its execution on a variety of problems.

  * The original vision was somewhat analogous to unit and system testing, in that tests should be very easy to run and should be quick.  This encourages the use of the testing environment, allowing developers to work more from a Test Driven Development (TDD) or Behaviour Driven Development (BDD) environment.

  * It was also recognised early on that many users would be interested in larger scale and longer tests that might run overnight, or even for days.  
 
 * The environment we created is based on the concept of testing a FLEXPART distribution with a specific Makefile and configuration of par_mod.f90.  To test this distribution, it would be necessary to make sure it compiles, and then run a number of test cases and make sure they all pass.


In the big picture, we consider a hierarchy of a Distribution test containing *MetCases*, which in turn contain RunCases, which in turn contain *BasicTests*.  Specific test suites are in user-defined XML files, such as the following simple example

	<distribution>
	
	    <!-- Sample XML testing namelist -->
	
	    <short_descr>
	        Basic distro
	    </short_descr>
	
	    <distropath>
	        /home/morton/flexpart
	    </distropath>
	
	    <parmodpath>
	        /home/morton/par_mod.f90
	    </parmodpath>
	
	    <metcase>
	
	        <short_descr>
	            ECMWF patch
	        </short_descr>
	
	        <metfiledir>
	            /home/morton/metfiles
	        </metfiledir>
	
	        <availablepath>
	            /home/morton/metfiles/AVAILABLE
	        </availablepath>
	
	        <runcase>
	            <short_descr>
	                Run case #1
	            </short_descr>
	
	            <casedir>
	                /home/morton/casedir/case01
	            </casedir>
	
	            <controldatadir>
	                /home/morton/casedir/case01/controldata
	            </controldatadir>
	
	            <basictest>
	
	                <short_descr>
	                    Max error over entire temporal and spatial domain
	                </short_descr>
	
	                <type>
	                    max_error
	                </type>
	
	                <max_threshold>
	                   1.0E-3
	                </max_threshold>
	            </basictest>
	
	            <basictest>
	                <short_descr>
	                    RMS error over entire temporal and spatial domain
	                </short_descr>
	
	                <type>
	                    rmse
	                </type>
	
	                <max_threshold>
	                   1.0E-6
	                </max_threshold>
	            </basictest>
	
	        </runcase>
	
	
	    </metcase>
	
	
	</distribution>
	

A *Distribution* test consists of a collection of one or more MetCases.  A single *MetCase* is just a timeseries of met files (for now, either NCEP or ECMWF) which could be used as input for a large variety of simulations - regional, global, forward, backward, different number of species, release/receptor points, parameterisations (e.g. convection or not), etc.

A single *MetCase* consists of a collection of one or more *RunCases*.  A single *RunCase* is a simulation of FLEXPART with a specific configuration.  A *RunCase* contains all of the configuration files that define the simulation, as well as a set of control output which defines expected correct output.  When the *RunCase* is deployed, FLEXPART runs as defined and produces a new set of output.  This output is compared with the control output in user-selected ways through one or more BasicTests

A single *BasicTest* defines the type of test to be performed - type of statistics, what parts of the output will be compared, etc., and a threshold that defines Pass or Fail.





### Implementation of the Testing Environment

* All Python based (2.7) with NumPy integration

* Set of low-level routines (in *flexpart-testing/flextest/*)
   * [FlexpartOutput](FlexpartOutput.md) - class that provides low level access to Flexpart output.  Based on modifications of John Burkhart's pflexible from 2011.  It provides methods to extract portions of output ranging from an entire multidimensional timeseries volume of all variables (levels, species, releases, etc.) or a subset down to a single horizontal slice for a single set of output at a single time, and everything in between.
   * [FlexpartErrors](FlexpartErrors.md) - extensible class that expects two FlexpartOutput objects (presumably a test and control) at instantiation, and supplies the methods to perform a variety of error calculations on user-defined slices of the FLEXPART output domain.  These methods' roles are to simply return a numerical value, and leave it to calling routines to make decisions based on these values
   * [FlexpartCase](FlexpartCase.md) - This class sets up a configured FLEXPART simulation in a temporary directory, uses a specified FLEXPART executable, and provides confirmation on whether the simulation completed successfully or not.  If so, then users might use the previously-referenced FlexpartErrors() to compare the new output with the control output.
   * [FlexpartExecutable](FlexpartExecutable.md)  - This class takes a FLEXPART code distribution with makefile and par_mod.f90 (which may be located outside the distribution for customisation), copies to a temporary directory, attempts to compile, and reports on success and location of the executable

* Higher level testing environment (in *flexpart-testing/distrotest/*) - uses the foundation provided by the low-level routines described above.  **A set of cascading images here showing the hierarchy in the containers**

    * [TestSuite](TestSuite.md)  - this is a high-level container that stores all information about a set of tests to perform.  It reads an XML specification file and creates a data hierarchy of Distributions (testing successful compilation and using the executable for tests),
    * [Distribution](Distribution.md) - this is a container that stores properties of a particular FLEXPART distribution test including paths to distribution and par_mod.f90, and a collection of MetCase hierarchical objects
    * [MetCase](MetCase.md) - this is a container that stores properties of a particular MetCase including location of met files (which should have an AVAILABLE file) and a collection of RunCase objects
    * [RunCase](RunCase.md) - this is a container that stores properties of a particular RunCase, including the simulation config files (like COMMAND, RELEASE, etc.), location of the control_output data to be compared against representing a single simulation, and a collection of zero or more BasicTests, each of which describes the test to be performed on the simulation output.  If the simulation is successful, the RunCase will have information on location of both control and test output data, which can then be examined by BasicTests
   * [BasicTest](BasicTest.md) - this is a container for testing specifications including the type of test to perform and the threshold value for the test.  
* Test driver, *check.py* (located in *flexpart-testing/check/*) - This is a Python program that expects an XML file as an argument, then creates a TestSuite consisting of all the components described above.  Then it proceeds through the hierarchy of Distribution, MetCase, RunCase and BasicTest, performing the compilations, simulations and testing.  (An example XML, and some sample output)
* 2016-01-08 - Test driver, *check-v2.py* (located in *flexpart-testing/check/*) supersedes *check.py*.  It now supports several command line arguments.
* The distribution test cases - set of relatively small cases are part of the current FLEXPART distribution.  They are generally set up so that user only needs to specify location of a custom Makefile




## Distribution testing environment


A set of test case XML specification files are available in flexpart-testing/check/.  These tests are set up so that the only parameter in the XML files that needs to be customized for the local environment is the path to the makefile (someday we will have the ability to auto-generate the makefiles, but we are not there yet).

	basic_ec_nc_testing.xml
	check.py
	ec_fwd_bwd_testing.xml
	generic1.xml
	generic_distro_only.xml
	generic_no_basictest.xml
	nc_fwd_bwd_testing.xml
	single_ec_bwd.xml
	single_ec_fwd.xml
	single_nc_bwd.xml
	single_nc_fwd.xml
	so_simple_ec_06h_bwd_preproc.xml
	so_simple_ec_06h_bwd.xml
	so_simple_ec_06h_fwd_preproc.xml
	so_simple_ec_06h_fwd.xml
	so_simple_ec_33h_bwd.xml
	so_simple_ec_33h_fwd.xml
	


*check.py* is the driver we have been using so far, and it expects a single argument - the path to an XML specification file.  A sample run follows:

	$ python check.py -m /home/morton/makefile.gfortran so_simple_ec_06h_bwd.xml
	Testing of FLEXPART for very small ECMWF met file
	../../flexpart_code
	/home/morton/makefile.gfortran
	../case_data/ecmwf_tiny/so_simple_ec_06h_bwd/par_mod.F90
	distro_destdir_name: /tmp/distrotest_d1104f82-2d99-4fb1-912f-0b9a16ecf850
	Executable exists: True
	
	============================
	
	compile test...
	Compile directory: /tmp/distrotest_d1104f82-2d99-4fb1-912f-0b9a16ecf850
	compile_success: True
	Executable exists: True
	
	============================
	
	[<distrotest.MetCase.MetCase object at 0x7ff1d578b810>]
	
	****************************
	Running MetCase: ECMWF patch - 11x11 1.0 degrees over Austria
	Met file dir: /home/morton/git/ctbto/flexpart-testing/case_data/ecmwf_tiny/met_data
	****************************
	
	Running RunCase: Backward run, 1 species, 1 emissions, 6 hours
	Case dir: /home/morton/git/ctbto/flexpart-testing/case_data/ecmwf_tiny/so_simple_ec_06h_bwd/rundir_template
	
	============================
	Case Test Backward run, 1 species, 1 emissions, 6 hours...
	Case template directory: /home/morton/git/ctbto/flexpart-testing/case_data/ecmwf_tiny/so_simple_ec_06h_bwd/rundir_template
	Case run directory: /tmp/caserun_12e6f6ac-82bf-4969-8777-c2a99be29d49
	Control data directory: /home/morton/git/ctbto/flexpart-testing/case_data/ecmwf_tiny/so_simple_ec_06h_bwd/control_output
	Met file dir: /home/morton/git/ctbto/flexpart-testing/case_data/ecmwf_tiny/met_data
	Executable: /tmp/distrotest_d1104f82-2d99-4fb1-912f-0b9a16ecf850/FLEXPART_GFORTRAN
	Number of basic tests: 2
	============================
	
	run_success: True
	Execution time: 3.33E+00 seconds
	
	============================
	
	
	-----------------------
	Basic Test
	Description: Max error over entire temporal and spatial mother domain
	Test type: mother_all_vars_maxabserr
	Threshold: 1.0E-03
	Test performed.  Error = 0.0E+00
	Test passed
	-----------------------
	
	
	-----------------------
	Basic Test
	Description: RMS error over entire temporal and spatial mother domain
	Test type: mother_all_vars_rmse
	Threshold: 1.0E-03
	Test performed.  Error = 0.0E+00
	Test passed
	-----------------------
	
	
If there is a failure in any part of the test, messages will typically tell the user where to find the test so that they can try to look at error logs and/or run it manually to gain necessary insight.

Finally, we have a document [HOWTOCreateTestCase](HOWTOCreateTestCase.md) that explains how to create customised test cases  outside of the FLEXPART distribution. 

***

## Notes on Documentation

To help keep the code and documentation better integrated, we chose to keep the documentation with the code in Markdown format.  In this way, we hope that developers are more likely to revise the documentation when the code is updated, and keep it in version control.

This documentation is written in plain Markdown (not necessarily the same Markdown you see in github).  Specifically, it has been edited with [Remarkable](https://remarkableapp.github.io/linux.html) , a Markdown editor for Linux.   In the *Style* menu, the *Markdown* option is chosen, rather than the default *Github*.

It is best viewed through a web browser with a Markdown Extension

* [Chrome Markdown Extension](https://github.com/borismus/markdown-preview)  (when installing this extension, be sure to pay attention to the note about selecting *Allow access to file URL's*)
* [Firefox Markdown Viewer](https://addons.mozilla.org/en-us/firefox/addon/markdown-viewer/)



