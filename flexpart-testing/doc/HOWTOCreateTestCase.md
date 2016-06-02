
# How To Create Your Own Test Case


The original vision of the testing environment was to build a small number of small tests into the FLEXPART distribution so that the entire system could be quickly and thoroughly tested during program development.  We have included a few tests in the distribution, but we recognise that users will have their own specialised testing needs.  This document explains the process - mostly by sample - of creating and using your own test case, outside of the FLEXPART distribution.

A number of larger test cases (which were created using the procedures outline here) are available at http://borealscicomp.com/CTBTO_FLEXPART/LargeFlexpartTestCases/

Before we start, let's quickly review what a test case will typically look like.   The test case is specified in a hierarchical XML file (below).  


	<distribution>
	
	    <!-- Sample XML testing namelist -->
	
	    <short_descr>
	        Testing of long backward ECMWF
	    </short_descr>
	
	    <distropath>
	        /home/morton/git/ctbto/flexpart_code
	    </distropath>
	
	    <parmodpath>
	        /home/morton/MyFlexpartTests/case_data/ecmwf_9.5day_1p0/ec_bwd/par_mod.F90
	    </parmodpath>
	
	    <execname>
	        FLEXPART_GFORTRAN
	    </execname>
	
	    <metcase>
	
	        <short_descr>
	            1.0 degree ECMWF global fields
	        </short_descr>
	
	        <metfiledir>
	            /home/weather-shared/met_data/CTBTO/10daymet/10d_ec1p0d
	        </metfiledir>
	
	        <runcase>
	            <short_descr>
	                Backward run, 9.5 days length, 1 species, 1 emissions, no nested outgrid
	            </short_descr>
	
	            <casedir>
	                /home/morton/MyFlexpartTests/case_data/ecmwf_9.5day_1p0/ec_bwd/rundir_template
	            </casedir>
	
	
	            <controldatadir>
	                /home/morton/MyFlexpartTests/case_data/ecmwf_9.5day_1p0/ec_bwd/control_output
	            </controldatadir>
	
	            <basictest>
	                <short_descr>
	                    Max error over entire temporal and spatial mother domain
	                </short_descr>
	
	                <type>
	                    mother_all_vars_maxabserr
	                </type>
	
	                <max_threshold>
	                   1.0E-3
	                </max_threshold>
	            </basictest>
	        </runcase>
	
	    </metcase>
	
	</distribution>
	
At the top of a hierarchy we specify the FLEXPART **Distribution**, and various parameters needed to successfully compile the distribution.  These parameters will consist of the location of the source code and *par_mod.f90.*  *par_mod.f90* is typically intended to be located outside of the distribution - otherwise, users would have to constantly modify it for their own purposes every time somebody else made a change in the source distribution.  This distribution, defined by the *Makefile* (which is also specified outside the distribution, and outside of the XML specification file) and *par_mod.f90* (and location of source code) is compiled by the test environment and used to run all of the underlying test simulations.  

Next down in the hierarchy is the **MetCase** - this defines the location of input met files to be used in underlying test simulations, or **RunCases**.   A MetCase can support numerous RunCases - for example, a set of global met files could support forward and backward simulations for any time period that lies within that of the met files.  Additionally, regional simulations could be performed with these same met files.  Note that the XML file can support more than one MetCase, each MetCase having its own RunCases.

The **RunCases** in a **MetCase** section all use the same met data (but may use different periods within the collection of met files).  The RunCase specifies where the configuration files for the case are located, where the control output data (what we are using to compare tests against) is located, and a set of **BasicTests** to compare the test output data with the control output data.  RunCases may be forward or backward runs, or runs that are very similar to each other with the exception of parameterisations - it is up to the user who generates these cases.  The Case Directory specified in the XML file points to an actual FLEXPART runtime environment in which all of the configuration files are set up for the desired case.  So, it is rather easy to take an existing simulation and put it in the testing environment.

## Creating a test case

In principle the steps for setting up a new case are simple

* Create a normal FLEXPART case and run it, generating output data.  To do this, the user needs to
    * Obtain and set up AVAILABLE file for met data
    * Set up all of the configuration files
    * Run the case
* Save the output in another directory (e.g. *control_output*)
* Create the XML descriptor file

Rather than going through the painfully tedious details (which was my original intent), let's simply look at some cases that are already set up.  At the following URL is a complete directory structure of test data and test cases for

* 9.5 days of 1.0 degree ECMWF data for global fwd and bwd simulations
* 9.5 days of 1.0 degree NCEP data for global fwd and bwd simulations
* 6 hours of 0.5 degree ECMWF data for global fwd and bwd simulations
* 6 hours of 0.5 degree NCEP data for global fwd and bwd simulations

*Note that the actual ECMWF files are not readable through this website, for obvious reasons that Europeans understand!*


http://borealscicomp.com/CTBTO_FLEXPART/LargeFlexpartTestCases/ 

The directory tree for the full test suite looks as follows:

	.
	 case_data
	  ecmwf_6hr_0p5
	   ec_bwd
	    control_output
	    par_mod.F90
	    rundir_template
	   ec_fwd
	       control_output
	       par_mod.F90
	       rundir_template
	  ecmwf_9.5day_1p0
	   ec_bwd
	    control_output
	    par_mod.F90
	    rundir_template
	   ec_fwd
	       control_output
	       par_mod.F90
	       rundir_template
	  ncep_6hr_0p5
	   nc_bwd
	    control_output
	    par_mod.F90
	    rundir_template
	   nc_fwd
	       control_output
	       par_mod.F90
	       rundir_template
	  ncep_9.5day_1p0
	      nc_bwd
	       control_output
	       par_mod.F90
	       rundir_template
	      nc_fwd
	          control_output
	          par_mod.F90
	          rundir_template
	 ec_6hr_0p5_bwd.xml
	 ec_6hr_0p5_fwd.xml
	 ec_9.5day_1p0_bwd.xml
	 ec_9.5day_1p0_fwd.xml
	 nc_6hr_0p5_bwd.xml
	 nc_6hr_0p5_fwd.xml
	 nc_9.5day_1p0_bwd.xml
	 nc_9.5day_1p0_fwd.xml
	




If you look in one of the the XML files (or the one at the top of this document), you see that the met files are not necessarily located in this distribution.  You may set up your test cases to point to directories of met data, control output data, and others anywhere in your filesystem.

Each RunCase (for example, ncep_9.5day_1p0/nc_fwd/) has a directory of control output data (but it doesn't have to be located in this tree - you can put it - and point to it - anywhere), a par_mod.F90 which was used to compile the code for the control_output, and run directory template which looks like a typical FLEXPART run directory -

	.
	 options
	    AGECLASSES
	    COMMAND
	    COMMAND~
	    COMMAND.alternative
	    COMMAND.reference
	    IGBP_int1.dat
	    OH_7lev_agl.dat
	    OUTGRID
	    OUTGRID_NEST
	    RECEPTORS
	    RELEASES
	    RELEASES~
	    RELEASES.alternative
	    RELEASES.reference
	    SPECIES
	    surfdata.t
	    surfdepo.t
	 pathnames
	


It is **very important** that you use the following for your *pathnames*, unless you understand the driving code for the test environment and know what you're doing:

	options/
	output/
	met_data/
	met_data/AVAILABLE
	
The code we have developed to drive the testing will create a symbolic link, *met_data*, in the temporary FLEXPART run directory, pointing to the actual location of the met data and its AVAILABLE file.  As long as you correctly specify the location of the met data in the XML file, and you use the *pathnames* as described above, you should be OK.

For simplicity, we additionally assume that paths in the AVAILABLE file are relative.

***

## NOTES 2016-01-16

* Vtables are now part of the CTBTO development system, and will need to be part of the case data
* Delia recently built up a case with input nests, and we need to still formalise the documentation on that
* The *makefile_path *was originally part of the XML specification file.  However, we learned that every time you go to a new machine, or a new account, this field would need to be modified before a successful test.  So, this was removed from the XML specification file, and it is assumed that the *makefile_path* will be specified elswhere (right now, our *check.py* prototype has command line flags for specifiying this.)
