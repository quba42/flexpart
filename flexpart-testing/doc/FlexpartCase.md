
# FlexpartCase


## Overview

This is a class intended for testing the execution of a FLEXPART pre-configured simulation.  An example of its usage follows


		import FlexpartCase
		
		src_rundir = '/home/me/case001'
		dest_rundir = '/tmp/me/test_case_rundir'
		flexpart_executable = '/home/me/flexpart_distro/FLEXPART_GFORTRAN'
	
		case_obj = FlexpartCase.FlexpartCase(
							src_dir=src_rundir,
							dest_dir=dest_rundir,
							flexpart_exe=flexpart_executable)
	
		case_obj.run() 
	
		success = case_obj.success()
	
		print 'case run success: ' + str(success)
	

	
The FlexpartCase class expects the following arguments

* *src_dir* - location of a properly configured run directory
* *dest_dir* - a temporary directory to be used for the actual case run test.  The contents of *src_dir* will be copied in here before the run test.  If *dest_dir* already exists, this will abort.  We don't want to accidentally overwrite an important directory that the user might have mistakenly put in as an argument.
* *flexpart_exe* - full path to the FLEXPART executable to be used for this test 



The pydoc representation:

	    class FlexpartCase(__builtin__.object)
	     |  Methods defined here:
	     |  
	     |  __init__(self, src_dir=None, dest_dir=None, flexpart_exe=None)
	     |      Set up the class
	     |      
	     |      src_dir : a directory structure all set to run FLEXPART.  Note that
	     |      the paths in pathnames must be all relative within this directory.
	     |      This directory will also need to contain the met data in directory
	     |      metdata
	     |      
	     |      dest_dir : location that src_dir will be copied to, and where the
	     |      run will be executed.  dest_dir cannot already exist.  We make
	     |      this restriction to prevent accidental overwriting of something
	     |      important.
	     |      
	     |      flexpart_exe : full path to a FLEXPART executable
	     |  
	     |  run(self)
	     |      Runs the case - assumes that the original case template was 
	     |      in good shape and that it was copied successfully.  The variable
	     |      self._destdir_ok provides a very simple, but incomplete check.
	     |      
	     |      This routine will launch the executable, and then wait for it to 
	     |      complete.  stdout will go to file stdout.txt at the top of the
	     |      dest_dir.
	     |  
	     |  success(self)
	     |      Looks at last line of stdout and insures it contains some of the
	     |      expected key words.  This is a very simple test and, in the future
	     |      should be replaced with something more robust that insures the expected 
	     |      output files are all present
	


## Issues

* None, so far
	




