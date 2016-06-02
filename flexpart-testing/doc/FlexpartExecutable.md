
# FlexpartExecutable 


## Overview

This is a class intended for testing the compilation of a FLEXPART distribution.  An example of its usage follows


		import FlexpartExecutable
		
		src_distro = '/home/me/flexpart'
		dest_dir = '/tmp/me/test_compile'
		makefile_name = 'makefile.ecmwf_gfortran'
		expected_exec_name = 'FLEXPART_GFORTRAN'
	
		exec_obj = FlexpartExecutable.FlexpartExecutable(
							srcdir=src_distro,
							destdir=dest_dir,
							makefile=makefile_name,
							executable_name=expected_exec_name)
	
		compile_success = exec_obj.compile_it()
	
		print 'compile_success: ' + str(compile_success)
	

	
The FlexpartExecutable class expects the following arguments

* *srcdir* - location of the FLEXPART distribution to be tested
* *destdir* - a temporary directory to be used for the actual compile test.  The contents of *srcdir* will be copied in here before the compile test.  If *destdir* already exists, this will abort.  We don't want to accidentally overwrite an important directory that the user might have mistakenly put in as an argument.
* *makefile_name* - the name of the makefile (in the distribution) to be used for compilation
* *executable_name* - the expected name of the executable.  Its presence will dictate whether compilation was successful or not.



The pydoc representation:

		CLASSES
		    FlexpartExecutable
		    
		    class FlexpartExecutable
		     |  Methods defined here:
		     |  
		     |  __init__(self, srcdir=None, destdir=None, makefile=None, executable_name=None)
		     |      Set up the class
		     |      
		     |      srcdir : full path to location of FLEXPART src tree
		     |      destdir : full path to copy the FLEXPART src tree into.
		     |                Files will be copied directly in here
		     |      makefile : The makefile in destdir to use
		     |      executable_name : The name of the expected executable (stripped path)
		     |  
		     |  compile_it(self)
		     |      Compile the code.  Return True or False depending on success
		     |  
		     |  executable_exists(self)
		     |      Returns True or False depending on whether the expected
		     |      executable exists.  Note that this says nothing about whether it
		     |      is a good executable or not - simply that an executable file of the
		     |      expected name exists in the expected location
		     |  
		     |  get_expected_executable_path(self)
		     |      Return path to expected executable.  Note that this does not
		     |      imply that the executable actually exists
		     |  
		     |  modify_makefile(self)
		     |      Can't remember why I might have considered this method
	


## Issues

* For the *makefile* argument, it's probably not a good idea to have it look in destdir for the makefile.  Since the intent of this is to test a new version, it's likely that the makefile already in the distribution won't work on the user's machine.  So, we might want to specify a makefile that's outside of the new distribution, but known to work on the machine.
	




