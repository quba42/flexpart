
# Distribution


## Overview

This is a class intended to serve as a container for the properties of a particular FLEXPART distribution test, which will contain zero or more **MetCase** objects.  The container include description, path to distribution, path to *par_mod.f90* file, and a list of *MetCase* objects to be applied.  

The path *par_mod.f90* is specified at instantiation because the default assumption is that we might not want to use the ones that are in the code distribution - the *par_mod.f90* in the distribution may easily conform to the previous developer's personel test cases, and not the ones we're interested in.  So, the typical usage will be that the tester points to a *par_mod.f90* customised for the case of interest.


An example of its use (from a unittest):


	        description = 'Distribution Test'
	        distro_path = self._distro_dir
	        parmod_path = self._parmod_file
	        exec_name = self._exec_name
	        met_case_list = self._met_case_list
	
	        dist = Distribution.Distribution(descr=description,
	                                         distro_path=distro_path,
	                                         parmod_path=parmod_path,
	                                         exec_name=exec_name,
	                                         met_case_list=met_case_list)
	
	        self.assertEqual(dist.get_descr(), description)
	        self.assertEqual(dist.get_exec_name(), exec_name)
	        self.assertTrue(os.path.isdir(dist.get_distro_path()))
	        self.assertTrue(os.path.isfile(dist.get_parmod_path()))
	
	        # Get the MetCases and insure they are correct instances
	        mc_list = dist.get_met_case_list()
	        self.assertTrue(isinstance(mc_list[0], MetCase.MetCase))
	        self.assertTrue(isinstance(mc_list[1], MetCase.MetCase))
	

The information in a Distribution object may be used to instantiate a FlexpartExecutable object and test compilation, as seen in the following excerpts

	makefile = '/home/morton/makefile.gfortran'
	good_init = False
	compile_success = False
	try:
	    srcdir = os.path.realpath(the_distro.get_distro_path())
	    parmodfile = os.path.realpath(the_distro.get_parmod_path())
	    exec_name = the_distro.get_exec_name()
	    exec_obj = Fexec.FlexpartExecutable(srcdir=srcdir,
	                                        destdir=distro_destdir_name,
	                                        makefile=makefile,
	                                        parmodfile=parmodfile,
	                                        executable_name=exec_name)
	    good_init = True
	except Exception as e:
	    print 'Bad instantiation: ' + str(e)
	    pass
	
	if good_init:
	    print 'Executable exists: ' + str(exec_obj.executable_exists())
	
	
	
	    # Try to compile it
	    print; print '============================'; print
	    print 'compile test...'
	    print 'Compile directory: ' + distro_destdir_name
	    compile_success = exec_obj.compile_it()
	
	    print 'compile_success: ' + str(compile_success)
	    print 'Executable exists: ' + str(exec_obj.executable_exists())
	
	    if compile_success:
	        flexpart_executable = distro_destdir_name + '/' + exec_name
	    else:
	        print 'compile test failed'
	        print 'The test distribution is located in: ' + distro_destdir_name
	        print 'The makefile being used is: ' + makefile
	        print 'You should try to go there and see if you can find error by compiling by hand'
	

The pydoc representation:


	    class Distribution(__builtin__.object)
	     |  Distribution() - container that stores properties of a particular FLEXPART
	     |  distribution test, which will contain one or more MetCase objects, which
	     |  will contain one or more RunCase objects, which will contain one or more 
	     |  BasicTest objects.
	     |  Container includes description, path to distribution, 
	     |  path to par_mod.f90 file, and a list of MetCase objects to be applied.
	     |  
	     |  Methods defined here:
	     |  
	     |  __init__(self, descr=None, distro_path=None,  parmod_path=None, exec_name=None, met_case_list=None)
	     |      descr : Description string
	     |      distro_path : path to source code directory
	     |      parmod_path : (option) Path to par_mod.f90.  If no arg, uses the one
	     |      in the distribution
	     |      exec_name : name of executable
	     |      met_case_list : (optional) List of MetCase objects.  Defaults to empty
	     |      list.
	     |  
	     |  get_descr(self)
	     |  
	     |  get_distro_path(self)
	     |  
	     |  get_exec_name(self)
	     |  
	     |  
	     |  get_met_case_list(self)
	     |  
	     |  get_parmod_path(self)
	



## Issues

* None to report at this time
	




