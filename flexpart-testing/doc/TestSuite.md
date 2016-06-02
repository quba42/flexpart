
# TestSuite


## Overview

This is a class intended to serve as a container for an entire test suite, instantiated from an XML specification file.  A TestSuite object contains Distribution classes, which in turn will each contain zero or more Metcases.  A TestSuite object stores all of the necessary parameters and paths for testing routines to do their job.


An example of its use


	import distrotest.TestSuite as TS
	
	import flextest.FlexpartCase as FlexpartCase
	import flextest.FlexpartExecutable as Fexec
	import flextest.FlexpartErrors as FlexpartErrors
	import flextest.flexread.FlexpartOutput as FlexpartOutput
	import flextest.OutputCompare as OutputCompare
	
	
	# Bring in the XML filename from the command line
	if len(sys.argv) == 2:
	    xml_file = sys.argv[1]
	    XML_FILE = [xml_file]
	else:
	    print 'This program expects an XML filename as an argument...'
	    sys.exit()
	
	t = TS.TestSuite(xml_files=XML_FILE)

	# Get the first distribution in the test suite
	distro_list = t.get_distribution_list()	
	the_distro = distro_list[0]
	
	print the_distro.get_descr()
	print the_distro.get_distro_path()
	print the_distro.get_parmod_path()
	
The object will instantiate on a list of XML files, each one constituting a *Distribution*. 


The pydoc representation:


	    class TestSuite(__builtin__.object)
	     |  Container for an entire test suite, initialised from an XML config file.
	     |  A TestSuite object contains Distribution classes, which in turn will each 
	     |  contain MetCase, RunCase and BasicTest objects.  A TestSuite object should 
	     |  have all of the information necessary for testing routines to do their
	     |  thang.
	     |  
	     |  Methods defined here:
	     |  
	     |  __init__(self, xml_files=None)
	     |      Creates a list of Distribution objects from a list of xml_files.
	     |      If only a single file is entered as an argument (which may often
	     |      be the case, it is inserted into a list of one element).  An XML file
	     |      is read, and a Distribution object is manufactured from this
	     |  
	     |  get_distribution_list(self)
	



## Issues

* The class hard-codes the definition for scratch space - all of the tests will be performed in temporary directories.  By default, these will all be under /tmp, but on some systems this may be a bad idea, especially if routines that use this class don't clean up afterwards.
	




