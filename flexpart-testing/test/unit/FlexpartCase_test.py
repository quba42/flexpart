# -*- coding: utf-8 -*-
"""
Created on Tue May 19 13:39:10 2015

@author: morton
"""

import os
import shutil
import tempfile
import uuid
import unittest

import flextest.FlexpartCase as Fcase

class FlexpartCaseTest(unittest.TestCase):


    @classmethod
    def setUpClass(cls):
                
        """Create a source code directory, put simple files in it, and
        save name of dir so it can be removed at teardown.  In this
        case we keep it very simple.  The source directory will have
        a single file, a simple Python executable which will print the
        same output we expect from a successful FLEXPART run
        
        Added 2015-09-01 - DJM - will also create a met data dir, which
        we will expect to be linked to in the run directory"""


        # Create the source code directory to be used for
        # this set of tests
        cls._srcdir = tempfile.mkdtemp(prefix='srcdir_', dir='/tmp')
        #print 'setUpClass(): cls._srcdir: ' + cls._srcdir


        # Create the met file proxy directory to be used for
        # this set of tests
        cls._metfiledir = tempfile.mkdtemp(prefix='metfiledir_', dir='/tmp')
        #print 'setUpClass(): cls._srcdir: ' + cls._srcdir

        # Create an AVAILABLE file
        open(cls._metfiledir + '/AVAILABLE', 'w').close()


        # Write the executable
        exec_code = """#!/usr/bin/env python
import time
for i in range(1):
    print str(i)
    time.sleep(1)

print 'CONGRATULATIONS: YOU HAVE SUCCESSFULLY COMPLETED A FLEXPART MODEL RUN!'
        """
        fh = open(cls._srcdir + '/testprog.py', 'w')
        fh.write(exec_code)
        fh.close()

        # Make it executable        
        os.chmod(cls._srcdir + '/testprog.py', 0755)        

	# Create output dir so that this passes init test
        os.mkdir(cls._srcdir + '/output', 0755)






    
    def test_successful_init(self):
        
        """Test the creation of the FlexpartCase object"""
        
        # Create the name of a temporary destintation directory.  
        # It cannot exist prior.
        tempfilepath = '/tmp/testdestdir_' + str(uuid.uuid4())
        
        exec_obj = Fcase.FlexpartCase(src_dir=self._srcdir, 
                                            dest_dir=tempfilepath,
                                            met_dir=self._metfiledir,
                                            flexpart_exe=self._srcdir + '/testprog.py')                                            
        
        # Now, make sure the single file exists in this destdir
        self.assertTrue( os.path.isfile(tempfilepath + '/testprog.py'))
        
        # And make sure this is a link to the met_dir
        self.assertTrue( os.path.islink(tempfilepath + '/met_data' ))
        
        # Now clean up the destdir
        shutil.rmtree(tempfilepath)
        self.assertFalse( os.path.isdir(tempfilepath))

    def test_init_raises_exception_when_no_srcdir(self):
        self.assertRaises(Exception, 
                          Fcase.FlexpartCase, src_dir=None)
        
    def test_init_raises_exception_when_bad_srcdir(self):
        self.assertRaises(Exception, 
                          Fcase.FlexpartCase, src_dir='/x65x!0')        

    def test_init_raises_exception_when_no_output_dir(self):
        # First, remove the output dir from the test dir
        os.rmdir(self._srcdir + '/output')

        self.assertRaises(Exception, 
                          Fcase.FlexpartCase, src_dir='/x65x!0')        

        # Then, put the output dir back in 
        os.mkdir(self._srcdir + '/output', 0755)

    def test_successful_run(self):
        
        """Test the successful run of the code in the 
        FlexpartCase object"""
        
        # Create the name of a temporary destintation directory.  
        # It cannot exist prior.
        tempfilepath = '/tmp/testdestdir_' + str(uuid.uuid4())


        case_obj = Fcase.FlexpartCase(src_dir=self._srcdir, 
                                            dest_dir=tempfilepath,
                                            met_dir=self._metfiledir,
                                            flexpart_exe=self._srcdir + '/testprog.py')        

        run_val = case_obj.run()
        success_val = case_obj.success()
        self.assertTrue(success_val)
        
    
        # Now clean up the destdir and make sure this was successful
        shutil.rmtree(tempfilepath)
        self.assertFalse( os.path.isdir(tempfilepath))    

    def test_unsuccessful_run_when_no_stdout(self):
        
        """Test that the success() method returns False if it can't find 
        a stdout file"""
        
        # Create the name of a temporary destintation directory.  
        # It cannot exist prior.
        tempfilepath = '/tmp/testdestdir_' + str(uuid.uuid4())


        case_obj = Fcase.FlexpartCase(src_dir=self._srcdir, 
                                            dest_dir=tempfilepath,
                                            met_dir=self._metfiledir,
                                            flexpart_exe=self._srcdir + '/testprog.py')        

        run_val = case_obj.run()

        # Hard-coded - we delete the stdout.txt that success() method looks for
        os.remove(tempfilepath + '/stdout.txt')
        
        success_val = case_obj.success()
        self.assertFalse(success_val)
        
    
        # Now clean up the destdir and make sure this was successful
        shutil.rmtree(tempfilepath)
        self.assertFalse( os.path.isdir(tempfilepath))    

    def test_unsuccessful_run_when_stdout_empty(self):
        
        """Test that the success() method returns False if it finds stdout, 
        but stdout file is empty"""
        
        # Create the name of a temporary destintation directory.  
        # It cannot exist prior.
        tempfilepath = '/tmp/testdestdir_' + str(uuid.uuid4())


        case_obj = Fcase.FlexpartCase(src_dir=self._srcdir, 
                                            dest_dir=tempfilepath,
                                            met_dir=self._metfiledir,
                                            flexpart_exe=self._srcdir + '/testprog.py')        

        run_val = case_obj.run()

        # Hard-coded - we create a new, empty stdout.txt, overwriting the
        # existing one
        open(tempfilepath + '/stdout.txt', 'w').close()
        #os.remove(tempfilepath + '/stdout.txt')
        
        success_val = case_obj.success()
        self.assertFalse(success_val)
    
        # Now clean up the destdir and make sure this was successful
        shutil.rmtree(tempfilepath)
        self.assertFalse( os.path.isdir(tempfilepath))    


    def test_execution_time_positive(self):
        
        """Test that the execution_time_seconds() method returns 
        something greater than zero"""
        
        # Create the name of a temporary destintation directory.  
        # It cannot exist prior.
        tempfilepath = '/tmp/testdestdir_' + str(uuid.uuid4())


        case_obj = Fcase.FlexpartCase(src_dir=self._srcdir, 
                                            dest_dir=tempfilepath,
                                            met_dir=self._metfiledir,
                                            flexpart_exe=self._srcdir + '/testprog.py')        

        run_val = case_obj.run()

        wall_time = case_obj.execution_time_seconds()
    
        # Now clean up the destdir and make sure this was successful
        shutil.rmtree(tempfilepath)
        self.assertTrue( wall_time > 0.0 )    








    @classmethod
    def tearDownClass(cls):
        """Remove the source code directory used in these tests"""
        
        if os.path.isdir(cls._srcdir):
            shutil.rmtree(cls._srcdir)
            
        if os.path.isdir(cls._metfiledir):
            shutil.rmtree(cls._metfiledir)            
            

if __name__ == '__main__':
    unittest.main()
    
