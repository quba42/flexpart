# -*- coding: utf-8 -*-
"""
Created on Wed Mar 11 14:16:46 2015

@author: morton
"""



import os
import shutil
import tempfile
import uuid
import unittest

import flextest.FlexpartExecutable as Fexec

class FlexpartExecutableTest(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
                
        """Create a source code directory, put simple files in it, and
        save name of dir so it can be removed at teardown"""


        # Create the source code directory to be used for
        # this set of tests
        cls._srcdir = tempfile.mkdtemp(prefix='srcdir_', dir='/tmp')
        #print 'setUpClass(): cls._srcdir: ' + cls._srcdir

        # Write the source code file
        f90_code = """
PROGRAM testprog       
    PRINT *, "Hello, World"           
END PROGRAM testprog
        """
        fh = open(cls._srcdir + '/testprog.f90', 'w')
        fh.write(f90_code)
        fh.close
        
        
        # Write the makefile - I couldn't do this in a docstring because
        # the tabs - all important in a Makefile - don't get preserved
        # This makefile is intended to be located in the source distribution
        fh = open(cls._srcdir + '/makefile.indistro', 'w')
        fh.write("FC=gfortran\n")
        fh.write("BIN=testprog\n")
        fh.write("$(BIN) : $(BIN).o\n")
        fh.write("\t$(FC) -o $(BIN) $(BIN).o\n")
        fh.write("$(BIN).o : $(BIN).f90\n")
        fh.write("\t$(FC) -c $(BIN).f90\n")
        fh.write("clean :\n")
        fh.write("\trm $(BIN) $(BIN).o\n")
        fh.close


        # Write the external makefile - I couldn't do this in a docstring because
        # the tabs - all important in a Makefile - don't get preserved
        # This makefile is intended to be located outside the source distribution
        cls._extmakefilepath = '/tmp/makefile.extdistro'
        fh = open(cls._extmakefilepath, 'w')
        fh.write("FC=gfortran\n")
        fh.write("BIN=testprog\n")
        fh.write("$(BIN) : $(BIN).o\n")
        fh.write("\t$(FC) -o $(BIN) $(BIN).o\n")
        fh.write("$(BIN).o : $(BIN).f90\n")
        fh.write("\t$(FC) -c $(BIN).f90\n")
        fh.write("clean :\n")
        fh.write("\trm $(BIN) $(BIN).o\n")
        fh.close

        # Write a proxy parmodfile - we name it something odd, to insure
        # that it is correctly copied in as par_mod.F90
        cls._parmodfilepath = '/tmp/proxyparmodfile'
        fh = open(cls._parmodfilepath, 'w')
        fh.write("This is a proxy par_mod.F90\n")
        fh.close



    
    def test_successful_init_with_indistro_makefile(self):
        
        """Test the creation of the FlexpartExecutable object"""
        
        # Create the name of a temporary destintation directory.  
        # It cannot exist prior.
        tempfilepath = '/tmp/testdestdir_' + str(uuid.uuid4())
        
        exec_obj = Fexec.FlexpartExecutable(srcdir=self._srcdir, 
                                            destdir=tempfilepath,
                                            makefile='makefile.indistro',
                                            executable_name='testprog')                                            
        
        # Now, make sure the two files exist in this destdir
        self.assertTrue( os.path.isfile(tempfilepath + '/testprog.f90'))
        self.assertTrue( os.path.isfile(tempfilepath + '/makefile.indistro'))
        
        # Now clean up the destdir
        shutil.rmtree(tempfilepath)
        self.assertFalse( os.path.isdir(tempfilepath))


    def test_external_parmodfile_copied(self):
        
        """Test that an external parmodfile is copied in as par_mod.F90"""
        
        # Create the name of a temporary destintation directory.  
        # It cannot exist prior.
        tempfilepath = '/tmp/testdestdir_' + str(uuid.uuid4())
        
        exec_obj = Fexec.FlexpartExecutable(srcdir=self._srcdir, 
                                            destdir=tempfilepath,
                                            parmodfile=self._parmodfilepath,
                                            makefile='makefile.indistro',
                                            executable_name='testprog')                                            
        
        # Now, make sure that there is now a par_mod.F90 in this destdir
        self.assertTrue( os.path.isfile(tempfilepath + '/par_mod.F90'))
        
        # Now clean up the destdir
        shutil.rmtree(tempfilepath)
        self.assertFalse( os.path.isdir(tempfilepath))





    def test_init_raises_exception_when_no_srcdir(self):
        self.assertRaises(Exception, 
                          Fexec.FlexpartExecutable, srcdir=None)
        
    def test_init_raises_exception_when_bad_srcdir(self):
        self.assertRaises(Exception, 
                          Fexec.FlexpartExecutable, srcdir='/x65x!0')        


    def test_successful_compile_with_indistro_makefile(self):
        
        """Test the successful compilation of the code in the 
        FlexpartExecutable object"""
        
        # Create the name of a temporary destintation directory.  
        # It cannot exist prior.
        tempfilepath = '/tmp/testdestdir_' + str(uuid.uuid4())


        exec_obj = Fexec.FlexpartExecutable(srcdir=self._srcdir, 
                                            destdir=tempfilepath,
                                            makefile="makefile.indistro",
                                            executable_name="testprog")        

        #print 'test_successful_compile obj id: ' + str(id(exec_obj))

        compile_success = exec_obj.compile_it()
        self.assertTrue(compile_success)
        
        self.assertTrue(os.path.isfile(exec_obj.get_expected_executable_path()))
    
        # Now clean up the destdir and make sure this was successful
        shutil.rmtree(tempfilepath)
        self.assertFalse( os.path.isdir(tempfilepath))    


    def test_successful_compile_with_extdistro_makefile(self):
        
        """Test the successful compilation of the code in the 
        FlexpartExecutable object"""
        
        # Create the name of a temporary destination directory.  
        # It cannot exist prior.
        tempfilepath = '/tmp/testdestdir_' + str(uuid.uuid4())


        exec_obj = Fexec.FlexpartExecutable(srcdir=self._srcdir, 
                                            destdir=tempfilepath,
                                            makefile=self._extmakefilepath,
                                            executable_name="testprog")        

        #print 'test_successful_compile obj id: ' + str(id(exec_obj))

        compile_success = exec_obj.compile_it()
        self.assertTrue(compile_success)
        
        self.assertTrue(os.path.isfile(exec_obj.get_expected_executable_path()))
    
        # Now clean up the destdir and make sure this was successful
        shutil.rmtree(tempfilepath)
        self.assertFalse( os.path.isdir(tempfilepath))    





    def test_executable_exists_returns_false_when_no_executable(self):
        
        """Tests that executable_exists() will return a False if the
        executable does not exist"""

        # Create the name of a temporary destintation directory.  
        # It cannot exist prior.
        tempfilepath = '/tmp/testdestdir_' + str(uuid.uuid4())

        exec_obj = Fexec.FlexpartExecutable(srcdir=self._srcdir, 
                                            destdir=tempfilepath,
                                            makefile="makefile.indistro",
                                            executable_name="testprog")        

        #print 'test_exec_exists_false obj id: ' + str(id(exec_obj))
        executable_exists = exec_obj.executable_exists()
        self.assertFalse(executable_exists)
        
        self.assertFalse(os.path.isfile(exec_obj.get_expected_executable_path()))
    
        # Now clean up the destdir and make sure this was successful
        shutil.rmtree(tempfilepath)
        self.assertFalse( os.path.isdir(tempfilepath))           
        

    def test_executable_exists_returns_true_when_executable(self):

        """Test that executable_exists() returns True when executable
        is present.  This will require that we compile first."""
        
        # Create the name of a temporary destintation directory.  
        # It cannot exist prior.
        """
        This will be a bit insecure, but I will use tempfile.NamedTemporaryFile,
        then just before calling the executable object, delete the file, and
        use that same name as the directory name
        """
        
        #tempfilepath = tempfile.mkstemp(prefix='testdestdir_',
        #                                dir='/tmp')[1]                                                   
        #os.remove(tempfilepath)
        tempfilepath = '/tmp/testdestdir_' + str(uuid.uuid4())

        #print 'test_exec_exists self._srcdir: ' + self._srcdir        
        #print 'test_exec_exists_compile tempfilepath: ' + tempfilepath

        # Now I can use tempfilename as the name of a new dir

                
        exec_obj = Fexec.FlexpartExecutable(srcdir=self._srcdir, 
                                            destdir=tempfilepath,
                                            makefile="makefile.indistro",
                                            executable_name="testprog")        
        #print 'test_exec_exists obj id: ' + str(id(exec_obj))
        
        compile_success = exec_obj.compile_it()
      
        self.assertTrue(compile_success)
        self.assertTrue(exec_obj.executable_exists())
        

    
        # Now clean up the destdir and make sure this was successful
        shutil.rmtree(tempfilepath)
        self.assertFalse( os.path.isdir(tempfilepath))    


    @classmethod
    def tearDownClass(cls):
        """Remove the source code directory used in these tests"""
        
        if os.path.isdir(cls._srcdir):
            shutil.rmtree(cls._srcdir)
            
            


if __name__ == '__main__':
    unittest.main()
    
