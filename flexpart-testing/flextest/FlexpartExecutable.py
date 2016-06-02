# -*- coding: utf-8 -*-
"""
Created on Wed Mar 11 12:30:25 2015

@author: morton
"""

import copy
import filecmp
import logging
import os
import shutil
import subprocess
import sys

class FlexpartExecutable:


    def __init__(self, srcdir=None, destdir=None, 
                 makefile=None, parmodfile=None,
                 executable_name=None):
        
        """Set up the class
        
        srcdir : full path to location of FLEXPART src tree
        destdir : full path to copy the FLEXPART src tree into.
                  Files will be copied directly in here
        makefile : The makefile to use - if there is no leading path, then
                   file is assumed to be in srcdir.  Otherwise, it is 
                   assumed to be located as defined by the path
        parmodfile : full path to an external (from source code) par_mod.F90
        executable_name : The name of the expected executable (stripped path)
        """        

        # Reasons to abort ##########################
        
        # Test that srcdir is found
        if os.path.isdir(srcdir):
            self._srcdir = srcdir
        else:
            msg = 'srcdir not found: ' + srcdir
            raise Exception(msg)
            
        # test for a destdir argument
        if destdir:
            # Make sure it doesn't already exist            
            if os.path.isdir(destdir):
                msg = 'destdir cannot already exist: ' + destdir
                raise Exception(msg)
            else:
                self._destdir = destdir
        else:
            msg = 'No destdir argument'
            raise Exception(msg)

            
        # test for a makefile argument.  If the argument has a prefixed path,
        # then put the whole path in the makefile name.  Otherwise, it is
        # assumed to be in the source directory
        if makefile:
            # Make sure the we get the correct path on the makefile
            path_tuple = os.path.split(makefile)
            #print path_tuple
            if len(path_tuple[0]) == 0:
                # No leading path
                self._makefile = self._srcdir + '/' + makefile
            else:
                # Full path
                self._makefile = makefile
            #print self._makefile
            # Now let's make sure it actually exists
            if not os.path.isfile(self._makefile):
                msg = 'source makefile not found: ' + makefile
                self._makefile = None
                raise Exception(msg)
        else:
            msg = 'No makefile argument'
            raise Exception(msg)            
            
        # test for executable_name argument
        if executable_name:
            self._path_to_executable = destdir + '/' + executable_name
        else:
            msg = 'No executable_name argument'
            raise Exception(msg)                        
            
        # Test for external par_mod.F90 and, if it's valid, store path
        self._parmodfile = None
        if parmodfile:
            if os.path.isfile(parmodfile):
                self._parmodfile = parmodfile
            else:         
                msg = 'parmodfile not found: ' + parmodfile
                raise Exception(msg)


        #print self._path_to_executable        
        self._destdir_ok = False     # This indicates if copy to destdir was ok

        init_success = True
        

        # Make sure destdir does not already exist
        if not os.path.isdir(destdir):
            # Copy the distribution from srcdir into destdir and validate success
            try: 
                #print srcdir, destdir
                shutil.copytree(srcdir, destdir)                        
                logging.info('FlexpartExecutable: copying source code to temp location')

                # Compare the directories.  Success would be indicated
                # by an empty list diffs.diff_files                
                diffs = filecmp.dircmp(srcdir, destdir)
                if diffs.diff_files:
                    logging.warning('FlexpartExecutable: src and dest differ')
                    init_success = False


            except:       
                init_success = False
                logging.warning('FlexpartExecutable: copy failed')
                
            # If we made it here, one last step is to copy in a substitute 
            # parmodfile, if it was specified               
            if init_success and self._parmodfile:
                try:
                    shutil.copy(self._parmodfile, destdir + '/par_mod.F90')
                except:
                    init_success = False
                    logging.warning('FlexpartExecutable: parmodfile copy failed')
                    
        else:
            init_success = False
            logging.warning('FlexpartExecutable: destdir exists')

        if init_success:
            self._destdir_ok = True            

        # Remove any *.o and *.mod files
        extensions = ('.o', '.mod')
        for current_file in os.listdir(destdir):
            if any(current_file.endswith(ext) for ext in extensions):
                os.remove(destdir + '/' + current_file)
                
        
        
    def modify_makefile(self):
        """ Can't remember why I might have considered this method """
        
        pass
    
    def executable_exists(self):
        """ Returns True or False depending on whether the expected
        executable exists.  Note that this says nothing about whether it
        is a good executable or not - simply that an executable file of the
        expected name exists in the expected location """
        
        return_value = False
        if self._path_to_executable and \
            os.path.isfile(self._path_to_executable) and \
            os.access(self._path_to_executable, os.X_OK):
            return_value = True
        
        return return_value
    
    def compile_it(self):
        """Compile the code.  Return True or False depending on success"""

        make_command = 'make -f ' + self._makefile

        with open(self._destdir + '/compile.out', 'w') as out:
            make_process = subprocess.Popen(make_command, shell=True,
                                            stdout=out,
                                            stderr=out,
                                            cwd=self._destdir)        
        
        
        STATUS = make_process.wait()
        #print 'STATUS: ' + str(STATUS)
        if STATUS == 0:
            return True
        else:
            return False


    def get_expected_executable_path(self):
        
        """Return path to expected executable.  Note that this does not
        imply that the executable actually exists"""
        
        return copy.copy(self._path_to_executable)

    
if __name__ == "__main__":
    SRC='/u1/uaf/morton/tmp/C3'
    DEST='/u1/uaf/morton/tmp/Cblah'
    try:
        e = FlexpartExecutable(srcdir=SRC, destdir=DEST) 
    except:
        print 'failed...'
    
    
        
