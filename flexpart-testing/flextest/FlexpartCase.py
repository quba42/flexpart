# -*- coding: utf-8 -*-
"""
Created on Tue May 19 13:38:38 2015

@author: morton

Don Morton
Boreal Scientific Computing LLC, Fairbanks, Alaska, USA
Don.Morton@borealscicomp.com
http://www.borealscicomp.com/

contributors
Christian Maurer
Delia Arnold
ZAMG, Vienna, Austria
christian.maurer@zamg.ac.at
delia.arnold-arias@zamg.ac.at

"""



import copy
import filecmp
import logging
import os
import shutil
import subprocess
import sys
import time

class FlexpartCase(object):
    
    
    
    
    def __init__(self, src_dir=None, dest_dir=None, 
                 met_dir=None, met_nest_dir=None, flexpart_exe=None):
        
        """Set up the class
        
        src_dir : a directory structure all set to run FLEXPART.  Note that
        the paths in pathnames must be all relative within this directory.
        This directory will also need to contain the met data in directory
        met_data
        
        dest_dir : location that src_dir will be copied to, and where the
        run will be executed.  dest_dir cannot already exist.  We make
        this restriction to prevent accidental overwriting of something
        important.

        met_dir : location of met files.  Assumed to have a valid 
        AVAILABLE file in it

        
        flexpart_exe : full path to a FLEXPART executable
        """

        # 2016-01-15 - DJM - this is something Delia and Christian introduced,
        # but not sure why.  I have modified logic so it's not needed. 
        #symlink_nest = False

        # test for a src_dir argument
        if src_dir:
            
            # Test that src_dir is found
            if os.path.isdir(src_dir):
                self._src_dir = src_dir
            else:
                msg = 'src_dir not found: ' + src_dir
                raise Exception(msg)
        else:
            msg = 'No src_dir argument'
            raise Exception(msg)
            
        # test for a dest_dir argument
        if dest_dir:
            # Make sure it doesn't already exist            
            if os.path.isdir(dest_dir):
                msg = 'dest_dir cannot already exist: ' + dest_dir
                raise Exception(msg)
            else:
                self._dest_dir = dest_dir
        else:
            msg = 'No dest_dir argument'
            raise Exception(msg)     

        # test for a met_dir argument
        if met_dir:
            # Make sure it doesn't already exist            
            if os.path.isdir(met_dir):
                self._met_dir = met_dir
            else:
                msg = 'met_dir not found: ' + met_dir
                raise Exception(msg)
        else:
            msg = 'No met_dir argument'
            raise Exception(msg)     

        # test for a met_nest_dir argument
        if met_nest_dir:
            # Make sure it doesn't already exist            
            if os.path.isdir(met_nest_dir):
                self._met_nest_dir = met_nest_dir
            else:
                msg = 'met_nest_dir not found: ' + met_nest_dir
                raise Exception(msg)
            
        # Test that flexpart_exe exists and is executable
        if flexpart_exe:
            # Make sure it exists and is executable
            if os.path.isfile(flexpart_exe) and os.access(flexpart_exe, os.X_OK):
                self._flexpart_exe = flexpart_exe
            else:
                msg = 'flexpart_exe does not exist and/or is not executable'
                raise Exception(msg)
        else:
            msg = 'No flexpart_exe argument'
            raise Exception(msg)            
 


        self._destdir_ok = False     # This indicates if copy to destdir was ok
        init_success = True
        # Make sure destdir does not already exist
        if not os.path.isdir(dest_dir):
            # Copy the distribution from src_dir into dest_dir and validate success
            try: 
                #print src_dir, dest_dir
                shutil.copytree(src_dir, dest_dir)                        
                logging.info('FlexpartCase: copying case dir to temp location')

                # Compare the directories.  Success would be indicated
                # by an empty list diffs.diff_files                
                diffs = filecmp.dircmp(src_dir, dest_dir)
                if diffs.diff_files:
                    logging.warning('FlexpartCase: src and dest differ')
                    init_success = False


            except:
                init_success = False
                logging.warning('FlexpartCase: copy failed')
        else:
            init_success = False
            logging.warning('FlexpartCase: dest_dir exists')

        if init_success:

            # Check that important files are present.  If not, go ahead
            # and try to make it, raise exception if fails
            if not os.path.isdir(dest_dir + '/output'):
                # Try to make one
                try:
                    os.mkdir(dest_dir + '/output', 0755)
                except:
                    msg = 'No output dir present in case directory'
                    raise Exception(msg)            

            # Make a link to the met_dir
            try:
                os.symlink(self._met_dir, dest_dir + '/met_data')
            except:
                msg = 'Unable to create met_data symlink to: ' + self._met_dir
                raise Exception(msg)

            # Make a link to the met_nest_dir
            if met_nest_dir:
                try:
                    os.symlink(self._met_nest_dir, dest_dir + '/met_data_nest')
                except:
                    msg = 'Unable to create met_data_nest symlink to: ' + \
                           self._met_nest_dir
                    raise Exception(msg)
            
            # Make sure there is an AVAILABLE file 
            if not os.path.isfile(dest_dir + '/met_data/AVAILABLE'):
                msg = 'Unable to find AVAILABLE in ' + dest_dir + '/met_data'
                raise Exception(msg)
          
            if met_nest_dir:
                if not os.path.isfile(dest_dir + '/met_data_nest/AVAILABLE'):
                    msg = 'Unable to find AVAILABLE in ' + dest_dir + '/met_data_nest'
                    raise Exception(msg)
                                                                   



            self._destdir_ok = True     

            # Make a link to the met directory
        
        self._execution_time_seconds = -9999.0




    
    def run(self):
        """Runs the case - assumes that the original case template was 
        in good shape and that it was copied successfully.  The variable
        self._destdir_ok provides a very simple, but incomplete check.
        
        This routine will launch the executable, and then wait for it to 
        complete.  stdout will go to file stdout.txt at the top of the
        dest_dir.
        """

        stdoutFH = open(self._dest_dir + '/stdout.txt', 'w')

        start_time = time.time()
        the_process = subprocess.Popen( self._flexpart_exe, shell=True,
                                        stdout=stdoutFH,
                                        cwd=self._dest_dir )
        
        status=the_process.wait()
        stop_time = time.time()
        
        self._execution_time_seconds = stop_time - start_time
        
        stdoutFH.close()
        
        return True
        
    def success(self):
        """Looks at last line of stdout and insures it contains some of the
        expected key words.  This is a very simple test and, in the future
        should be replaced with something more robust that insures the expected 
        output files are all present
        """        
        
        so_far_so_good = True

        # Try to open the file for reading
        try:
            stdoutFH = open(self._dest_dir + '/stdout.txt', 'r')        
        except:
            so_far_so_good = False
        
        if so_far_so_good:
            # Try to get the contents of the last line and store in tokens
            try:
                for the_line in stdoutFH:
                    pass
                last_line = the_line
                stdoutFH.close()
        
                #print last_line
        
                tokens = last_line.split()
            except:
                so_far_so_good = False

        
        # If no errors so far, check for several expected keywords in the
        # tokens from the last line of the stdout file
        if so_far_so_good and ('CONGRATULATIONS:' and \
                               'SUCCESSFULLLY' and 'COMPLETED' \
                               and 'FLEXPART' in tokens):
            #print 'SUCCESS!!'
            return_val = True
        else:
            #print 'FAILED!!'
            return_val = False
        
        return return_val
        

    def execution_time_seconds(self):

        """Returns value of execution time in seconds"""

        return self._execution_time_seconds        
        
        
