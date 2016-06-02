#!/usr/bin/env python

"""
@author: arnold

Delia Arnold
Arnold Scientific Consulting, Manresa, Spain.
delia.arnold.consulting@gmail.com


This routine will go over a set of selected cases
to ensure many of the functionalities of FLEXPART are working
under certain conditions. 

It may include multiple species, fwd - bwd, depositing species, 
VTABLES and fp format

"""

# --- import needed modules ----
import argparse
import os
import sys
import subprocess
import time
# ------------------------------

#import ur_config as urcf
__all__ = []
__version__ = 0.1
__date__ = '2016-02-10'
__updated__ = '2016-02-10'

DEBUG = 1 # [ 0: None, 1: all ]
ERR =  "[ERROR]   "
INFO = "[INFO]    "
WRN =  "[WARNING] "
SPACES = "           "


#---------  start of the main program ------------

def main():

    # To keep track of failures and exceptions
    list_failing_cases = []
    any_fails = 0
    list_exceptions_cases = []
    any_exceptions = 0
    list_compiling_errors = []
    any_failed_compile = 0
    list_running_errors = []
    any_failed_run = 0


    # Get the information from the command line:
    name_makefile_path = None   # Initial value for the makefile from command line
    parser = argparse.ArgumentParser()
    parser.add_argument("-m", "--makefile",
                    help="Full path to makefile",
                    action="store", dest="cmdline_makefile_path")
    parser.add_argument("-f", "--filelist",
                    help="file list of a name of xml cases",
                    action="store", dest="cases_filelist")
                                            
    args = parser.parse_args()

    # Get the command line arguments after parsing.  Some might be "None"
    cmdline_makefile_path = args.cmdline_makefile_path
    cases_filelist = args.cases_filelist
    
    # following the concept of check.py, if the path is not given
    # the ones specified in the xml files may be used - not yet implemented
    if  cmdline_makefile_path == None:
        if DEBUG == 1:
            print INFO, ' the makefile pathname was not specified and the tests cannot be performed'
            print SPACES, 'please specifiy a pathname use the -m <path> option'
        sys.exit()
    if cases_filelist == None:
        if DEBUG == 1:
            print INFO, ' the file with the list of cases was not specified and the tests cannot be performed'
            print SPACES, 'please specifiy a pathname use the -f <filename> option'
        sys.exit()
  
    # Get the list of cases LIST_OF_XML_FILES from argument file:
    LIST_OF_XML_FILES = []
    with open(cases_filelist) as f:
        LIST_OF_XML_FILES = f.read().splitlines()

    # START EXECUTING THE CASES
    if DEBUG == 1:
        print ' '
        print INFO, ' The following cases will be executed: '
        for items in LIST_OF_XML_FILES:
            print SPACES, ' -)' , items 
    print INFO, ' Start execution of the test cases'
    print SPACES, ' ... the time invested on running'
    print SPACES, '     these tests is variable, from 2'
    print SPACES, '     minutes to 30 with all cases'
    print ' ----------------------------------\n'
    
    # for the list of xml files, check.py is executed with the makefile provided (if provided)
    
    # to get the time:
    start_time = time.time()
    
    for xml_case in LIST_OF_XML_FILES:
        print '\n EXECUTING TEST : ', xml_case, '\n'
        if cmdline_makefile_path != None:
            process=subprocess.Popen(['./check-v3.py','-m', cmdline_makefile_path,  xml_case ],
                                      stdout=subprocess.PIPE,stderr=subprocess.PIPE)
            # print the output of the process
            for line in process.stdout:
                if "Exception" in line:
                    any_exceptions = 1
                if "failed" in line:
                    any_fails = 1
                if "compile_success: False" in line:
                    any_failed_compile = 1
                if "run_success: False" in line:
                    any_failed_run = 1
                sys.stdout.write(line)
            for line in process.stderr:
                if "Exception" in line:
                    any_exceptions = 1
                if "failed" in line:
                    any_fails = 1
                if "compile_success: False" in line:
                    any_failed_compile = 1
                if "run_success: False" in line:
                    any_failed_run = 1
                sys.stderr.write(line)
            
            # using os.system it works and the stfout and stderr is output on the fly.
            #os.system('./check-v3.py -m '+cmdline_makefile_path+' '+xml_case)

        else: # I leave this ready in case check-v3.py ever supports back to use a default makefile
            print INFO, ' No path to makefile was specified, run again the script'
            print SPACES, '  with -m path/makefile option'

        if any_exceptions == 1:
            list_exceptions_cases.append(xml_case)
        if any_fails == 1:
            list_failing_cases.append(xml_case)
        if any_failed_run == 1:
            list_running_errors.append(xml_case)
        if any_failed_compile == 1:
            list_compiling_errors.append(xml_case)

        any_exceptions = 0
        any_fails = 0
        any_failed_compile = 0
        any_failed_run = 0
 
        print '\n ******************************************** \n'        

    # to get the time
    stop_time = time.time()
    total_time = stop_time - start_time
    if DEBUG == 1:
        print INFO, ' -------------- SUMMARY-----------------'
        print INFO, ' Failing compilations: ', list_compiling_errors
        print INFO, ' Failing runs: ', list_running_errors
        print INFO, ' Failing cases: ', list_failing_cases
        print INFO, ' Cases raising exceptions: ', list_exceptions_cases

    if DEBUG == 1:
        print '\n'

        print INFO, ' Time invested in running the tests: '
        print SPACES, total_time, ' seconds'

#-----------------------------------------------------------

if __name__ == "__main__":

    main()
