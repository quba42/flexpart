# -*- coding: utf-8 -*-
"""
Created on Thu Aug 13 23:45:12 2015

@author: morton

Don Morton
Boreal Scientific Computing LLC, Fairbanks, Alaska, USA
Don.Morton@borealscicomp.com
http://www.borealscicomp.com/

@contributors
Christian Maurer
Delia Arnold
ZAMG, Vienna, Austria
christian.maurer@zamg.ac.at
delia.arnold-arias@zamg.ac.at

"""

import os
import xml.etree.ElementTree as XMLTree

import distrotest.BasicTest as BasicTest
import distrotest.RunCase as RunCase 
import distrotest.MetCase as MetCase
import distrotest.Distribution as Distribution

class TestSuite(object):
    
    """
    Container for an entire test suite, initialised from an XML config file.
    A TestSuite object contains Distribution classes, which in turn will each 
    contain MetCase, RunCase and BasicTest objects.  A TestSuite object should 
    have all of the information necessary for testing routines to do their
    thang.
    """
    
    def __init__(self, xml_files=None):
        
        """
        Creates a list of Distribution objects from a list of xml_files.
        If only a single file is entered as an argument (which may often
        be the case), it is inserted into a list of one element.  An XML file
        is read, and a Distribution object is manufactured from this
        """
        
        if xml_files:
            if type(xml_files) is str:
                self._xml_files = [xml_files]
            elif type(xml_files) is list:
                self._xml_files = xml_files
            else:
                raise Exception('xml_files should be a list')
        else:
            raise Exception('A list of valid xml_files is needed')
            
        # If we are here, we have our XML file list, and we should load the
        # contents of each one into a Distribution object
        self._distribution_list = []
        for the_xml_file in self._xml_files:
            
            # print the_xml_file
            try:
                #print 'hello'                
                distro_tree = XMLTree.parse(the_xml_file)
                #print distro_tree

            except:
                print 'Error parsing XML file: ' + str(the_xml_file)
                raise Exception

            distro_object = self._create_distribution(distro_tree=distro_tree)                
            self._distribution_list.append( distro_object )
            



    def get_distribution_list(self):
        return self._distribution_list
        
    def _create_distribution(self, distro_tree=None):

        """
        Creates and returns a distribution object (e.g. FLEXPART-ECMWF 
        (nested/not nested input), FLEXPART-GFS or FLEXPART-WRF) based 
        on the parsed XML distribution tree

        """

        # Find the Distribution parameter and MetCase nodes and iterate 
        # through those to create the 
        # MetCase Objects (which will recursively create RunCase and
        # BasicTest objects).  This is set up, however, to permit empty
        # metcase lists.  Creates and returns a Distribution object

        the_distribution = None
        if distro_tree:

            distro_path = parmod_path = exec_name = None            
            met_case_list = []
            distro_root = distro_tree.getroot()
            for child in distro_root:
                if child.tag == 'metcase':
                    the_met_case = self._create_met_case(met_case_tree=child)
                    met_case_list.append(the_met_case)
                elif child.tag == 'short_descr':
                    descr = child.text.strip()
                elif child.tag == 'distropath':
                    distro_path = child.text.strip()
                elif child.tag == 'parmodpath':
                    parmod_path = child.text.strip()
                elif child.tag == 'execname':
                    exec_name = child.text.strip()
                    

            if descr and distro_path and exec_name:
                the_distribution = Distribution.Distribution(
                                                 descr=descr, 
                                                 distro_path=distro_path,
                                                 parmod_path=parmod_path,
                                                 exec_name=exec_name,
                                                 met_case_list=met_case_list
                                                 )     
            else:
                raise ValueError(
                        "Unable to create Distribution object - missing args"
                        )

        return the_distribution 
                

    def _create_met_case(self, met_case_tree=None):
        
        """ Creates and returns a MetCase object based on the parsed XML
        tree.  This will read in the parameters for a MetCase object, as
        well as RunCase objects, if available.  Otherwise, inserts an empty
        RunCase list"""
        
        the_met_case = None
        metnestfile_dir = None
        if met_case_tree is not None:
            run_case_list = []
            met_case_root = met_case_tree.getchildren()
            for child in met_case_root:
                if child.tag == 'runcase':
                    the_run_case = self._create_run_case(run_case_tree=child)
                    run_case_list.append(the_run_case)
                elif child.tag == "short_descr":
                    descr = child.text.strip()
                elif child.tag == "metfiledir":
                    metfile_dir = child.text.strip()
                elif child.tag == "metnestfiledir":
                    metnestfile_dir = child.text.strip()

            if descr and metfile_dir:
                the_met_case = MetCase.MetCase(descr=descr,
                                               metfile_dir=metfile_dir,
                                               metnestfile_dir=metnestfile_dir,
                                               run_case_list=run_case_list)
            else:
                raise ValueError(
                    "Unable to create MetCase object - missing args"
                    )
   
        return the_met_case
    
    def _create_run_case(self, run_case_tree=None):
        """Creates and returns a RunCase object based on the parsed XML tree.
        This will read in the parameters for runcase objects, as
        well as BasicTest objects, if available.  Otherwise, inserts and
        empty BasicTest list"""

        the_run_case = None
        if run_case_tree is not None:
            descr = testtype = max_threshold = None
            
            basic_test_list = []
            run_case_root = run_case_tree.getchildren()
            for child in run_case_root:
                if child.tag == 'basictest':
                    the_basic_test = self._create_basic_test(basic_test_tree=child)
                    basic_test_list.append(the_basic_test)
                elif child.tag == 'short_descr':
                    descr = child.text.strip()
                elif child.tag == 'casedir':
                    case_dir = child.text.strip()
                elif child.tag == 'controldatadir':
                    control_data_dir = child.text.strip()

                
            if descr and case_dir and control_data_dir:
                the_run_case = RunCase.RunCase(descr=descr,
                                               case_dir=case_dir,
                                               control_data_dir=control_data_dir,
                                               test_list=basic_test_list)
            else:
                raise ValueError(
                    "Unable to create RunCase object - missing args"
                    )
                        
        return the_run_case


    def _create_basic_test(self, basic_test_tree=None):

        """ Creates and returns a BasicTest object based on the parsed
        XML tree parameters"""
        
        the_basic_test = None
        if basic_test_tree is not None:
            basic_test_root = basic_test_tree.getchildren()

            for child in basic_test_root:
                if child.tag == 'short_descr':
                    descr = child.text.strip()
                elif child.tag == 'type':
                    testtype = child.text.strip()
                elif child.tag == 'max_threshold':
                    max_threshold = child.text.strip()

            if descr and testtype and max_threshold:
                the_basic_test = BasicTest.BasicTest(descr=descr,
                                                     test_type=testtype,
                                                     threshold=max_threshold)
            else:
                raise ValueError(
                    "Unable to create BasicTest object - missing args"
                    )
        
        return the_basic_test

    
