# -*- coding: utf-8 -*-
"""
Created on Wed Aug 26 01:29:27 2015

@author: morton

Don Morton
Boreal Scientific Computing LLC, Fairbanks, Alaska, USA
Don.Morton@borealscicomp.com
http://www.borealscicomp.com/
"""

import os
import shutil
import tempfile
import unittest

import distrotest.BasicTest as BasicTest
import distrotest.RunCase as RunCase
import distrotest.MetCase as MetCase
import distrotest.Distribution as Distribution
import distrotest.TestSuite as TestSuite

class TestSuiteTest(unittest.TestCase):
    
    pass

    @classmethod
    def setUpClass(cls):
        
        """
        TODO - 
        
        Create temporary XML files from content in private methods below
        Instantiate some TestSuite objects 
        
        - a bare bones distro with no metcases
        - a distro where metcase has no runcases
        - a distro where runcase has no basictest
        - a distro that goes all the way down to basictest
        - a complex distro with multiple metcases, runcases, basictests
        """

        xml_input_file_no_metcase = tempfile.mkstemp(
                            prefix='xml_no_metcase_', dir='/tmp')
        cls._xml_input_filename_no_metcase = xml_input_file_no_metcase[1]
        fh = open(cls._xml_input_filename_no_metcase, 'w')
        fh.write( cls._sample_xml_no_metcase())
        fh.close()

        xml_input_file_no_runcase = tempfile.mkstemp(
                            prefix='xml_no_runcase_', dir='/tmp')
        cls._xml_input_filename_no_runcase = xml_input_file_no_runcase[1]
        fh = open(cls._xml_input_filename_no_runcase, 'w')
        fh.write( cls._sample_xml_no_runcase())
        fh.close()

        xml_input_file_no_basictest = tempfile.mkstemp(
                            prefix='xml_no_basictest_', dir='/tmp')
        cls._xml_input_filename_no_basictest = xml_input_file_no_basictest[1]
        fh = open(cls._xml_input_filename_no_basictest, 'w')
        fh.write( cls._sample_xml_no_basictest())
        fh.close()

        xml_input_file_1 = tempfile.mkstemp(prefix='xml_1_', dir='/tmp')
        cls._xml_input_filename_1 = xml_input_file_1[1]
        fh = open(cls._xml_input_filename_1, 'w')
        fh.write( cls._sample_xml_1())
        fh.close()

        xml_input_file_2 = tempfile.mkstemp(prefix='xml_2_', dir='/tmp')
        cls._xml_input_filename_2 = xml_input_file_2[1]
        fh = open(cls._xml_input_filename_2, 'w')
        fh.write( cls._sample_xml_2())
        fh.close()

        
        cls._test_suite_no_metcase = TestSuite.TestSuite(xml_files = 
                                        cls._xml_input_filename_no_metcase)        

        cls._test_suite_no_runcase = TestSuite.TestSuite(xml_files = 
                                        cls._xml_input_filename_no_runcase)

        cls._test_suite_no_basictest = TestSuite.TestSuite(xml_files = 
                                        cls._xml_input_filename_no_basictest)


        # This is a full, but simple, XML file all the way down to
        # basictest
        cls._test_suite_1 = TestSuite.TestSuite(xml_files = 
                                        cls._xml_input_filename_1)


        xml_filelist = [cls._xml_input_filename_1, cls._xml_input_filename_2]
        cls._test_suite_2 = TestSuite.TestSuite(xml_files=xml_filelist)

#    def test_inits_ok_on_good_xml_file(self):
#        pass
    
#    def test_raises_exception_on_bad_xml_file(self):
#       pass
    
    def test_retrieval_of_good_test_suite_no_metcase(self):
        """ Just do some spot checking in here to make sure basic fields
        are present """
        

        distro_list = self._test_suite_no_metcase.get_distribution_list()
        
        self.assertEqual(1, len(distro_list), msg="Length of distro list")

        #print type(distro_list[0])
        self.assertTrue(isinstance(distro_list[0], Distribution.Distribution), 
                        msg="Distribution instance - no runcase")
                        
        # Check one of the stored parameters for the distribution
        self.assertEqual('/tmp', distro_list[0].get_distro_path())


    def test_retrieval_of_good_test_suite_no_runcase(self):
        """ Just do some spot checking in here to make sure basic fields
        are present """
        

        distro_list = self._test_suite_no_runcase.get_distribution_list()
        
        self.assertEqual(1, len(distro_list), msg="Length of distro list")

        #print type(distro_list[0])
        self.assertTrue(isinstance(distro_list[0], Distribution.Distribution), 
                        msg="Distribution instance")
                        
        # Check one of the stored parameters for the distribution
        self.assertEqual('/tmp', distro_list[0].get_distro_path())
        
        # Check the metcase object briefly
        the_metcase = distro_list[0].get_met_case_list()[0]
        self.assertEqual(the_metcase.get_descr(), "ECMWF patch")

    def test_retrieval_of_good_test_suite_no_basictest(self):
        """ Just do some spot checking in here to make sure basic fields
        are present """
        

        distro_list = self._test_suite_no_basictest.get_distribution_list()
        
        self.assertEqual(1, len(distro_list), msg="Length of distro list")

        #print type(distro_list[0])
        self.assertTrue(isinstance(distro_list[0], Distribution.Distribution), 
                        msg="Distribution instance")
                        
        # Check one of the stored parameters for the distribution
        self.assertEqual('/tmp', distro_list[0].get_distro_path())
        
        # Check the metcase object briefly
        the_metcase = distro_list[0].get_met_case_list()[0]
        self.assertEqual(the_metcase.get_descr(), "ECMWF patch")

        # Check the runcase object briefly
        the_runcase = the_metcase.get_run_case_list()[0]
        self.assertTrue(isinstance(the_runcase, RunCase.RunCase), 
                        msg="RunCase instance")
                        
        self.assertEqual(the_runcase.get_descr(), "Run case #1")


    def test_retrieval_of_good_test_suite_1(self):
        """ A bit more complex, as this distribution will have met cases,
        and the test will make sure the metcases are good, as well as
        the underlying runcases, and basictests"""
        
        distro_list = self._test_suite_1.get_distribution_list()
        
        self.assertEqual(1, len(distro_list), msg="Length of distro list")

        #print type(distro_list[0])
        self.assertTrue(isinstance(distro_list[0], Distribution.Distribution), 
                        msg="Distribution instance")
                        
        # Check one of the stored parameters for the distribution
        self.assertEqual('/tmp', distro_list[0].get_distro_path())

        # Check the metcase object briefly
        the_metcase = distro_list[0].get_met_case_list()[0]
        self.assertEqual(the_metcase.get_descr(), "ECMWF patch")

        # Check the runcase object briefly
        the_runcase = the_metcase.get_run_case_list()[0]
        self.assertTrue(isinstance(the_runcase, RunCase.RunCase), 
                        msg="RunCase instance")
                        
        self.assertEqual(the_runcase.get_descr(), "Run case #1")
        
        # Check the basictest object briefly
        the_basictest = the_runcase.get_test_list()[1]
        self.assertTrue(isinstance(the_basictest, BasicTest.BasicTest), 
                        msg="BasicTest instance")
                        
        self.assertEqual(the_basictest.get_test_type(), "rmse")        
    
    def test_retrieval_of_good_test_suite_2(self):
    
        """ Even more complex, as this test suite has two distributions.  Will
        test second distribution will have met cases,
        and the test will make sure the metcases are good, as well as
        the underlying runcases, and basictests
        
        We will look at the second basic test of the second runcase of the
        second metcase of the second distribution"""
        
        distro_list = self._test_suite_2.get_distribution_list()
        
        self.assertEqual(2, len(distro_list), msg="Length of distro list")

        #print type(distro_list[0])
        self.assertTrue(isinstance(distro_list[1], Distribution.Distribution), 
                        msg="Distribution instance")
                        
        # Check one of the stored parameters for the distribution
        self.assertEqual('/tmp', distro_list[1].get_distro_path())

        # Check the second metcase object briefly
        the_metcase = distro_list[1].get_met_case_list()[1]
        self.assertEqual(the_metcase.get_descr(), "GFS Some date")

        # Check the second runcase object briefly
        the_runcase = the_metcase.get_run_case_list()[1]
        self.assertTrue(isinstance(the_runcase, RunCase.RunCase), 
                        msg="RunCase instance")
                        
        self.assertEqual(the_runcase.get_descr(), 
                         "Run case #2 of Met case #2 of second distribution")
        
        # Check the second basictest object briefly
        the_basictest = the_runcase.get_test_list()[1]
        self.assertTrue(isinstance(the_basictest, BasicTest.BasicTest), 
                        msg="BasicTest instance")
                        
        self.assertEqual(the_basictest.get_test_type(), "rmse2")            
    
        # Run case #2 of Met case #2 of second distribution    
    
    
        
    @classmethod
    def tearDownClass(cls):
        """Remove the XML input files"""
        if os.path.isfile(cls._xml_input_filename_no_metcase):
            os.remove(cls._xml_input_filename_no_metcase)

        if os.path.isfile(cls._xml_input_filename_no_runcase):
            os.remove(cls._xml_input_filename_no_runcase)
            
        if os.path.isfile(cls._xml_input_filename_no_basictest):
            os.remove(cls._xml_input_filename_no_basictest)
                        
        if os.path.isfile(cls._xml_input_filename_1):
            os.remove(cls._xml_input_filename_1)

        if os.path.isfile(cls._xml_input_filename_2):
            os.remove(cls._xml_input_filename_2)


    @classmethod
    def _sample_xml_no_metcase(self):
        
        
        """ Returns raw text for a test XML file.  Note that in places where
        I specify a directory or file, I'm specifying a dir/file that's sure to
        exist in a Unix system - the constructors will otherwise raise an
        exception if the dir/file isn't found 
        
        This is a simple distribution, with no met cases"""
        
        the_xml = """<distribution>

    <!-- Sample XML testing namelist -->

    <short_descr>
        Basic distro
    </short_descr>

    <distropath>
        /tmp
    </distropath>

    <makefilepath>
        /bin/ls
    </makefilepath>

    <parmodpath>
        /bin/ls
    </parmodpath>
    
    <execname>
        A_FLEXPART_EXECUTABLE
    </execname>    

</distribution>
        """
        return the_xml

    @classmethod
    def _sample_xml_no_runcase(self):
        
        
        """ Returns raw text for a test XML file.  Note that in places where
        I specify a directory or file, I'm specifying a dir/file that's sure to
        exist in a Unix system - the constructors will otherwise raise an
        exception if the dir/file isn't found 
        
        This has a metcase, with no runcase"""
        
        the_xml = """<distribution>

    <!-- Sample XML testing namelist -->

    <short_descr>
        Basic distro
    </short_descr>

    <distropath>
        /tmp
    </distropath>

    <makefilepath>
        /bin/ls
    </makefilepath>

    <parmodpath>
        /bin/ls
    </parmodpath>
    
    <execname>
        A_FLEXPART_EXECUTABLE
    </execname>    
    
    <metcase>

        <short_descr>
            ECMWF patch
        </short_descr>

        <metfiledir>
            /tmp
        </metfiledir>


    </metcase>


</distribution>
        """
        return the_xml


    @classmethod
    def _sample_xml_no_basictest(self):
        
        
        """ Returns raw text for a test XML file.  Note that in places where
        I specify a directory or file, I'm specifying a dir/file that's sure to
        exist in a Unix system - the constructors will otherwise raise an
        exception if the dir/file isn't found
        
        This has a metcase and runcase, but no basictest in the runcase"""
        
        the_xml = """<distribution>

    <!-- Sample XML testing namelist -->

    <short_descr>
        Basic distro
    </short_descr>

    <distropath>
        /tmp
    </distropath>

    <makefilepath>
        /bin/ls
    </makefilepath>

    <parmodpath>
        /bin/ls
    </parmodpath>
    
    <execname>
        A_FLEXPART_EXECUTABLE
    </execname>    
    
    <metcase>

        <short_descr>
            ECMWF patch
        </short_descr>

        <metfiledir>
            /tmp
        </metfiledir>

        <runcase>
            <short_descr>
                Run case #1
            </short_descr>

            <casedir>
                /tmp
            </casedir>

            <controldatadir>
                /tmp
            </controldatadir>

        </runcase>

    </metcase>


</distribution>
        """
        return the_xml







    @classmethod
    def _sample_xml_1(self):
        
        
        """ Returns raw text for a test XML file.  Note that in places where
        I specify a directory or file, I'm specifying a dir/file that's sure to
        exist in a Unix system - the constructors will otherwise raise an
        exception if the dir/file isn't found """
        
        the_xml = """<distribution>

    <!-- Sample XML testing namelist -->

    <short_descr>
        Basic distro
    </short_descr>

    <distropath>
        /tmp
    </distropath>

    <makefilepath>
        /bin/ls
    </makefilepath>

    <parmodpath>
        /bin/ls
    </parmodpath>
    
    <execname>
        A_FLEXPART_EXECUTABLE
    </execname>    
    
    <metcase>

        <short_descr>
            ECMWF patch
        </short_descr>

        <metfiledir>
            /tmp
        </metfiledir>

        <runcase>
            <short_descr>
                Run case #1
            </short_descr>

            <casedir>
                /tmp
            </casedir>

            <controldatadir>
                /tmp
            </controldatadir>

            <basictest>

                <short_descr>
                    Max error over entire temporal and spatial domain
                </short_descr>

                <type>
                    max_error
                </type>

                <max_threshold>
                   1.0E-3
                </max_threshold>
            </basictest>

            <basictest>
                <short_descr>
                    RMS error over entire temporal and spatial domain
                </short_descr>

                <type>
                    rmse
                </type>

                <max_threshold>
                   1.0E-6
                </max_threshold>
            </basictest>

        </runcase>


    </metcase>


</distribution>
        """
        return the_xml


    @classmethod
    def _sample_xml_2(self):
        the_xml = """<distribution>

    <!-- Sample XML testing namelist -->

    <short_descr>
        Basic distro
    </short_descr>

    <distropath>
        /tmp
    </distropath>

    <makefilepath>
        /bin/ls
    </makefilepath>

    <parmodpath>
        /bin/ls
    </parmodpath>
    
    <execname>
        A_FLEXPART_EXECUTABLE
    </execname>    
    
    <metcase>

        <short_descr>
            ECMWF patch
        </short_descr>

        <metfiledir>
            /tmp
        </metfiledir>

        <runcase>
            <short_descr>
                Run case #1
            </short_descr>

            <casedir>
                /tmp
            </casedir>

            <controldatadir>
                /tmp
            </controldatadir>

            <basictest>

                <short_descr>
                    Max error over entire temporal and spatial domain
                </short_descr>

                <type>
                    max_error
                </type>

                <max_threshold>
                   1.0E-3
                </max_threshold>
            </basictest>

            <basictest>
                <short_descr>
                    RMS error over entire temporal and spatial domain
                </short_descr>

                <type>
                    rmse
                </type>

                <max_threshold>
                   1.0E-6
                </max_threshold>
            </basictest>

        </runcase>


    </metcase>

    <metcase>

        <short_descr>
            GFS Some date
        </short_descr>

        <metfiledir>
            /tmp
        </metfiledir>

        <runcase>
            <short_descr>
                Run case #1
            </short_descr>

            <casedir>
                /tmp
            </casedir>

            <controldatadir>
                /tmp
            </controldatadir>

            <basictest>

                <short_descr>
                    Max error over entire temporal and spatial domain
                </short_descr>

                <type>
                    max_error
                </type>

                <max_threshold>
                   1.0E-3
                </max_threshold>
            </basictest>

            <basictest>
                <short_descr>
                    RMS error over entire temporal and spatial domain
                </short_descr>

                <type>
                    rmse
                </type>

                <max_threshold>
                   1.0E-6
                </max_threshold>
            </basictest>

        </runcase>

        <runcase>
            <short_descr>
                Run case #2 of Met case #2 of second distribution
            </short_descr>

            <casedir>
                /tmp
            </casedir>

            <controldatadir>
                /tmp
            </controldatadir>

            <basictest>

                <short_descr>
                    Max error over entire temporal and spatial domain
                </short_descr>

                <type>
                    max_error
                </type>

                <max_threshold>
                   1.0E-3
                </max_threshold>
            </basictest>

            <basictest>
                <short_descr>
                    RMS error over entire temporal and spatial domain
                </short_descr>

                <type>
                    rmse2
                </type>

                <max_threshold>
                   1.0E-6
                </max_threshold>
            </basictest>

        </runcase>

    </metcase>

</distribution>
        """
        return the_xml









if __name__ == '__main__':
    unittest.main()
