# -*- coding: utf-8 -*-
"""
Created on Wed Aug 12 00:34:57 2015

@author: morton

Don Morton
Boreal Scientific Computing LLC, Fairbanks, Alaska, USA
Don.Morton@borealscicomp.com
"""

import os
import shutil
import tempfile
import unittest

import distrotest.BasicTest as BasicTest
import distrotest.RunCase as RunCase
import distrotest.MetCase as MetCase
import distrotest.Distribution as Distribution

class test_Distribution(unittest.TestCase):

    @classmethod
    def setUpClass(cls):

        """Create temp directories and MetCase lists for testing"""
        
        # Create a MetCase list of two tests.  Note, will just use /tmp as
        # the case_dir and control_data_dir, since I'm not doing anything 
        # with it in this test
        cls._met_case_list = []

        description = 'Met Case 1'
        metfile_dir = '/tmp'    
        

        mc = MetCase.MetCase(descr=description, metfile_dir=metfile_dir)
        cls._met_case_list.append(mc)

        description = 'Met Case 2'
        metfile_dir = '/tmp'


        mc = MetCase.MetCase(descr=description, metfile_dir=metfile_dir)
        cls._met_case_list.append(mc)

            
        # Create a temporary distro directory with a par_mod.f90 - won't do anything
        # in them, just want to test that routines find or not find them
        cls._distro_dir = tempfile.mkdtemp(prefix='distrodir_', dir='/tmp')   

        # Create a par_mod.f90 file in the distro_dir
        cls._parmod_file = cls._distro_dir + '/par_mod.f90'
        open(cls._parmod_file, 'w').close()
                             


        # Create a makefile (won't do anything with it)
        #os.mkdir(cls._availablefile_dir + '/AVAILABLE')
        cls._makefile_path = '/tmp/makefile.gfortran'
        open(cls._makefile_path, 'w').close()
        
        # Create an executable name - won't do anything with it, just test
        # that routines find it there.
        cls._exec_name = 'FLEXPART_EXECUTABLE'
        



    def test_works_with_good_args(self):

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



    def test_raises_exception_if_no_distro_path_arg(self):


        description = 'Distribution Test'
        distro_path = self._distro_dir
        makefile_path = self._makefile_path
        parmod_path = self._parmod_file
        exec_name = self._exec_name        
        met_case_list = self._met_case_list

        with self.assertRaises(Exception):
            dist = Distribution.Distribution(descr=description, 
                                             makefile_path=makefile_path,
                                             parmod_path=parmod_path,
                                             exec_name=exec_name,
                                             met_case_list=met_case_list)





      
    def test_raises_exception_if_bad_distro_path(self):


        description = 'Distribution Test'
        distro_path = self._distro_dir + '/zzz'  # bad path
        makefile_path = self._makefile_path
        parmod_path = self._parmod_file
        exec_name = self._exec_name        
        met_case_list = self._met_case_list

        with self.assertRaises(Exception):
            dist = Distribution.Distribution(descr=description,
                                             distro_path=distro_path,
                                             makefile_path=makefile_path,
                                             parmod_path=parmod_path,
                                             exec_name=exec_name,
                                             met_case_list=met_case_list)





    '''
    # DJM - 2016-01-08 -- changes were made such that in some cases it's ok not to have a
    # makefile path defined in the XML file.  Therefore, this test is currently not valid
    # and can likely be removed at any time.
    def test_raises_exception_if_no_makefile_path_arg(self):

        description = 'Distribution Test'
        distro_path = self._distro_dir
        makefile_path = self._makefile_path
        parmod_path = self._parmod_file
        exec_name = self._exec_name          
        met_case_list = self._met_case_list

        with self.assertRaises(Exception):
            dist = Distribution.Distribution(descr=description,
                                             distro_path=distro_path,
                                             parmod_path=parmod_path,
                                             exec_name=exec_name,
                                             met_case_list=met_case_list)
    '''




    def test_raises_exception_if_bad_makefile_path(self):

        description = 'Distribution Test'
        distro_path = self._distro_dir
        makefile_path = self._makefile_path + '/zzz'  # Bad path
        parmod_path = self._parmod_file
        exec_name = self._exec_name        
        met_case_list = self._met_case_list

        with self.assertRaises(Exception):
            dist = Distribution.Distribution(descr=description,
                                             distro_path=distro_path,
                                             makefile_path=makefile_path,
                                             parmod_path=parmod_path,
                                             exec_name=exec_name,
                                             met_case_list=met_case_list)



    def test_defaults_to_distro_parmod_if_no_parmod_arg(self):

        description = 'Distribution Test'
        distro_path = self._distro_dir
        parmod_path = self._parmod_file
        exec_name = self._exec_name                
        met_case_list = self._met_case_list

        dist = Distribution.Distribution(descr=description,
                                         distro_path=distro_path,
                                         exec_name=exec_name,
                                         met_case_list=met_case_list)

        expected_parmod_path = distro_path + '/par_mod.f90'
        self.assertEqual(dist.get_parmod_path(), expected_parmod_path)

    def test_raises_exception_if_bad_parmod_path(self):

        description = 'Distribution Test'
        distro_path = self._distro_dir
        makefile_path = self._makefile_path
        parmod_path = self._parmod_file + '/zzz' # bad path
        exec_name = self._exec_name
        met_case_list = self._met_case_list

        with self.assertRaises(Exception):
            dist = Distribution.Distribution(descr=description,
                                             distro_path=distro_path,
                                             makefile_path=makefile_path,
                                             parmod_path=parmod_path,
                                             exec_name=exec_name,
                                             met_case_list=met_case_list)


    def test_raises_exception_if_no_exec_name_arg(self):

        description = 'Distribution Test'
        distro_path = self._distro_dir
        makefile_path = self._makefile_path
        parmod_path = self._parmod_file       
        met_case_list = self._met_case_list

        with self.assertRaises(Exception):
            dist = Distribution.Distribution(descr=description,
                                             distro_path=distro_path,
                                             parmod_path=parmod_path,
                                             met_case_list=met_case_list)

    def test_returns_empty_met_case_list_if_no_met_case_list_arg(self):


        description = 'Distribution Test'
        distro_path = self._distro_dir
        parmod_path = self._parmod_file
        exec_name = self._exec_name        
        met_case_list = self._met_case_list


        dist = Distribution.Distribution(descr=description,
                                         distro_path=distro_path,
                                         parmod_path=parmod_path,
                                         exec_name=exec_name,)


        # Testing that returned list is empty (or False)
        self.assertFalse(dist.get_met_case_list() )



    @classmethod
    def tearDownClass(cls):
        """Remove the temp directories created in these tests"""

        if os.path.isdir(cls._distro_dir):
            shutil.rmtree(cls._distro_dir)

        if os.path.isfile(cls._makefile_path):
            os.remove(cls._makefile_path)

if __name__ == '__main__':
    unittest.main()
