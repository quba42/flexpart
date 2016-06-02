


import logging
import os
import unittest

import numpy as np

import flextest.flexread.FlexpartOutput as Flexout

from test.unittest_data import __file__ as test_data_dir

class FlexpartOutputTestCase(unittest.TestCase):

    

    logging.disable(logging.CRITICAL)
    test_data_dir = os.path.dirname(test_data_dir)


    """Test flex output directories"""
    _timestamp_list = ['20140919010000', '20140919020000',
                       '20140919030000'] 

    _forward_tiny = {'flexout_dir' : test_data_dir + '/flexout_forward_tiny',
                     'timestamps' : _timestamp_list}

    _forward_tiny_complex = {'flexout_dir' :
                                test_data_dir + '/flexout_forward_tiny_complex_withnest',
                                'timestamps' : _timestamp_list}

    _backward_tiny_complex = {'flexout_dir' :
                                test_data_dir + '/flexout_backward_tiny_complex_withnest',
                                'timestamps' : _timestamp_list}    


    def test_init(self):

        """Test simple initialization"""

        the_output = Flexout.FlexpartOutput(output_dir=self._forward_tiny['flexout_dir'])

        # Just check that a valid object was returned
        self.assertIsNotNone(the_output, msg='constructor test')
       

    def test_init_raises_ioerror_if_bad_dir_arg(self):
        
        """Test that the initialization will end with a IOError exception
        if there is a bad argument for the data dir"""
        
        with self.assertRaises(IOError):
            the_output = Flexout.FlexpartOutput(output_dir='xyz123')



    def test_get_horiz_slice_good_grid_forward_tiny(self):

        """Test that we can extract a simple 4x3 grid from forward_tiny case,
           last timestep"""

        the_output = Flexout.FlexpartOutput(output_dir=
                                       self._forward_tiny['flexout_dir'])

        expected_slice = np.zeros( (4,3), dtype=np.float64)
        expected_slice[1,1] = 0.40171927
        expected_slice[2,1] = 0.12121147

        the_slice = the_output.get_horiz_slice(timestamp=
                                                  self._timestamp_list[-1])

        self.assertTrue( np.allclose(the_slice, expected_slice, atol=1.0e-7) )

    def test_get_horiz_slice_good_grid_forward_tiny_default_timestamp(self):

        """Test that we can extract a simple 4x3 grid from forward_tiny case,
           no timestep specified"""

        the_output = Flexout.FlexpartOutput(output_dir=
                                       self._forward_tiny['flexout_dir'])

        expected_slice = np.zeros( (4,3), dtype=np.float64)
        expected_slice[1,1] = 0.24117851
        expected_slice[2,1] = 0.1118383

        the_slice = the_output.get_horiz_slice()
        #print the_slice

        self.assertTrue( np.allclose(the_slice, expected_slice, atol=1.0e-7) )



    
    def test_get_horiz_slice_bad_level_forward_tiny(self):

        """Test that get_horiz_slice() returns None if we specify a bad level"""

        the_output = Flexout.FlexpartOutput(output_dir=
                                       self._forward_tiny['flexout_dir'])

        the_slice = the_output.get_horiz_slice(timestamp=
                                                  self._timestamp_list[-1],
                                               level=5555)

        self.assertIsNone( the_slice )


    def test_get_full_volume_forward_tiny(self):

        """Test that we can extract a simple 4x3 grid from forward_tiny case,
           last timestep"""

        the_output = Flexout.FlexpartOutput(output_dir=
                                       self._forward_tiny['flexout_dir'])

        expected_volume = np.zeros( (4,3, 2), dtype=np.float64)
        expected_volume[1,1,0] = 0.24117851
        expected_volume[2,1,0] = 0.1118383
        expected_volume[1,1,1] = 0.05572807
        expected_volume[2,1,1] = 0.00273365       

        the_volume = the_output.get_volume(timestamp=self._timestamp_list[-2])

        self.assertTrue( np.allclose(the_volume, expected_volume, atol=1.0e-6) )


    def test_get_full_volume_forward_tiny_default_timestamp(self):

        """Test that we can extract a simple 4x3 grid from forward_tiny case,
           no timestep specified"""

        the_output = Flexout.FlexpartOutput(output_dir=
                                       self._forward_tiny['flexout_dir'])

        expected_volume = np.zeros( (4,3, 2), dtype=np.float64)
        expected_volume[1,1,0] = 0.24117851
        expected_volume[2,1,0] = 0.1118383
        expected_volume[1,1,1] = 0.05572807
        expected_volume[2,1,1] = 0.00273365       

        the_volume = the_output.get_volume()
        #print the_volume

        self.assertTrue( np.allclose(the_volume, expected_volume, atol=1.0e-6) )




    def test_get_level_list_volume_forward_tiny(self):

        """Test that we can extract a simple 4x3 grid from forward_tiny case,
           second to last timestep.  Use a variety of level_lists.  Includes
           a case that has a bad level list"""



        the_output = Flexout.FlexpartOutput(output_dir=
                                       self._forward_tiny['flexout_dir'])

        level_list = [1,2]
        expected_volume = np.zeros( (4,3, 2), dtype=np.float64)
        expected_volume[1,1,0] = 0.24117851
        expected_volume[2,1,0] = 0.1118383
        expected_volume[1,1,1] = 0.05572807
        expected_volume[2,1,1] = 0.00273365       

        the_volume = the_output.get_volume(timestamp=self._timestamp_list[-2],
                                           level_list=level_list)

        self.assertTrue( np.allclose(the_volume, expected_volume, atol=1.0e-6),
                         msg='level_list=[1,2]')


        level_list = [1]
        expected_volume = np.zeros( (4,3, 1), dtype=np.float64)
        expected_volume[1,1,0] = 0.24117851
        expected_volume[2,1,0] = 0.1118383 

        the_volume = the_output.get_volume(timestamp=self._timestamp_list[-2],
                                           level_list=level_list)

        self.assertTrue( np.allclose(the_volume, expected_volume, atol=1.0e-6),
                         msg='level_list=[1]')

        # Bad level list - insure it returns None
        level_list = [1,3]

        the_volume = the_output.get_volume(timestamp=self._timestamp_list[-2],
                                           level_list=level_list)


        self.assertIsNone( the_volume, msg="test of bad level list" )

    def test_get_horiz_timeseries_full_timelist_forward_tiny(self):

        """Test that we can extract a full timeseries of simple 4x3 grids from
        forward_tiny case"""

        the_output = Flexout.FlexpartOutput(output_dir=
                                       self._forward_tiny['flexout_dir'])


        # Set up expected values for the full timeseries - there are three
        # timesteps for this case

        # create a 3 x 4 x 3 NumPy array (t, x, y)
        expected_timeseries = np.zeros( (3, 4, 3), dtype=np.float64 )
        expected_timeseries[0,1,1] = 0.07111748
        expected_timeseries[0,2,1] = 0.09205926
        expected_timeseries[1,1,1] = 0.24117851
        expected_timeseries[1,2,1] = 0.1118383
        expected_timeseries[2,1,1] = 0.40171927
        expected_timeseries[2,2,1] = 0.12121147        
        
        the_horiz_timeseries = the_output.get_horiz_timeseries()
        #print the_horiz_timeseries


        self.assertTrue( np.allclose(the_horiz_timeseries,
                                     expected_timeseries,
                                     atol=1.0e-6) )
        #self.assertFalse(True)

    def test_get_horiz_timeseries_partial_timelist_forward_tiny(self):

        """Test that we can extract a partial timeseries of simple 4x3 grids from
        forward_tiny case"""

        the_output = Flexout.FlexpartOutput(output_dir=
                                       self._forward_tiny['flexout_dir'])


        # Set up expected values for the full timeseries - there are three
        # timesteps for this case

        # create a 2 x 4 x 3 NumPy array (t, x, y)
        expected_timeseries = np.zeros( (2, 4, 3), dtype=np.float64 )
        expected_timeseries[0,1,1] = 0.07111748
        expected_timeseries[0,2,1] = 0.09205926
        expected_timeseries[1,1,1] = 0.40171927
        expected_timeseries[1,2,1] = 0.12121147        
        
        tlist = ['20140919010000','20140919030000']
        the_horiz_timeseries = the_output.get_horiz_timeseries(timestamp_list=tlist)
        #print the_horiz_timeseries


        self.assertTrue( np.allclose(the_horiz_timeseries,
                                     expected_timeseries,
                                     atol=1.0e-6) )


    def test_get_volume_timeseries_full_timelist_forward_tiny(self):

        """Test that we can extract a full timeseries of simple 4x3x2 grids from
        forward_tiny case"""

        the_output = Flexout.FlexpartOutput(output_dir=
                                       self._forward_tiny['flexout_dir'])


        # Set up expected values for the full timeseries - there are three
        # timesteps for this case

        # create a 3 x 4 x 3 x 2 NumPy array (t, x, y, z)
        expected_timeseries = np.zeros( (3, 4, 3, 2), dtype=np.float64 )

        expected_timeseries[0,1,1,0] = 0.07111748
        expected_timeseries[0,1,1,1] = 0.01441978
        expected_timeseries[0,2,1,0] = 0.09205926
        expected_timeseries[0,2,1,1] = 0.00105805
        
        expected_timeseries[1,1,1,0] = 0.24117851
        expected_timeseries[1,1,1,1] = 0.05572807
        expected_timeseries[1,2,1,0] = 0.1118383
        expected_timeseries[1,2,1,1] = 0.00273365        
        
        expected_timeseries[2,1,1,0] = 0.40171927
        expected_timeseries[2,1,1,1] = 0.09976709
        expected_timeseries[2,2,1,0] = 0.12121147
        expected_timeseries[2,2,1,1] = 0.00522563             
       
        
        the_volume_timeseries = the_output.get_volume_timeseries()
        #print the_volume_timeseries


        self.assertTrue( np.allclose(the_volume_timeseries,
                                     expected_timeseries,
                                     atol=1.0e-6) )


    def test_get_volume_timeseries_partial_timelist_forward_tiny(self):

        """Test that we can extract a partial timeseries of simple 4x3x2 grids from
        forward_tiny case"""

        the_output = Flexout.FlexpartOutput(output_dir=
                                       self._forward_tiny['flexout_dir'])


        # Set up expected values for the full timeseries - there are three
        # timesteps for this case

        # create a 3 x 4 x 3 x 2 NumPy array (t, x, y, z)
        expected_timeseries = np.zeros( (2, 4, 3, 2), dtype=np.float64 )

        expected_timeseries[0,1,1,0] = 0.07111748
        expected_timeseries[0,1,1,1] = 0.01441978
        expected_timeseries[0,2,1,0] = 0.09205926
        expected_timeseries[0,2,1,1] = 0.00105805
        
        expected_timeseries[1,1,1,0] = 0.40171927
        expected_timeseries[1,1,1,1] = 0.09976709
        expected_timeseries[1,2,1,0] = 0.12121147
        expected_timeseries[1,2,1,1] = 0.00522563             
       
        tlist = ['20140919010000','20140919030000']        
        the_volume_timeseries = the_output.get_volume_timeseries(
                                                    timestamp_list=tlist)
        #print the_volume_timeseries


        self.assertTrue( np.allclose(the_volume_timeseries,
                                     expected_timeseries,
                                     atol=1.0e-6) )








    ###############################################################
    #
    # Unittests for the forward_tiny_complex test cases
    #
    ###############################################################
    
    def test_get_horiz_slice_good_grid_forward_tiny_complex(self):

        """Test that we can extract a simple 4x3 grid from
        forward_tiny_complex case,last timestep"""

        the_output = Flexout.FlexpartOutput(output_dir=
                                       self._forward_tiny_complex['flexout_dir'])


        # Default level 1, species1, release 1
        expected_slice = np.zeros( (4,3), dtype=np.float64)
        expected_slice[1,1] = 0.40096313
        expected_slice[2,1] = 0.11558438

        the_slice = the_output.get_horiz_slice(timestamp=
                                    self._timestamp_list[-1])

        self.assertTrue( np.allclose(the_slice, expected_slice,
                                     atol=1.0e-7),
                                     msg="Default level 1, species 1, release 1" )


        # level 2, species 2, release 4
        expected_slice = np.zeros( (4,3), dtype=np.float64)
        expected_slice[2,1] = 0.00072793
        expected_slice[3,1] = 0.08195718

        the_slice = the_output.get_horiz_slice(timestamp=
                                    self._timestamp_list[-1],
                                               level=2,
                                               species=2,
                                               release=4)
        
        self.assertTrue( np.allclose(the_slice, expected_slice,
                                     atol=1.0e-7),
                                     msg="level 2, species 2, release 4" )

    def test_get_dry_deposition_good_grid_forward_tiny_complex(self):

        """Test that we can extract a simple 4x3 dry deposition grid from
        forward_tiny_complex case,last timestep"""

        the_output = Flexout.FlexpartOutput(output_dir=
                                       self._forward_tiny_complex['flexout_dir'])


        # Default level 1, species1, release 1
        expected_depo = np.zeros( (4,3), dtype=np.float64)

        the_depo = the_output.get_deposition(timestamp=
                                    self._timestamp_list[-1])

        # print the_depo
        self.assertTrue( np.allclose(the_depo, expected_depo,
                                     atol=1.0e-7),
                                     msg="Dry depo, species 1, release 1" )


        # level 2, species 2, release 4
        expected_depo = np.zeros( (4,3), dtype=np.float64)
        expected_depo[2,0] = 4.71624645e-07
        expected_depo[2,1] = 4.77216035e-01
        expected_depo[2,2] = 9.23220664e-02
        expected_depo[3,0] = 3.87174950e-05
        expected_depo[3,1] = 4.09034729e+00
        expected_depo[3,2] = 4.36384439e-01       
        
        
        the_depo = the_output.get_deposition(timestamp=
                                    self._timestamp_list[-1],
                                               species=2,
                                               release=4,
                                               depo_type='dry')

        #print the_depo        
        self.assertTrue( np.allclose(the_depo, expected_depo,
                                     atol=1.0e-7),
                                     msg="Dry depo, species 2, release 4" )
                                     
    def test_get_dry_deposition_good_grid_forward_tiny_complex_default_timestamp(self):

        """Test that we can extract a simple 4x3 dry deposition grid from
        forward_tiny_complex case, no timestep specified"""

        the_output = Flexout.FlexpartOutput(output_dir=
                                       self._forward_tiny_complex['flexout_dir'])


        # Default level 1, species1, release 1
        expected_depo = np.zeros( (4,3), dtype=np.float64)

        the_depo = the_output.get_deposition()

        #print the_depo
        self.assertTrue( np.allclose(the_depo, expected_depo,
                                     atol=1.0e-7),
                                     msg="Dry depo, species 1, release 1" )


        # level 2, species 2, release 4
        expected_depo = np.zeros( (4,3), dtype=np.float64)
        expected_depo[2,0] = 1.49522535e-07
        expected_depo[2,1] = 1.99161291e-01
        expected_depo[2,2] = 2.73171775e-02
        expected_depo[3,0] = 2.97258084e-05
        expected_depo[3,1] = 2.40028429e+00
        expected_depo[3,2] =  1.91489279e-01      
        
        
        the_depo = the_output.get_deposition( species=2,
                                               release=4,
                                               depo_type='dry')

        #print the_depo        
        self.assertTrue( np.allclose(the_depo, expected_depo,
                                     atol=1.0e-7),
                                     msg="Dry depo, species 2, release 4" )                                     
                                     
                                     

    def test_get_horiz_slice_good_grid_forward_tiny_complex_nest(self):

        """Test that we can extract a simple 10x5 grid nest from
        forward_tiny_complex case,last timestep"""

        the_output = Flexout.FlexpartOutput(output_dir=
                                       self._forward_tiny_complex['flexout_dir'],
                                            nest=True)


        # Default level 1, species1, release 1
        expected_slice = np.zeros( (10,5), dtype=np.float64)
        expected_slice[3,2] = 0.04524897
        expected_slice[3,3] = 0.48730424
        expected_slice[4,2] = 6.31771755
        expected_slice[4,3] = 3.18699765
        expected_slice[5,2] = 2.72790289
        expected_slice[5,3] = 0.16338137


        the_slice = the_output.get_horiz_slice(timestamp=
                                    self._timestamp_list[-1])

        #print the_slice

        self.assertTrue( np.allclose(the_slice, expected_slice,
                                     atol=1.0e-7),
                                     msg="Default level 1, species 1, release 1" )





    def test_get_full_volume_forward_tiny_complex(self):

        """Test that we can extract a simple 4x3 grid from
        forward_tiny_complex case,
           last timestep"""

        the_output = Flexout.FlexpartOutput(output_dir=
                                       self._forward_tiny_complex['flexout_dir'])

        expected_volume = np.zeros( (4,3, 5), dtype=np.float64)
        expected_volume[1,1,0] = 0.24113688
        expected_volume[2,1,0] = 0.11248351
        expected_volume[1,1,1] = 0.05435027
        expected_volume[2,1,1] = 0.00392907       

        the_volume = the_output.get_volume(timestamp=self._timestamp_list[-2])
        #print the_volume

        self.assertTrue( np.allclose(the_volume,
                                     expected_volume,
                                     atol=1.0e-6),
                                     msg='Default species 1, release 1')


    ###############################################################
    #
    # Unittests for the backward_tiny_complex test cases
    #
    ###############################################################

    def test_get_horiz_slice_good_grid_backward_tiny_complex(self):

        """Test that we can extract a simple 4x3 grid from
        backward_tiny_complex case,first timestep"""

        the_output = Flexout.FlexpartOutput(output_dir=
                                       self._backward_tiny_complex['flexout_dir'])


        # Default level 1, species1, release 1
        expected_slice = np.zeros( (4,3), dtype=np.float64)
        expected_slice[1,1] = 56.87136841
        expected_slice[2,1] = 1011.26373291


        the_slice = the_output.get_horiz_slice(timestamp=
                                    self._timestamp_list[0])
    
        #print the_slice

        self.assertTrue( np.allclose(the_slice, expected_slice,
                                     atol=1.0e-7),
                                     msg="Default level 1, species 1, release 1" )



        # level 2, species 2, release 4
        expected_slice = np.zeros( (4,3), dtype=np.float64)
        expected_slice[3,1] = 560.6885376

        the_slice = the_output.get_horiz_slice(timestamp=
                                    self._timestamp_list[0],
                                               level=2,
                                               species=2,
                                               release=4)

        #print the_slice        
        self.assertTrue( np.allclose(the_slice, expected_slice,
                                     atol=1.0e-7),
                                     msg="level 2, species 2, release 4" )



    def test_get_horiz_slice_good_grid_backward_tiny_complex_nest(self):

        """Test that we can extract a simple 10x5 grid nest from
        backward_tiny_complex case, first timestep"""

        the_output = Flexout.FlexpartOutput(output_dir=
                                       self._backward_tiny_complex['flexout_dir'],
                                            nest=True)


        # Default level 1, species1, release 1
        expected_slice = np.zeros( (10,5), dtype=np.float64)
        expected_slice[4,1] = 2.00158691
        expected_slice[4,2] = 54.86978531
        expected_slice[5,1] = 73.73590851
        expected_slice[5,2] = 931.95013428
        expected_slice[6,1] = 4.38299847
        expected_slice[6,2] = 1.19588506


        the_slice = the_output.get_horiz_slice(timestamp=
                                    self._timestamp_list[0])

        #print the_slice

        self.assertTrue( np.allclose(the_slice, expected_slice,
                                     atol=1.0e-7),
                                     msg="Default level 1, species 1, release 1" )



    def test_get_full_volume_backward_tiny_complex(self):

        """Test that we can extract a simple 4x3 grid from
        backward_tiny_complex case,
           first timestep"""

        the_output = Flexout.FlexpartOutput(output_dir=
                                       self._backward_tiny_complex['flexout_dir'])

        expected_volume = np.zeros( (4,3, 5), dtype=np.float64)
        expected_volume[1,1,0] = 56.87136841
        expected_volume[2,1,0] = 1011.26373291
        expected_volume[1,1,1] = 57.710289
        expected_volume[2,1,1] = 607.09680176      

        the_volume = the_output.get_volume(timestamp=self._timestamp_list[0])
        #print the_volume

        self.assertTrue( np.allclose(the_volume,
                                     expected_volume,
                                     atol=1.0e-6),
                                     msg='Default species 1, release 1')


    def test_get_timestamp_list(self):
        """
        Tests that valid timestamp list is returned
        """
    
        the_output = Flexout.FlexpartOutput(output_dir=
                                       self._forward_tiny['flexout_dir'])        

        expected_timestamps = self._forward_tiny['timestamps']
        the_timestamps = the_output.get_timestamp_list()
        
        identical_lists = expected_timestamps == the_timestamps
        self.assertTrue(identical_lists)

if __name__=='__main__':
    unittest.main()

