# -*- coding: utf-8 -*-
"""
Created on Tue May  5 15:28:08 2015

@author: morton
"""

import logging
import os
import unittest

import numpy as np

import flextest.flexread.FlexpartOutput as Flexout
import flextest.FlexpartErrors as FlexErr

from test.unittest_data import __file__ as test_data_dir

class FlexpartErrorsTestCase(unittest.TestCase):

    

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
                                
    _ncepforward_tiny_complex = {'flexout_dir' :
                                test_data_dir + '/ncepflexout_forward_tiny_complex_withnest',
                                'timestamps' : _timestamp_list}

    _ncepbackward_tiny_complex = {'flexout_dir' :
                                test_data_dir + '/ncepflexout_backward_tiny_complex_withnest',
                                'timestamps' : _timestamp_list}                                    
                                

    def test_init(self):

        """Test simple initialization"""

        control_output = Flexout.FlexpartOutput(output_dir=self._forward_tiny['flexout_dir'])
        test_output = Flexout.FlexpartOutput(output_dir=self._forward_tiny['flexout_dir'])

        error_object = FlexErr.FlexpartErrors(control=control_output,
                                              test=test_output)

        # Just check that a valid object was returned
        self.assertIsNotNone(error_object, msg='constructor test')

    def test_get_diff_grid_simple_slice(self):

        """Test retrieval of simple diff_grid slice"""

        control_output = Flexout.FlexpartOutput(output_dir=self._forward_tiny['flexout_dir'])
        test_output = Flexout.FlexpartOutput(output_dir=self._forward_tiny['flexout_dir'])

        error_object = FlexErr.FlexpartErrors(control=control_output,
                                              test=test_output)

        expected_diff_grid = np.zeros( (4,3), dtype=np.float64)
        diff_grid = error_object.get_diff_grid()
        #print diff_grid

        self.assertTrue( np.allclose(diff_grid, expected_diff_grid, 
                                     atol=1.0e-7) )

    def test_get_diff_grid_ncep_ecmwf_slice(self):

        """Test retrieval of simple diff_grid slice with ecmwf as control
        and ncep as test"""

        control_output = Flexout.FlexpartOutput(output_dir=self._forward_tiny_complex['flexout_dir'])
        test_output = Flexout.FlexpartOutput(output_dir=self._ncepforward_tiny_complex['flexout_dir'])

        error_object = FlexErr.FlexpartErrors(control=control_output,
                                              test=test_output)

        expected_diff_grid = np.zeros( (4,3), dtype=np.float64)
        expected_diff_grid[1,1] = -0.09765793
        expected_diff_grid[2,1] = 0.18723705    
        
        diff_grid = error_object.get_diff_grid()
        #print diff_grid

        self.assertTrue( np.allclose(diff_grid, expected_diff_grid, 
                                     atol=1.0e-7) )


                           
 
    def test_get_diff_grid_complex_slice_wet_depo(self):

        """Test retrieval of complex diff grid slice with wet depo"""

        control_output = Flexout.FlexpartOutput(output_dir=self._forward_tiny_complex['flexout_dir'])
        test_output = Flexout.FlexpartOutput(output_dir=self._forward_tiny_complex['flexout_dir'])

        error_object = FlexErr.FlexpartErrors(control=control_output,
                                              test=test_output)

        expected_diff_grid = np.zeros( (4,3), dtype=np.float64)

        diff_grid = error_object.get_diff_grid(wet=True)
        #print diff_grid

        self.assertTrue( np.allclose(diff_grid, expected_diff_grid, 
                                     atol=1.0e-7) )

        diff_grid = error_object.get_diff_grid(wet=True, species=2, release=4)
        #print diff_grid

        self.assertTrue( np.allclose(diff_grid, expected_diff_grid, 
                                     atol=1.0e-7) )


    def test_get_diff_grid_complex_slice_dry_depo(self):

        """Test retrieval of complex diff grid slice with dry depo"""

        control_output = Flexout.FlexpartOutput(
                output_dir=self._forward_tiny_complex['flexout_dir'])
        test_output = Flexout.FlexpartOutput(
                output_dir=self._forward_tiny_complex['flexout_dir'])

        error_object = FlexErr.FlexpartErrors(control=control_output,
                                              test=test_output)

        expected_diff_grid = np.zeros( (4,3), dtype=np.float64)

        diff_grid = error_object.get_diff_grid(dry=True)
        #print diff_grid

        self.assertTrue( np.allclose(diff_grid, expected_diff_grid, 
                                     atol=1.0e-7) )

        diff_grid = error_object.get_diff_grid(timestamp='20140919030000',
                                               dry=True, species=2, release=4)
        #print diff_grid

        self.assertTrue( np.allclose(diff_grid, expected_diff_grid, 
                                     atol=1.0e-7) )

    def test_get_diff_grid_simple_volume(self):

        """Test retrieval of simple diff_grid volume"""

        control_output = Flexout.FlexpartOutput(
                output_dir=self._forward_tiny['flexout_dir'])
        test_output = Flexout.FlexpartOutput(
                output_dir=self._forward_tiny['flexout_dir'])

        error_object = FlexErr.FlexpartErrors(control=control_output,
                                              test=test_output)

        expected_diff_grid = np.zeros( (4,3,2), dtype=np.float64)
        diff_grid = error_object.get_diff_grid(volume=True)
        #print diff_grid

        self.assertTrue( np.allclose(diff_grid, expected_diff_grid, 
                                     atol=1.0e-7) )


    def test_get_diff_grid_complex_volume(self):

        """Test retrieval of complex diff_grid volume"""

        control_output = Flexout.FlexpartOutput(
                output_dir=self._forward_tiny_complex['flexout_dir'])
        test_output = Flexout.FlexpartOutput(
                output_dir=self._forward_tiny_complex['flexout_dir'])

        error_object = FlexErr.FlexpartErrors(control=control_output,
                                              test=test_output)

        expected_diff_grid = np.zeros( (4,3,5), dtype=np.float64)

        diff_grid = error_object.get_diff_grid(volume=True)


        self.assertTrue( np.allclose(diff_grid, expected_diff_grid, 
                                     atol=1.0e-7), msg='All levels' )

        
        expected_diff_grid = np.zeros( (4,3,3), dtype=np.float64)                                     
        diff_grid = error_object.get_diff_grid(volume=True,
                                               level_list=[1,2,4])


        self.assertTrue( np.allclose(diff_grid, expected_diff_grid, 
                                     atol=1.0e-7), msg='levels 1,2,4' )                                     


        expected_diff_grid = np.zeros( (4,3,3), dtype=np.float64)                                     
        diff_grid = error_object.get_diff_grid(volume=True,
                                               level_list=[1,2,4],
                                                species=2,
                                                release=4)


        self.assertTrue( np.allclose(diff_grid, expected_diff_grid, 
                                     atol=1.0e-7), 
                                     msg='levs 1,2,4, spec 2, rel 4' )   
                               
    def test_get_diff_grid_simple_horiz_timeseries(self):

        """Test retrieval of simple diff_grid horiz timeseries"""

        control_output = Flexout.FlexpartOutput(
                output_dir=self._forward_tiny['flexout_dir'])
        test_output = Flexout.FlexpartOutput(
                output_dir=self._forward_tiny['flexout_dir'])

        error_object = FlexErr.FlexpartErrors(control=control_output,
                                              test=test_output)

        expected_diff_grid = np.zeros( (3,4,3), dtype=np.float64)
        diff_grid = error_object.get_diff_grid(timeseries=True)
        #print diff_grid

        self.assertTrue( np.allclose(diff_grid, expected_diff_grid, 
                                     atol=1.0e-7) )

    def test_get_diff_grid_complex_horiz_timeseries(self):

        """Test retrieval of complex diff_grid horiz_timeseries"""

        control_output = Flexout.FlexpartOutput(
                output_dir=self._forward_tiny_complex['flexout_dir'])
        test_output = Flexout.FlexpartOutput(
                output_dir=self._forward_tiny_complex['flexout_dir'])

        error_object = FlexErr.FlexpartErrors(control=control_output,
                                              test=test_output)

        expected_diff_grid = np.zeros( (3,4,3), dtype=np.float64)

        diff_grid = error_object.get_diff_grid(timeseries=True)


        self.assertTrue( np.allclose(diff_grid, expected_diff_grid, 
                                     atol=1.0e-7), msg='All times' )

        
        expected_diff_grid = np.zeros( (2,4,3), dtype=np.float64)                                     
        diff_grid = error_object.get_diff_grid(timeseries=True,
                                               timestamp_list=
                                               ['20140919010000',
                                               '20140919030000'])

        #print diff_grid
        self.assertTrue( np.allclose(diff_grid, expected_diff_grid, 
                                     atol=1.0e-7), msg='times 1,3' )                                     


        expected_diff_grid = np.zeros( (2,4,3), dtype=np.float64)                                     
        diff_grid = error_object.get_diff_grid(timeseries=True,
                                               timestamp_list=
                                               ['20140919010000',
                                               '20140919030000'],
                                                species=2,
                                                release=4)



    def test_get_diff_grid_complex_deposition_timeseries(self):

        """Test retrieval of complex diff_grid deposition timeseries"""

        control_output = Flexout.FlexpartOutput(
                output_dir=self._forward_tiny_complex['flexout_dir'])
        test_output = Flexout.FlexpartOutput(
                output_dir=self._forward_tiny_complex['flexout_dir'])

        error_object = FlexErr.FlexpartErrors(control=control_output,
                                              test=test_output)

        expected_diff_grid = np.zeros( (3,4,3), dtype=np.float64)

        diff_grid = error_object.get_diff_grid(timeseries=True,wet=True)


        self.assertTrue( np.allclose(diff_grid, expected_diff_grid, 
                                     atol=1.0e-7), msg='All times' )

   
        expected_diff_grid = np.zeros( (2,4,3), dtype=np.float64)                                     
        diff_grid = error_object.get_diff_grid(timeseries=True,
                                               dry=True,
                                               timestamp_list=
                                               ['20140919010000',
                                               '20140919030000'])

        #print diff_grid
        self.assertTrue( np.allclose(diff_grid, expected_diff_grid, 
                                     atol=1.0e-7), msg='times 1,3' )                                     


        expected_diff_grid = np.zeros( (2,4,3), dtype=np.float64)                                     
        diff_grid = error_object.get_diff_grid(timeseries=True,
                                               dry=True,
                                               timestamp_list=
                                               ['20140919010000',
                                               '20140919030000'],
                                                species=2,
                                                release=4)





        self.assertTrue( np.allclose(diff_grid, expected_diff_grid, 
                                     atol=1.0e-7), 
                                     msg='times 1,3, spec 2, rel 4' )   




    def test_get_diff_grid_complex_volume_timeseries(self):

        """Test retrieval of complex diff_grid volume timeseries"""

        control_output = Flexout.FlexpartOutput(
                output_dir=self._forward_tiny_complex['flexout_dir'])
        test_output = Flexout.FlexpartOutput(
                output_dir=self._forward_tiny_complex['flexout_dir'])

        error_object = FlexErr.FlexpartErrors(control=control_output,
                                              test=test_output)

        expected_diff_grid = np.zeros( (3,4,3,5), dtype=np.float64)

        diff_grid = error_object.get_diff_grid(timeseries=True,
                                               volume=True)

        self.assertTrue( np.allclose(diff_grid, expected_diff_grid, 
                                     atol=1.0e-7), msg='All times' )


        expected_diff_grid = np.zeros( (2,4,3,5), dtype=np.float64)                                     
        diff_grid = error_object.get_diff_grid(timeseries=True,
                                               volume=True,
                                               timestamp_list=
                                               ['20140919010000',
                                               '20140919030000'])

        #print diff_grid
        self.assertTrue( np.allclose(diff_grid, expected_diff_grid, 
                                     atol=1.0e-7), msg='times 1,3' )   

        expected_diff_grid = np.zeros( (2,4,3,5), dtype=np.float64)                                     
        diff_grid = error_object.get_diff_grid(timeseries=True,
                                               volume=True,
                                               timestamp_list=
                                               ['20140919010000',
                                               '20140919030000'],
                                                species=2,
                                                release=4)


        self.assertTrue( np.allclose(diff_grid, expected_diff_grid, 
                                     atol=1.0e-7), 
                                     msg='times 1,3, spec 2, rel 4' )  


        expected_diff_grid = np.zeros( (2,4,3,3), dtype=np.float64)                                     
        diff_grid = error_object.get_diff_grid(timeseries=True,
                                               volume=True,
                                               level_list=[1,3,5],
                                               timestamp_list=
                                               ['20140919010000',
                                               '20140919030000'],
                                                species=2,
                                                release=4)


        self.assertTrue( np.allclose(diff_grid, expected_diff_grid, 
                                     atol=1.0e-7), 
                                     msg='times 1,3, levels 1,3,5, spec 2, rel 4' )  



    def test_mean_absolute_error_forward_tiny_complex(self):

        """Tests computation of mean absolute error"""

        control_output = Flexout.FlexpartOutput(
                output_dir=self._forward_tiny_complex['flexout_dir'])
        test_output = Flexout.FlexpartOutput(
                output_dir=self._forward_tiny_complex['flexout_dir'])

        error_object = FlexErr.FlexpartErrors(control=control_output,
                                              test=test_output)    


        expected_mae = 0.0
        mae = error_object.mean_absolute_error()
        self.assertAlmostEqual(expected_mae, mae, msg="default args")
        #print mae
        
        expected_mae = 0.0
        mae = error_object.mean_absolute_error(volume=True, timeseries=True)
        self.assertAlmostEqual(expected_mae, mae, msg="default volume, timeseries")
        #print mae        
        
        expected_mae = 0.0
        mae = error_object.mean_absolute_error(dry=True, species=2, timeseries=True)
        #print mae
        self.assertAlmostEqual(expected_mae, mae, msg="default timeseries, dry depo, species 2")


    def test_mean_absolute_error_forward_tiny_complex_ncep_ecmwf(self):

        """Tests computation of mean absolute error"""

        control_output = Flexout.FlexpartOutput(
                output_dir=self._forward_tiny_complex['flexout_dir'])
        test_output = Flexout.FlexpartOutput(
                output_dir=self._ncepforward_tiny_complex['flexout_dir'])

        error_object = FlexErr.FlexpartErrors(control=control_output,
                                              test=test_output)    


        expected_mae = 0.0237412483742
        mae = error_object.mean_absolute_error()
        #print mae
        self.assertAlmostEqual(expected_mae, mae, msg="default args")

        
        expected_mae = 0.00584379989757
        mae = error_object.mean_absolute_error(volume=True, timeseries=True)
        #print mae
        self.assertAlmostEqual(expected_mae, mae, msg="default volume, timeseries")
      
        
        expected_mae = 0.0481176506921
        mae = error_object.mean_absolute_error(dry=True, species=2, timeseries=True)
        #print mae
        self.assertAlmostEqual(expected_mae, mae, msg="default timeseries, dry depo, species 2")





    def test_max_absolute_error_forward_tiny_complex(self):

        """Tests computation of max absolute error"""

        control_output = Flexout.FlexpartOutput(
                output_dir=self._forward_tiny_complex['flexout_dir'])
        test_output = Flexout.FlexpartOutput(
                output_dir=self._forward_tiny_complex['flexout_dir'])

        error_object = FlexErr.FlexpartErrors(control=control_output,
                                              test=test_output)    


        expected_mae = 0.0
        mae = error_object.max_absolute_error()
        self.assertAlmostEqual(expected_mae, mae, msg="default args")
        #print mae
        
        expected_mae = 0.0
        mae = error_object.max_absolute_error(volume=True, timeseries=True)
        self.assertAlmostEqual(expected_mae, mae, msg="default volume, timeseries")
        #print mae        
        
        expected_mae = 0.0
        mae = error_object.max_absolute_error(dry=True, species=2, timeseries=True)
        #print mae
        self.assertAlmostEqual(expected_mae, mae, msg="default timeseries, dry depo, species 2")

    def test_max_absolute_error_forward_tiny_complex_ncep_ecmwf(self):

        """Tests computation of max absolute error"""

        control_output = Flexout.FlexpartOutput(
                output_dir=self._forward_tiny_complex['flexout_dir'])
        test_output = Flexout.FlexpartOutput(
                output_dir=self._ncepforward_tiny_complex['flexout_dir'])

        error_object = FlexErr.FlexpartErrors(control=control_output,
                                              test=test_output)    


        expected_mae = 0.187237046659
        mae = error_object.max_absolute_error()
        #print mae
        self.assertAlmostEqual(expected_mae, mae, msg="default args")
        
        expected_mae = 0.300363861024
        mae = error_object.max_absolute_error(volume=True, timeseries=True)
        #print mae
        self.assertAlmostEqual(expected_mae, mae, msg="default volume, timeseries")   
        
        expected_mae = 0.608737587929
        mae = error_object.max_absolute_error(dry=True, species=2, timeseries=True)
        #print mae
        self.assertAlmostEqual(expected_mae, mae, msg="default timeseries, dry depo, species 2")


    def test_rmse_forward_tiny_complex(self):

        """Tests computation of rmse"""

        control_output = Flexout.FlexpartOutput(
                output_dir=self._forward_tiny_complex['flexout_dir'])
        test_output = Flexout.FlexpartOutput(
                output_dir=self._forward_tiny_complex['flexout_dir'])

        error_object = FlexErr.FlexpartErrors(control=control_output,
                                              test=test_output)    


        expected_mae = 0.0
        mae = error_object.rmse()
        self.assertAlmostEqual(expected_mae, mae, msg="default args")
        #print mae
        
        expected_mae = 0.0
        mae = error_object.rmse(volume=True, timeseries=True)
        self.assertAlmostEqual(expected_mae, mae, msg="default volume, timeseries")
        #print mae        
        
        expected_mae = 0.0
        mae = error_object.rmse(dry=True, species=2, timeseries=True)
        #print mae
        self.assertAlmostEqual(expected_mae, mae, msg="default timeseries, dry depo, species 2")

    def test_rmse_forward_tiny_complex_ncep_ecmwf(self):

        """Tests computation of rmse"""

        control_output = Flexout.FlexpartOutput(
                output_dir=self._forward_tiny_complex['flexout_dir'])
        test_output = Flexout.FlexpartOutput(
                output_dir=self._ncepforward_tiny_complex['flexout_dir'])

        error_object = FlexErr.FlexpartErrors(control=control_output,
                                              test=test_output)    


        expected_rmse = 0.060960905287
        rmse = error_object.rmse()
        #print rmse
        self.assertAlmostEqual(expected_rmse, rmse, msg="default args")

        
        expected_rmse = 0.0307219910249
        rmse = error_object.rmse(volume=True, timeseries=True)
        #print rmse
        self.assertAlmostEqual(expected_rmse, rmse, msg="default volume, timeseries")    
        
        expected_rmse = 0.128813627575
        rmse = error_object.rmse(dry=True, species=2, timeseries=True)
        #print rmse
        self.assertAlmostEqual(expected_rmse, rmse, msg="default timeseries, dry depo, species 2")




    def test_rmse_backward_tiny_complex_ncep_ecmwf(self):

        """Tests computation of rmse"""

        control_output = Flexout.FlexpartOutput(
                output_dir=self._backward_tiny_complex['flexout_dir'])
        test_output = Flexout.FlexpartOutput(
                output_dir=self._ncepbackward_tiny_complex['flexout_dir'])

        error_object = FlexErr.FlexpartErrors(control=control_output,
                                              test=test_output)    


        expected_rmse = 184.487273779
        rmse = error_object.rmse()
        #print rmse
        self.assertAlmostEqual(expected_rmse, rmse, msg="default args")
        
        expected_rmse = 165.127650886
        rmse = error_object.rmse(volume=True, timeseries=True)
        #print rmse
        self.assertAlmostEqual(expected_rmse, rmse, msg="default volume, timeseries")    
        
        expected_rmse = 0.0
        rmse = error_object.rmse(dry=True, species=2, timeseries=True)
        #print rmse
        self.assertAlmostEqual(expected_rmse, rmse, msg="default timeseries, dry depo, species 2")

    def test_max_error_forward_tiny_complex_ncep_ecmwf(self):

        """Tests computation of rmse"""

        control_output = Flexout.FlexpartOutput(
                output_dir=self._forward_tiny_complex['flexout_dir'])
        test_output = Flexout.FlexpartOutput(
                output_dir=self._ncepforward_tiny_complex['flexout_dir'])

        error_object = FlexErr.FlexpartErrors(control=control_output,
                                              test=test_output)    


        expected_max_err = 0.187237046659
        max_err = error_object.max_error()
        #print max_err
        self.assertAlmostEqual(expected_max_err, max_err, msg="default args")

        
        expected_max_err = 0.300363861024
        max_err = error_object.max_error(volume=True, timeseries=True)
        #print max_err
        self.assertAlmostEqual(expected_max_err, max_err, msg="default volume, timeseries")    
        
        expected_max_err = 0.608737587929
        max_err = error_object.max_error(dry=True, species=2, timeseries=True)
        #print max_err
        self.assertAlmostEqual(expected_max_err, max_err, msg="default timeseries, dry depo, species 2")


    def test_mean_bias_forward_tiny_complex_ncep_ecmwf(self):

        """Tests computation of mean bias"""

        control_output = Flexout.FlexpartOutput(
                output_dir=self._forward_tiny_complex['flexout_dir'])
        test_output = Flexout.FlexpartOutput(
                output_dir=self._ncepforward_tiny_complex['flexout_dir'])

        error_object = FlexErr.FlexpartErrors(control=control_output,
                                              test=test_output)    


        expected_mean_bias = 0.00746492606898
        mean_bias = error_object.mean_bias()
        #print mean_bias
        self.assertAlmostEqual(expected_mean_bias, mean_bias, msg="default args")

        
        expected_mean_bias = 0.0011877637253
        mean_bias = error_object.mean_bias(volume=True, timeseries=True)
        #print mean_bias
        self.assertAlmostEqual(expected_mean_bias, mean_bias, msg="default volume, timeseries")    
        
        expected_mean_bias = 0.0481129836797
        mean_bias = error_object.mean_bias(dry=True, species=2, timeseries=True)
        #print mean_bias
        self.assertAlmostEqual(expected_mean_bias, mean_bias, msg="default timeseries, dry depo, species 2")


                                
                                
if __name__=='__main__':
    unittest.main()
                                
