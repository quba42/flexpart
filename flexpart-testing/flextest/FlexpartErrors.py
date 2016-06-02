# -*- coding: utf-8 -*-
"""
Created on Mon May  4 15:50:42 2015

@author: morton
"""


import logging
import numpy as np

class FlexpartErrors(object):


    """
    A class that takes two FlexpartOutput objects as input and provides
    methods for calculation of errors on slices, volumes and timeseries.
    
    The following arguments are included in most of the calls to methods,
    so are described here rather than in each method.  There are a variety
    of combinations, which might get confusing, as not all arguments are
    used in all cases.
    
    timestamp : If it has a value, and if this is not a timeseries, then 
    the indicated timestamp is used
    
    timestamp_list : if timeseries is True, then this will be used to provide
    the list of timestamps to use.  
    
    level : specifies the level to use for a slice.  Indexed from 1.
    
    level_list : if volume is True, then this will be used to provide the
    list of levels to use.
    
    species : species number to use.  Indexed from 1
    
    release : release number to use.  Indexed from 1
    
    age_class : age_class number to use.  Indexed from 1
    
    wet : if True, use the wet deposition.  If dry is also True, the
    result is non-deterministic
    
    dry : if True, use the dry deposition.  If wet is also True, the result
    is non-deterministic
    
    timeseries : if True, then it will use a timeseries as defined in 
    timestamp_list.  If timestamp_list is None, then a timeseries of
    all available timestamps will be used
    
    volume : if True, then it will use a volume as defined in level_list.
    If level_list is None, then a volume of all available levels will be
    used.
    """





    
    def __init__(self, control=None, test=None):
        
        """
        Constructor
        control: a FlexpartOutput object to be used as control data
        test: a FlexpartOutput object to be used as test data
        """
        
        # Let's do a bit of a sanity check on equal dimensions.  Get
        # a default volume from each and compare
        # the dimensions.  Also check that timestamps are the same.  


        control_volume_shape = control.get_volume().shape
        test_volume_shape = test.get_volume().shape
        
        control_timestamps = control.get_timestamp_list()
        test_timestamps = test.get_timestamp_list()        

        if control_volume_shape == test_volume_shape and \
            control_timestamps == test_timestamps:
           
            self._control_output = control
            self._test_output = test
            
        else:
                
            print '** WARNING - FlexpartErrors __init__():'
            print '    control and test volume shapes are not the same, and/or'
            print '    control and test timestamps are not the same'
            print '        control_volume_shape: ', control_volume_shape
            print '        test_volume_shape: ', test_volume_shape
            print '        control_timestamps: ', control_timestamps
            print '        test_timestamps: ', test_timestamps

            self._control_output = None
            self._test_output = None



                
    def get_diff_grid(self, timestamp=None, 
                      timestamp_list=None,
                      level=1,
                      level_list=None,
                      species=1,
                      release=1,
                      age_class=1,
                      wet=False,
                      dry=False,
                      timeseries=False,
                      volume=False):
                          
        """
        Gets the difference grid as specified by the parameters.
        
        Note the True/False values of the last parameters:
        

        """
        
        # Extract grids from the control and test FlexpartOutput objects.
        # The grid is defined by the various parameters.          
        control_grid = self._get_grid(flexout_obj=self._control_output,
                                      timestamp=timestamp,
                                      timestamp_list=timestamp_list,
                                      level=level,
                                      level_list=level_list,
                                      species=species,
                                      release=release,
                                      age_class=age_class,
                                      wet=wet,
                                      dry=dry,
                                      timeseries=timeseries,
                                      volume=volume)

        test_grid = self._get_grid(flexout_obj=self._test_output,
                                      timestamp=timestamp,
                                      timestamp_list=timestamp_list,
                                      level=level,
                                      level_list=level_list,
                                      species=species,
                                      release=release,
                                      age_class=age_class,
                                      wet=wet,
                                      dry=dry,
                                      timeseries=timeseries,
                                      volume=volume)

        diff_grid = test_grid - control_grid
        
        
        return diff_grid


    def mean_absolute_error(self, timestamp=None, 
                      timestamp_list=None,
                      level=1,
                      level_list=None,
                      species=1,
                      release=1,
                      age_class=1,
                      wet=False,
                      dry=False,
                      timeseries=False,
                      volume=False):        
        """
        Returns the mean absolute error of the test and control grid, as
        defined by the parameters
        """

        diff_grid = self.get_diff_grid(timestamp=timestamp,
                                       timestamp_list=timestamp_list,
                                       level=level,
                                       level_list=level_list,
                                       species=species,
                                       release=release,
                                       age_class=age_class,
                                       wet=wet,
                                       dry=dry,
                                       timeseries=timeseries,
                                       volume=volume)
                                       
        return np.absolute(diff_grid).mean() 


    def max_absolute_error(self, timestamp=None, 
                      timestamp_list=None,
                      level=1,
                      level_list=None,
                      species=1,
                      release=1,
                      age_class=1,
                      wet=False,
                      dry=False,
                      timeseries=False,
                      volume=False):        
        """
        Returns the max absolute error of the test and control grid, as
        defined by the parameters
        """

        diff_grid = self.get_diff_grid(timestamp=timestamp,
                                       timestamp_list=timestamp_list,
                                       level=level,
                                       level_list=level_list,
                                       species=species,
                                       release=release,
                                       age_class=age_class,
                                       wet=wet,
                                       dry=dry,
                                       timeseries=timeseries,
                                       volume=volume)
                                       
        return np.absolute(diff_grid).max() 

    
    def max_error(self, timestamp=None, 
                      timestamp_list=None,
                      level=1,
                      level_list=None,
                      species=1,
                      release=1,
                      age_class=1,
                      wet=False,
                      dry=False,
                      timeseries=False,
                      volume=False):        
        """
        Returns the max error of the test and control grid, as
        defined by the parameters.  
        NOTE - I "think" this is the same as max_abs_error as written,
        and maybe shouldn't have the key=abs... 
        """

        diff_grid = self.get_diff_grid(timestamp=timestamp,
                                       timestamp_list=timestamp_list,
                                       level=level,
                                       level_list=level_list,
                                       species=species,
                                       release=release,
                                       age_class=age_class,
                                       wet=wet,
                                       dry=dry,
                                       timeseries=timeseries,
                                       volume=volume)
                                       
        return max(diff_grid.max(), diff_grid.min(), key=abs) 


    def mean_bias(self, timestamp=None, 
                      timestamp_list=None,
                      level=1,
                      level_list=None,
                      species=1,
                      release=1,
                      age_class=1,
                      wet=False,
                      dry=False,
                      timeseries=False,
                      volume=False):        
        """
        Returns the mean of the biases of the test and control grid, as
        defined by the parameters
        """

        diff_grid = self.get_diff_grid(timestamp=timestamp,
                                       timestamp_list=timestamp_list,
                                       level=level,
                                       level_list=level_list,
                                       species=species,
                                       release=release,
                                       age_class=age_class,
                                       wet=wet,
                                       dry=dry,
                                       timeseries=timeseries,
                                       volume=volume)
                                       
        return diff_grid.mean()  


    def rmse(self, timestamp=None, 
                      timestamp_list=None,
                      level=1,
                      level_list=None,
                      species=1,
                      release=1,
                      age_class=1,
                      wet=False,
                      dry=False,
                      timeseries=False,
                      volume=False):        
        """
        Returns the root mean square error of the test and control grid, as
        defined by the parameters
        """

        diff_grid = self.get_diff_grid(timestamp=timestamp,
                                       timestamp_list=timestamp_list,
                                       level=level,
                                       level_list=level_list,
                                       species=species,
                                       release=release,
                                       age_class=age_class,
                                       wet=wet,
                                       dry=dry,
                                       timeseries=timeseries,
                                       volume=volume)
                                       
        return np.sqrt( ( diff_grid**2).mean() )


        return diff_grid.max() 






        
        
    def _get_grid(self, flexout_obj=None,
                      timestamp=None, 
                      timestamp_list=None,
                      level=1,
                      level_list=None,
                      species=1,
                      release=1,
                      age_class=1,
                      wet=False,
                      dry=False,
                      timeseries=False,
                      volume=False):


                          
        """
        Gets the grid as specified by the parameters.  This is a private
        method and flexout_obj is intended to be the control or test grid
        that this class contains.
        
        Note the True/False values of the last parameters.  They will 
        dictate the flow of this routine.
        """

        if not timeseries and not volume:
            # horiz slices

            if not wet and not dry:
                the_grid = flexout_obj.get_horiz_slice(
                        timestamp=timestamp,
                        level=level,
                        species=species,
                        release=release,
                        age_class=age_class)

            else:

                if wet and dry:
                    logging.error("Bad options - cannot specify both wet and dry")
                    the_grid = None
                else:
                    if wet: depo_type = 'wet'
                    if dry: depo_type = 'dry'
                    
                    the_grid = flexout_obj.get_deposition(
                            timestamp=timestamp,
                            species=species,
                            release=release,
                            age_class=age_class,
                            depo_type=depo_type)
                        
            
        elif volume and not timeseries:
            # Volume grid
            the_grid = flexout_obj.get_volume(
                    timestamp=timestamp,
                    level_list=level_list,
                    species=species,
                    release=release,
                    age_class=age_class)            

            
        elif timeseries and not volume:
            # Timeseries of horiz slices
        
            if not wet and not dry:        
                the_grid = flexout_obj.get_horiz_timeseries(
                        timestamp_list=timestamp_list,
                        level=level,
                        species=species,
                        release=release,
                        age_class=age_class)                      


            else:

                if wet and dry:
                    logging.error("Bad options - cannot specify both wet and dry")
                    the_grid = None
                else:
                    if wet: depo_type = 'wet'
                    if dry: depo_type = 'dry'

                    the_grid = flexout_obj.get_deposition_timeseries(
                            timestamp_list=timestamp_list,
                            species=species,
                            release=release,
                            age_class=age_class,
                            depo_type=depo_type)

            
        elif timeseries and volume:
            # Timeseries of volumes
            the_grid = flexout_obj.get_volume_timeseries(
                    timestamp_list=timestamp_list,
                    level_list=level_list,
                    species=species,
                    release=release,
                    age_class=age_class)                      

        else:
            # If we made it here, we have bad combination of options
            # and can't select a routine to generate the the_grid
            logging.error("Bad options - cannot generate a the_grid")
            the_grid = None
        
        return the_grid


        
        
