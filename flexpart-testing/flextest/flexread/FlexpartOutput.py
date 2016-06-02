# -*- coding: utf-8 -*-
"""
FlexpartOutput.py
"""

import logging
import numpy as np
import os
import sys

#import pflexible as pf
import read_header as rh
import read_grid as rg

class FlexpartOutput(object):
    
    """High level interface to encapsulate and provide access to
    the output of a specified FLEXPART simulation"""

    def __init__(self, output_dir=None, nest=False):
        
        """Constructor
            output_dir: full path to directory of FLEXPART output
            nest: Denotes whether this object is for the mother grid or a nest
        """
        

        
        self._output_dir = output_dir

        if self._output_dir and os.path.isdir(self._output_dir):
            try:
                self._Header = rh.Header( self._output_dir, nested=nest )
            except:
                raise IOError('Problem getting output data header')
        else:
            raise IOError('Bad output data dir: ' + str(self._output_dir))

        #print self._Header
    def get_horiz_slice(self, timestamp=None, level=1, species=1, 
                        release=1, age_class=1):

        """Extracts 2D horizontal slice.  Note that level, species and 
        release index values for the user start at 1, but they are all
        decremented internal to this method so they start at 0.
        Returned 2D grid is a NumPy array

        Note that if timestamp is not defined, then a timestamp "close to
        the middle" will be selected.

        If there is any kind of error, we return None
        """

        # if timestamp is not defined, choose a "mid way" timestamp for default
        if not timestamp:
            ntimes = len(self._Header['available_dates'])
            tstamp_idx = ntimes/2
            timestamp = self._Header['available_dates'][tstamp_idx]
            
        # Get the full grid for specified timestamp and species
        so_far_so_good = True
        try:

            G = rg.read_grid(self._Header, date=timestamp, 
                             nspec_ret=species-1,
                             pspec_ret=release-1,
                             age_ret=age_class-1,
                             )
                             
        except:
            logging.error('Problem getting G from read_grid()')
            so_far_so_good = False

        if so_far_so_good:
            try:
                # Get the grid (x,y,z,release)
                the_grid = G[ (species-1, timestamp) ]
            except:
                logging.error('Problem getting the_grid from G')
                so_far_so_good = False
        
        if so_far_so_good:
            try:
                # Get the 2D slice horizontal slice (x,y)
                the_slice = the_grid.grid[:, :, level-1, release-1]
               
            except:
                logging.error('Problem getting the_slice from the_grid')
                so_far_so_good = False

        if so_far_so_good:
            return the_slice
        else:
            return None


    def get_deposition(self, timestamp=None, species=1, 
                        release=1, age_class=1, 
                        depo_type='dry'):

        """Extracts 2D horizontal slice of deposition.  
        Note that species and 
        release index values for the user start at 1, but they are all
        decremented internal to this method so they start at 0.
        Returned 2D grid is a NumPy array

        Note that if timestamp is not defined, then a timestamp "close to
        the middle" will be selected.

        If there is any kind of error, we return None
        """

        # if timestamp is not defined, choose a "mid way" timestamp for default
        if not timestamp:
            ntimes = len(self._Header['available_dates'])
            tstamp_idx = ntimes/2
            timestamp = self._Header['available_dates'][tstamp_idx]

        # Get the full grid for specified timestamp and species
        so_far_so_good = True
        try:
            dry_flag=False; wet_flag=False
            if depo_type=='dry':
                dry_flag = True
            if depo_type=='wet':
                wet_flag = True
            
            G = rg.read_grid(self._Header, date=timestamp, 
                             nspec_ret=species-1,
                             pspec_ret=release-1,
                             age_ret=age_class-1,
                             getdry=dry_flag,
                             getwet=wet_flag
                             )

            #print "G"
            #print G
            #print G.shape
            #sys.exit()
                             
        except:
            logging.error('Problem getting G from read_grid()')
            so_far_so_good = False

        if so_far_so_good:
            try:
                # Get the grid (x,y,z,release)
                the_grid = G[ (species-1, timestamp) ]
            except:
                logging.error('Problem getting the_grid from G')
                so_far_so_good = False
        
        if so_far_so_good:
            try:
                # Get the 2D slice horizontal slice (x,y)

                # 2016-03-23 -- DJM modification - apparently John put a
                # "squeeze()" call on these arrays when they came out in
                # read_grid(), so they can come out as either 2D or 3D.  I
                # need consistency, so if they're 2D, I'm expanding back out
                # to 3D
                #
                # WARNING - DJM - 2016-03-23 - I'm not sure why I used
                # "release" as an index in one of the following rather than
                # species.  I "think" it was logical, and I just went by
                # the dimensions I had available in a test case, but I'm
                # not 100% positive that this ain't going to get us into
                # trouble one day.
                if depo_type=='dry':
                    #print the_grid.dry
                    #print 'dry shape: ', the_grid.dry.shape
                    if the_grid.dry.ndim == 2:
                        dry_grid = np.expand_dims(the_grid.dry, axis=2)
                    else:
                        dry_grid = the_grid.dry
                    the_depo = dry_grid[:, :, release-1]
                elif depo_type=='wet':
                    #print 'wet shape: ', the_grid.wet.shape
                    if the_grid.wet.ndim == 2:
                        wet_grid = np.expand_dims(the_grid.wet, axis=2)
                    else:
                        wet_grid = the_grid.wet
                    the_depo = wet_grid[:, :, release-1]
                else:
                    the_depo = None               
            except:
                logging.error('Problem getting the_depo from the_grid')
                so_far_so_good = False

        if so_far_so_good:
            return the_depo
        else:
            return None



    def get_volume(self, timestamp=None, level_list=None, species=1, 
                        release=1, age_class=1):

        """Extracts 3D volume.  if level_list is not defined, we return the full
        volume for the timestamp.  If level_list is defined,then we extract the
        specified levels and create a volume from them

        Note that level, species and 
        release index values for the user start at 1, but they are all
        decremented internal to this method so they start at 0.
        Returned 2D grid is a NumPy array

        If there is any kind of error, we return None
        """

        # if timestamp is not defined, choose a "mid way" timestamp for default
        if not timestamp:
            ntimes = len(self._Header['available_dates'])
            tstamp_idx = ntimes/2
            timestamp = self._Header['available_dates'][tstamp_idx]

        # Get the full grid for specified timestamp and species
        so_far_so_good = True
        try:
            G = rg.read_grid(self._Header, date=timestamp, 
                             nspec_ret=species-1,
                             pspec_ret=release-1,
                             age_ret=age_class-1,
                             )

        except:
            logging.error('Problem getting G from read_grid()')
            so_far_so_good = False

        if so_far_so_good:
            try:
                # Get the grid (x,y,z,release)
                the_grid = G[ (species-1, timestamp) ]
            except:
                logging.error('Problem getting the_grid from G')
                so_far_so_good = False

        if so_far_so_good:
            if level_list:
                # Decrement indices to 0 start
                level_indices = list(map(lambda x: x-1, level_list))
                try:
                    # Get the volume (x,y,z)
                    the_volume = the_grid.grid[:, :, level_indices, release-1]
                except:
                    logging.error('Problem getting the_volume from the_grid')
                    so_far_so_good = False
            else:
                # Extract the full volume
                # Make sure all level numbers are less than levels dimension
                # of grid
                
                try:
                    # Get the volume (x,y,z)
                    the_volume = the_grid.grid[:, :, :, release-1]
                except:
                    logging.error('Problem getting the_volume from the_grid')
                    so_far_so_good = False

        if so_far_so_good:
            return the_volume
        else:
            return None


    def get_horiz_timeseries(self, timestamp_list=None,
                             level=1, species=1,
                             release=1, age_class=1):

        """Extracts timeseries of 2D horizontal slices.  If timestamp_list is
        defined correctly, it extracts just the timestamps (in YYYYMMDDHHMMss
        format) listed.  Otherwise, it extracts all timestamps. The timeseries
        is returned in a 3D NumPy array, indexed by (t, x, y)"""

        # Get the dimensions to create a 3D (t, x, y) array
        if not timestamp_list:
            timestamp_list = self._Header['available_dates']
        Nx = self._Header.nx
        Ny = self._Header.ny

        slice_timeseries = np.ndarray( (len(timestamp_list), Nx, Ny),
                                       dtype=np.float64 )

        
        #print Nx, Ny

        time_idx = 0
        for ts in timestamp_list:
            next_slice = self.get_horiz_slice(timestamp=ts,
                                              level=level,
                                              species=species,
                                              release=release,
                                              age_class=age_class)

            

            #print next_slice
            slice_timeseries[time_idx, :, :] = next_slice
            time_idx += 1


        return slice_timeseries


    def get_deposition_timeseries(self, timestamp_list=None,
                                 species=1,
                                 release=1, age_class=1,
                                 depo_type='dry'):

        """Extracts timeseries of 2D deposition.  If timestamp_list is
        defined correctly, it extracts just the timestamps (in YYYYMMDDHHMMss
        format) listed.  Otherwise, it extracts all timestamps. The timeseries
        is returned in a 3D NumPy array, indexed by (t, x, y)"""

        # Get the dimensions to create a 3D (t, x, y) array
        if not timestamp_list:
            timestamp_list = self._Header['available_dates']
        Nx = self._Header.nx
        Ny = self._Header.ny

        depo_timeseries = np.ndarray( (len(timestamp_list), Nx, Ny),
                                       dtype=np.float64 )

        
        #print Nx, Ny

        time_idx = 0
        for ts in timestamp_list:
            next_depo = self.get_deposition(timestamp=ts,
                                              species=species,
                                              release=release,
                                              age_class=age_class,
                                              depo_type=depo_type
                                              )


            #print 'next_depo: ', next_depo
            #print next_depo.shape

            #print next_slice
            depo_timeseries[time_idx, :, :] = next_depo
            time_idx += 1


        return depo_timeseries


    def get_volume_timeseries(self, timestamp_list=None,
                             level_list=None, species=1,
                             release=1, age_class=1):

        """Extracts timeseries of 2D horizontal slices.  If timestamp_list is
        defined correctly, it extracts just the timestamps (in YYYYMMDDHHMMss
        format) listed.  Otherwise, it extracts all timestamps. The timeseries
        is returned in a 3D NumPy array, indexed by (t, x, y)"""

        # Get the dimensions to create a 3D (t, x, y) array
        if not timestamp_list:
            timestamp_list = self._Header['available_dates']
        Nx = self._Header.nx
        Ny = self._Header.ny
        
        if not level_list:
            Nz = self._Header.nz
        else:
            Nz = len( level_list )

        volume_timeseries = np.ndarray( (len(timestamp_list), Nx, Ny, Nz),
                                       dtype=np.float64 )

        
        #print Nx, Ny, Nz

        time_idx = 0
        for ts in timestamp_list:
            next_volume = self.get_volume(timestamp=ts,
                                              level_list=level_list,
                                              species=species,
                                              release=release,
                                              age_class=age_class)

            

            #print next_volume
            volume_timeseries[time_idx, :, :] = next_volume
            time_idx += 1


        return volume_timeseries



    def get_timestamp_list(self):
        
        """
        Returns the list of timestamps as Python list
        """
        timestamp_list = self._Header['available_dates']        

        return timestamp_list        




if __name__=="_main__":
    pass
