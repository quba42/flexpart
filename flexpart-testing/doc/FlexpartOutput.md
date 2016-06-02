
# FlexpartOutput


## Overview

This is a class intended to provide low level data access to Flexpart output.  The original source for this came from John Burkhart's *pflexible*, which was modified by Morton and Arnold, mostly to repackage just the code necessary for accessing the Flexpart output fields.

Providing a number of methods that facilitate the extraction of horizontal slices, volumes and timeseries, this class serves as the foundation for the *FlexpartErrors* class, doing the low level work needed to get output fields for comparison.

*FlexpartOutput* objects are created by specifying the path to the Flexpart output directory, and an optional argument for specifying the extraction of an outgrid nest rather than the mother outgrid domain.  Note that a single *FlexpartOutput* object will contain either a mother outgrid domain, or an outgrid nest, but not both.

All returned slices, volumes and timeseries are NumPy arrays.  A timeseries of a volume would result in a 4-dimension NumPy array being returned.  A simple example of usage follows:

		import flexread.FlexpartOutput as FlexpartOutput
					
		output_obj = FlexpartOutput.FlexpartOutput(output_dir='flexpart_run/output')
					
		# Get default horizontal slice on level 1
		slice1 = output_obj.get_horiz_slice()
					
		# Get horiz slice at specified timestamp, level, species and release
		slice2 = output_obj.get_horiz_slice(timestamp='20140919030000',
 	                                        level=2, species=2, release=4)
					
		# Get dry deposition for specified species and release.  
		dry_depo = output_obj.get_deposition(species=2, release=4, depo_type='dry')
					
		# Get volume for default timestamp, species, release
		volume = output_obj.get_volume()




The pydoc representation:

		    class FlexpartOutput(__builtin__.object)
		     |  High level interface to encapsulate and provide access to
		     |  the output of a specified FLEXPART simulation
		     |  
		     |  Methods defined here:
		     |  
		     |  __init__(self, output_dir=None, nest=False)
		     |      Constructor
		     |      output_dir: full path to directory of FLEXPART output
		     |      nest: Denotes whether this object is for the mother grid or a nest
		     |  
		     |  get_deposition(self, timestamp=None, species=1, release=1, age_class=1, depo_type='dry')
		     |      Extracts 2D horizontal slice of deposition.  
		     |      Note that species and 
		     |      release index values for the user start at 1, but they are all
		     |      decremented internal to this method so they start at 0.
		     |      Returned 2D grid is a NumPy array
		     |      
		     |      If there is any kind of error, we return None
		     |  
		     |  get_deposition_timeseries(self, timestamp_list=None, species=1, release=1, age_class=1, depo_type='dry')
		     |      Extracts timeseries of 2D deposition.  If timestamp_list is
		     |      defined correctly, it extracts just the timestamps (in YYYYMMDDHHMMss
		     |      format) listed.  Otherwise, it extracts all timestamps. The timeseries
		     |      is returned in a 3D NumPy array, indexed by (t, x, y)
		     |  
		     |  get_horiz_slice(self, timestamp=None, level=1, species=1, release=1, age_class=1)
		     |      Extracts 2D horizontal slice.  Note that level, species and 
		     |      release index values for the user start at 1, but they are all
		     |      decremented internal to this method so they start at 0.
		     |      Returned 2D grid is a NumPy array
		     |      
		     |      If there is any kind of error, we return None
		     |  
		     |  get_horiz_timeseries(self, timestamp_list=None, level=1, species=1, release=1, age_class=1)
		     |      Extracts timeseries of 2D horizontal slices.  If timestamp_list is
		     |      defined correctly, it extracts just the timestamps (in YYYYMMDDHHMMss
		     |      format) listed.  Otherwise, it extracts all timestamps. The timeseries
		     |      is returned in a 3D NumPy array, indexed by (t, x, y)
		     |  
		     |  get_timestamp_list(self)
		     |      Returns the list of timestamps as Python list
		     |  
		     |  get_volume(self, timestamp=None, level_list=None, species=1, release=1, age_class=1)
		     |      Extracts 3D volume.  if level_list is not defined, we return the full
		     |      volume for the timestamp.  If level_list is defined,then we extract the
		     |      specified levels and create a volume from them
		     |      
		     |      Note that level, species and 
		     |      release index values for the user start at 1, but they are all
		     |      decremented internal to this method so they start at 0.
		     |      Returned 2D grid is a NumPy array
		     |      
		     |      If there is any kind of error, we return None
		     |  
		     |  get_volume_timeseries(self, timestamp_list=None, level_list=None, species=1, release=1, age_class=1)
		     |      Extracts timeseries of 2D horizontal slices.  If timestamp_list is
		     |      defined correctly, it extracts just the timestamps (in YYYYMMDDHHMMss
		     |      format) listed.  Otherwise, it extracts all timestamps. The timeseries
		     |      is returned in a 3D NumPy array, indexed by (t, x, y)
		




## Issues

* By default, a timeseries will contain all timestamps, and a volume will contain all levels.  However, users can specify timestamp_list and level_list to customise the creation of the timeseries or volume.  There is nothing in this code that checks the sanity of these customisations.  It is therefore possible for the user to create a volume of disordered levels, or a timeseries where all timestamps are the same.  Perhaps this should be clarified at some point - for now, we choose to keep this very flexible, putting the burden of caution on the user.
	




