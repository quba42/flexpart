import pflexible as pf
import read_header as readh
import read_grid as readg

import numpy as np

#OUTPUT_DIR='../unittest_data/flexout_forward_tiny_complex_withnest'
#OUTPUT_DIR='../unittest_data/flexout_backward_tiny_complex_withnest'
OUTPUT_DIR='output'
TIME='20140924000000'
AGE_CLASS_NUM = 0
RELEASE_NUM = 0
SPEC_NUM = 0     # Indexed from 0.  So, species 1 should be 0...
NEST=False
DRY=True 
WET=True

#H =    pf.Header(OUTPUT_DIR)
H = readh.Header(OUTPUT_DIR, nested=NEST)
print H


'''
G =    pf.read_grid(H, nspec_ret=SPEC_NUM,
                    age_ret=AGE_CLASS_NUM, 
                    pspec_ret=RELEASE_NUM,
                    date=TIME)
'''



G = readg.read_grid(H, nspec_ret=SPEC_NUM,
                    age_ret=AGE_CLASS_NUM, 
                    pspec_ret=RELEASE_NUM,
                    date=TIME,
                    getdry=DRY,
                    getwet=WET)
'''
G = readg.read_grid(H, nspec_ret=SPEC_NUM,
                    age_ret=AGE_CLASS_NUM, 
                    pspec_ret=RELEASE_NUM,
                    date=TIME)
'''
#print G

print '==============================='

'''
G = readg.read_grid(H,
                    date=TIME,
                    age_ret=AGE_CLASS_NUM, 
                    nspec_ret=SPEC_NUM,
                    pspec_ret=RELEASE_NUM,
                    getwet=WET,
                    getdry=DRY,
                    )
'''

the_grid = G[ (SPEC_NUM, TIME) ]
#print the_grid.grid
print 'Dry Grid'
print the_grid.dry.shape
print 'Nonzero elements: ', np.count_nonzero(the_grid.dry)
print the_grid['dry']
print '---------'
print 'Wet Grid'
print the_grid.wet.shape
print 'Nonzero elements: ', np.count_nonzero(the_grid.wet)
print the_grid['wet']
print '---------'



