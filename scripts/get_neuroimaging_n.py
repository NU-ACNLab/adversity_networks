### This script creates a csv of the subjects and 
### sessions with BIDS data
###
### Ellyn Butler
### January 17, 2025


import os
import shutil
import re
import numpy as np
import glob

rise_indir = '/projects/b1108/studies/rise/data/raw/neuroimaging/bids/'
crest_indir = '/projects/b1108/studies/rise/data/raw/neuroimaging/bids/'

rise_subdirs = glob.glob(rise_indir + "sub-*")
crest_subdirs = glob.glob(crest_indir + "sub-*")
subdirs = rise_subdirs + crest_subdirs

t1_t3 = {'subid' = [],
         'sesid' = []}

for subdir in subdirs:
    sub = subdir.split('/')[9]
    subid = sub.split('-')[1]
    sessions = glob.glob(subdir + "/ses-*")
    sessions = [i.split('/')[10] for i in sessions]
    for ses in sessions:
        sesid = ses.split('-')[1]
        if ses == '1':
            t1_t3['subid'].append(subid)
            t1_t3['sesid'].append(sesid)
        elif ses == '2':
            t1_t3['subid'].append(subid)
            t1_t3['sesid'].append(sesid)