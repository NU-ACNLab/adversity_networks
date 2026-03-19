### This script creates a csv of the subjects and 
### sessions with BIDS data
###
### Ellyn Butler
### January 17, 2025 - January 21, 2025


import os
import shutil
import re
import numpy as np
import glob
import pandas as pd
from datetime import datetime

rise_indir = '/projects/b1108/studies/rise/data/raw/neuroimaging/bids/'
crest_indir = '/projects/b1108/studies/crest/data/raw/neuroimaging/bids/'

rise_subdirs = glob.glob(rise_indir + "sub-*")
crest_subdirs = glob.glob(crest_indir + "sub-*")
subdirs = rise_subdirs + crest_subdirs

t1_t3 = {'subid':[],
         'sesid':[]}

for subdir in subdirs:
    sub = subdir.split('/')[9]
    subid = str(sub.split('-')[1])
    sessions = glob.glob(subdir + "/ses-*")
    sessions = [i.split('/')[10] for i in sessions]
    if 'ses-1' in sessions and 'ses-2' in sessions and subid != '10.21':
        for ses in sessions:
            sesid = ses.split('-')[1]
            if sesid == '1':
                t1_t3['subid'].append(subid)
                t1_t3['sesid'].append(sesid)
            elif sesid == '2':
                t1_t3['subid'].append(subid)
                t1_t3['sesid'].append(sesid)

t1_t3_df = pd.DataFrame.from_dict(t1_t3)
t1_t3_df.to_csv('/projects/b1108/studies/rise/data/processed/neuroimaging/tabulated/rise_crest_t1_t3_neuro_'+datetime.today().strftime('%Y-%m-%d')+'.csv', index=False)