### This script generates submission obtaining personalized network metrics
###
### Ellyn Butler
### March 24, 2026

import os
import shutil
import re
import numpy as np
import glob
import nibabel as nib
import pandas as pd

basedir = '/projects/b1108/projects/adversity_networks/data/processed/neuroimaging/'
launchdir = basedir + 'launch/surfnet/'
indir = basedir + 'surf/'
outdir = basedir + 'surfnet/'
tabdir = basedir + 'tabulated/'

if not os.path.exists(launchdir):
    os.mkdir(launchdir)

if not os.path.exists(outdir):
    os.mkdir(outdir)

# Get the subject directories 
subdirs = glob.glob(indir + "sub-*")

for subdir in subdirs:
    sub = subdir.split('/')[9]
    if not os.path.exists(outdir+sub):
        os.mkdir(outdir+sub)
    sub_bold_imgs = glob.glob(indir+sub+'/*/*/*_task-all_space-fsLR_desc-postproc_bold.dscalar.nii')
    sessions = np.unique([i.split('/')[10] for i in sub_bold_imgs])
    subid = sub.split('-')[1]
    for ses in sessions:
        sesid = ses.split('-')[1]
        if not os.path.exists(outdir+sub+'/'+ses):
            os.mkdir(outdir+sub+'/'+ses)
        if not os.path.exists(outdir + 'sub-' + subid + '/ses-' + sesid + '/sub-' +
                              subid + '_ses-' + sesid + '_surf_network_metrics.csv'):
            sesid = str(ses).split('-')[1]
            cmd = ['Rscript /projects/b1108/projects/adversity_networks/scripts/process/get_surf_network_metrics.R -s', subid, '-e', sesid]
            get_surf_network_metrics_script = launchdir+sub+'_'+ses+'_get_surf_network_metrics_run.sh'
            os.system('cat /projects/b1108/projects/adversity_networks/scripts/process/sbatchinfo_9hr_10G_general.sh > '+get_surf_network_metrics_script)
            os.system('echo '+' '.join(cmd)+' >> '+get_surf_network_metrics_script)
            os.system('chmod +x '+get_surf_network_metrics_script)
            os.system('sbatch -o '+launchdir+sub+'_'+ses+'_get_surf_network_metrics.txt'+' '+get_surf_network_metrics_script)