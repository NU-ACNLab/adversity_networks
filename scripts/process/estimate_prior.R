### This script creates a prior for personalized network metrics 
###
### Ellyn Butler
### March 23, 2026

library(dplyr)

# Input directory
indir <- '/projects/b1108/projects/adversity_networks/data/processed/neuroimaging/'
#indir <- '~/Documents/Northwestern/projects/adversity_networks/data/processed/neuroimaging/'

# Output directory
outdir <- '/projects/b1108/projects/adversity_networks/data/processed/neuroimaging/prior/'
#outdir <- '~/Documents/Northwestern/projects/adversity_networks/data/processed/neuroimaging/prior/'

# Packages
#devtools::install_github('mandymejia/fMRItools', '0.4') # Need dev version, not CRAN
library(fMRItools)
stopifnot(utils::packageVersion('fMRItools') >= '0.4.4')
#install.packages('ciftiTools')
library(ciftiTools)
ciftiTools.setOption('wb_path', '/projects/b1108/software/workbench')
#devtools::install_github('mandymejia/priorICAr', '8.0') # Need dev version, not CRAN
#^ try again
library(BayesBrainMap)


## -----------------------------------------------------------------------------


save(list=ls(), file=paste0(outdir, 'estimate_prior_args.rda'))

temp_subjs <- read.csv(paste0(indir, 'tabulated/prior_subjects_2026-03-23.csv'))

GPARC <- readRDS(paste0(outdir, '/GPARC.rds'))

print('Resample GPARC')

GPARC <- resample_cifti(GPARC, resamp_res = 10000)

print('Get the paths to the postprocessed data.')
Sys.setenv('R_MAX_VSIZE'=32000000000)
paths <- c()
for (j in 1:nrow(temp_subjs)) { 
  subid <- temp_subjs[j, 'subid']
  sesid <- temp_subjs[j, 'sesid']
  path <- c(system(paste0('find ', indir, 'surf/sub-', subid, '/ses-', sesid, '/func/ ', 
        '-name "*_task-all_space-fsLR_desc-postproc_bold.dscalar.nii"'), intern=TRUE))
  paths <- c(paths, path)
}

print('Estimate prior.') 
prior <- estimate_prior(
  paths,
  template = GPARC, 
  hpf = 0, 
  scale = 'local',
  brainstructures = c('left', 'right'),
  GSR = FALSE,
  FC = FALSE,
  scale_sm_surfL = load_surf('left'),
  scale_sm_surfR = load_surf('right'), 
  verbose = TRUE#, usePar=4, wb_path=wb_path
) 

saveRDS(prior, paste0(outdir, 'prior.rds'))


#plot(temp, idx=1:17, fname=paste0('/Users/flutist4129/Documents/Northwestern/projects/adversity_networks/plots/temp_maxpostproc_sub-ses2_ses-rand_', 1:17))