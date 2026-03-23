### This script calculates mean FD for all tasks
### 
### Ellyn Butler
### March 19, 2026

outdir <- '/projects/b1108/projects/adversity_networks/data/processed/neuroimaging/'
basedir <- paste0(outdir, 'fmriprep_25.2.4')

df <- read.csv(paste0(outdir, 'tabulated/quality_2026-03-19.csv'))
df <- df[, c('subid', 'sesid')]
df <- unique(df)
df$meanFD <- NA

for (i in 1:nrow(df)) {
    subid <- df[i, 'subid']
    sesid <- df[i, 'sesid']
    files <- Sys.glob(file.path(basedir, paste0('sub-', subid), paste0('ses-', sesid), 'func',
        paste0('sub-', subid, '_ses-', sesid,'_task-*_run-*_desc-confounds_timeseries.tsv')))
    
    fd <- c()
    for (j in 1:length(files)) {
      tmp <- read.table(files[j], sep = '\t', header = TRUE)
      fd <- c(fd, as.numeric(tmp$framewise_displacement))
    }
    meanFD <- mean(fd, na.rm=TRUE)
    df[i, 'meanFD'] <- meanFD
}

write.csv(df, paste0(outdir, 'tabulated/meanFD_', format(Sys.Date(), "%Y-%m-%d"), '.csv'), row.names = FALSE)