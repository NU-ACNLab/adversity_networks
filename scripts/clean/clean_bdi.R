### This script outputs the individual BDI-2 items
###
### Ellyn Butler
### June 29, 2026

# Load libraries
library(tidyverse)

basedir <- '~/Documents/Northwestern/projects/adversity_networks/data/raw/clinical/'

####### RISE

# Load data
rise_df <- read.csv(paste0(basedir, 'ProjectRISE-17BDI_DATA_2026-03-20_1043.csv'))

# Recode session
rise_df$sesid <- recode(rise_df$redcap_event_name, 't1s2_b1_arm_1' = 1,
                            't2_b1_arm_1' = NaN, 't3s2_b1_arm_1' = 2,
                            't4_b1_arm_1' = NaN, 't5s2_b1_arm_1' = NaN)
rise_df$subid <- rise_df$rise_id
rise_df <- rise_df[which(rise_df$sesid == 2), ]
rise_df <- rise_df[, c('subid', paste0('bdi_', 1:21))]
names(rise_df) <- c('subid', paste0('bdi_', 1:21, '_2'))

####### CREST

# Load data
crest_df <- read.csv(paste0(basedir, 'ProjectCREST-15BDI_DATA_2026-03-20_1056.csv'))

# Recode session
crest_df$sesid <- recode(crest_df$redcap_event_name, 't1_b2_arm_1' = 1,
                             't2_b1_arm_1' = NaN, 't3_b2_arm_1' = 2, 't4_b1_arm_1' = NaN,
                             't5_b2_arm_1' = NaN, 't6_b1_arm_1' = NaN)
crest_df$subid <- crest_df$crest_id
crest_df <- crest_df[which(crest_df$sesid == 2), ]
crest_df <- crest_df[, c('subid', paste0('bdi_', 1:21))]
names(crest_df) <- c('subid', paste0('bdi_', 1:21, '_2'))

# Finalize and export

df <- rbind(rise_df, crest_df)
write.csv(df, paste0('~/Documents/Northwestern/projects/adversity_networks/data/processed/clinical/bdi_', format(Sys.Date(), "%Y-%m-%d"), '.csv'), row.names = FALSE)