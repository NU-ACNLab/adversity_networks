### This script cleans demographic data files
###
### Ellyn Butler
### March 26, 2026 - March 31, 2026

# Load libraries
library(haven)
library(tidyverse)
library(dplyr)

# Load data
rise_demo_df <- read_sav('~/Documents/Northwestern/projects/personalized_versus_group/data/raw/demographic/RISE_Demo_8.7.2025.sav')
rise_bdi_df <- read.csv('~/Documents/Northwestern/projects/adversity_networks/data/raw/clinical/ProjectRISE-17BDI_DATA_2026-03-20_1043.csv')
rise_les_t3_df <- read.csv('~/Documents/Northwestern/projects/adversity_networks/data/raw/clinical/ProjectRISE-103LEST3_DATA_2026-03-20_1049.csv')
rise_mri_df <- read.csv('~/Documents/Northwestern/projects/adversity_networks/data/raw/demographic/ProjectRISE-260FMRIProtocolDateV_DATA_2026-03-31_1218.csv')
crest_demo_df <- read_sav('~/Documents/Northwestern/projects/personalized_versus_group/data/raw/demographic/CREST_Demo_8.7.2025.sav')
crest_bdi_df <- read.csv('~/Documents/Northwestern/projects/adversity_networks/data/raw/clinical/ProjectCREST-15BDI_DATA_2026-03-20_1056.csv')
crest_les_t3_df <- read.csv('~/Documents/Northwestern/projects/adversity_networks/data/raw/clinical/ProjectCREST-123LEST3_DATA_2026-03-20_1057.csv')
crest_mri_df <- read.csv('~/Documents/Northwestern/projects/adversity_networks/data/raw/demographic/ProjectCREST-230FMRIProtocolDateV_DATA_2026-03-31_1222.csv')

# Filter BDI data to get the date
rise_bdi_df$sesid <- recode(rise_bdi_df$redcap_event_name, 't1s2_b1_arm_1' = 1,
                            't2_b1_arm_1' = NaN, 't3s2_b1_arm_1' = 2,
                            't4_b1_arm_1' = NaN, 't5s2_b1_arm_1' = NaN)
rise_bdi_df$redcap_event_name <- NULL
rise_bdi_df <- rise_bdi_df[which(rise_bdi_df$sesid == 2), ]

crest_bdi_df$sesid <- recode(crest_bdi_df$redcap_event_name, 't1_b2_arm_1' = 1,
                            't2_b1_arm_1' = NaN, 't3_b2_arm_1' = 2,
                            't4_b1_arm_1' = NaN, 't5_b2_arm_1' = NaN,
                            't6_b1_arm_1' = NaN)
crest_bdi_df$redcap_event_name <- NULL
crest_bdi_df <- crest_bdi_df[which(crest_bdi_df$sesid == 2), ]

# Filter LES data to get the date
rise_les_t3_df <- rise_les_t3_df[, c('rise_id', 'les_t3_date_time')]
crest_les_t3_df <- crest_les_t3_df[, c('crest_id', 'les_t3_date_time')]

# Filter MRI data to get the date
rise_mri_df <- rise_mri_df[which(rise_mri_df$redcap_event_name == 't3s2_b2_arm_1'), c('rise_id', 'rise_fmri_protocol_date')]
crest_mri_df <- crest_mri_df[which(crest_mri_df$redcap_event_name == 't3_b3_arm_1'), c('crest_id', 'crest_fmri_protocol_date')]

# Merge data
rise_df <- merge(rise_demo_df, rise_bdi_df, by = 'rise_id')
rise_df <- merge(rise_df, rise_les_t3_df, by = 'rise_id')
rise_df <- merge(rise_df, rise_mri_df, by = 'rise_id')
rise_df$subid <- rise_df$rise_id
rise_df$fmri_protocol_date <- rise_df$rise_fmri_protocol_date
rise_df$rise_fmri_protocol_date <- NULL
rise_df$rise_id <- NULL
rise_df$bdi_risk_calc <- NULL
rise_df$rise_group <- NULL
crest_df <- merge(crest_demo_df, crest_bdi_df, by = 'crest_id')
crest_df <- merge(crest_df, crest_les_t3_df, by = 'crest_id')
crest_df <- merge(crest_df, crest_mri_df, by = 'crest_id')
crest_df$subid <- crest_df$crest_id
crest_df$fmri_protocol_date <- crest_df$crest_fmri_protocol_date
crest_df$crest_fmri_protocol_date <- NULL
crest_df$crest_id <- NULL
crest_df$Correction_Notes <- NULL
crest_df$bdi_risk <- NULL
crest_df$crest_group <- NULL

if (all(names(rise_df) == names(crest_df)) == TRUE) {
    df <- rbind(rise_df, crest_df)
}

# Calculate age
df$demo_dob <- as.Date(df$demo_dob, '%Y-%m-%d')
df$bdi_date_time <- as.Date(df$bdi_date_time, '%Y-%m-%d')
df$age_bdi <- (df$bdi_date_time - df$demo_dob)/365
df$age_bdi <- as.numeric(df$age_bdi)

df$les_t3_date_time <- as.Date(df$les_t3_date_time, '%Y-%m-%d')
df$age_les <- (df$les_t3_date_time - df$demo_dob)/365
df$age_les <- as.numeric(df$age_les)

df$fmri_protocol_date <- as.Date(df$fmri_protocol_date, '%Y-%m-%d')
df$age_mri <- (df$fmri_protocol_date - df$demo_dob)/365
df$age_mri <- as.numeric(df$age_mri)

# Clean up
df$sex <- df$demo_sex
df$gender <- df$demo_gender
df$race <- df$demo_race
df$latino <- df$demo_latino

df$Sex <- as_factor(df$sex)
df$Sex <- recode(df$Sex, `1`='Female', `0`='Male') #checked
df$Sex <- factor(df$Sex)

df$Race <- as_factor(df$race)
df$Race <- recode(df$Race, `0`='Caucasian/White', `1`='African American or Black',
                  `2`='Asian-American or Asian', `3`='Native American / Alaska Native', 
                  `4`='Native Hawaiian or Other Pacific Islander', 
                  `5`='Biracial/Multiracial', `6`='Other', `-999`='Other')
df$Race <- factor(df$Race)

df$Ethnicity <- as_factor(df$latino)
df$Ethnicity <- recode(df$Ethnicity, `0`='Not Hispanic or Latino/a', 
                       `1`='Hispanic or Latino/a')
df$Ethnicity <- factor(df$Ethnicity)

# Filter data (subid, sesid, age, sex, gender, race, latino)
df <- df[, c('subid', 'sesid', 'age_bdi', 'age_les', 'age_mri', 'Sex', 'Race', 'Ethnicity')]
df$age_bdi_minus_age_mri <- df$age_bdi - df$age_mri
df$age_mri_minus_age_les <- df$age_mri - df$age_les
df$age_bdi_minus_age_les <- df$age_bdi - df$age_les

# Write
write.csv(df, paste0('~/Documents/Northwestern/projects/adversity_networks/data/processed/demographic/demographic_', format(Sys.Date(), "%Y-%m-%d"), '.csv'), row.names = FALSE)