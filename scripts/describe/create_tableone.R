### This script creates Table 1
###
### Ellyn Butler
### June 29, 2026


library(table1) # v 1.5.1
library(dplyr) # v 1.1.4


df <- read.csv('~/Documents/Northwestern/projects/adversity_networks/data/processed/combined/combined_data_2026-06-01.csv')
df <- df[!is.na(df$exp_salienceb_pos_2), ]

df$Age <- df$age_mri

df$Depression <- df$bdi_sum_2


table1(~ Age + Race + Ethnicity + Familys_Gross_Total_Income +
        Depression | Sex, data=df)
