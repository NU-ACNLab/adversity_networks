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

df[,'Family\'s Gross Total Income'] <- ordered(df$Familys_Gross_Total_Income, c('$20,000 - $34,999',
                                    '$35,000 - $49,999', '$50,000 - $74,999', '$75,000 - $99,999', 
                                    '$100,000 - $149,999', '$150,000 - $199,999', '$200,000 - $249,999', 
                                    '$250,000 - $299,999', '$300,000 and higher'))

df <- df |>
  mutate(Race = case_match(
    Race,
    'Other  {demo_race_other}' ~ 'Other',
    .default = Race
  ))

table1(~ Age + Race + Ethnicity + `Family\'s Gross Total Income` +
        Depression | Sex, data = df)
