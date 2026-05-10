### Merge all data types for final sample
###
### Ellyn Butler
### March 26, 2026 - March 31, 2026

library(dplyr)

# load data
basedir <- '/projects/b1108/projects/adversity_networks/data/processed/'
demo_df <- read.csv(paste0(basedir, 'demographic/demographic_2026-03-31.csv'))
ses_df <- read.csv('/projects/b1108/projects/personalized_versus_group/data/processed/demographic/ses_2025-11-02.csv')
clin_df <- read.csv(paste0(basedir, 'clinical/clinical_2026-03-23.csv'))
net_df <- read.csv(paste0(basedir, 'neuroimaging/tabulated/surf_network_metrics_2026-03-26.csv'))
subjs_df <- read.csv(paste0(basedir, 'neuroimaging/tabulated/prior_subjects_2026-03-23.csv'))

# Remove some columns
demo_df$sesid <- NULL
ses_df$sesid <- NULL

# Widen net_df
net_df1 <- net_df[which(net_df$sesid == 1), ]
net_df1 <- net_df1[, c('subid', 'exp_salienceb_pos', 'FC_pers_salienceb_pos',
                       'exp_dorsalattentionb_pos', 'FC_pers_dorsalattentionb_pos')]
names(net_df1) <- c('subid', 'exp_salienceb_pos_1', 'FC_pers_salienceb_pos_1',
                       'exp_dorsalattentionb_pos_1', 'FC_pers_dorsalattentionb_pos_1')
net_df2 <- net_df[which(net_df$sesid == 2), ]
net_df2 <- net_df2[, c('subid', 'exp_salienceb_pos', 'FC_pers_salienceb_pos',
                       'exp_dorsalattentionb_pos', 'FC_pers_dorsalattentionb_pos')]
names(net_df2) <- c('subid', 'exp_salienceb_pos_2', 'FC_pers_salienceb_pos_2',
                       'exp_dorsalattentionb_pos_2', 'FC_pers_dorsalattentionb_pos_2')

#
ses_df$income_category <- ordered(ses_df$income_category, c('$0 - $4,999', '$5,000 - $19,999', 
                        '$20,000 - $34,999', '$35,000 - $49,999', '$50,000 - $74,999',
                        '$75,000 - $99,999', '$100,000 - $149,999', '$150,000 - $199,999',
                        '$200,000 - $249,999', '$250,000 - $299,999', '$300,000 and higher',
                        'Not known'))
ses_df$Familys_Gross_Total_Income <- ses_df$income_category

# Merge
final_df <- merge(demo_df, ses_df, by=c('subid'), all=TRUE)
final_df <- merge(final_df, clin_df, by=c('subid'), all=TRUE)
final_df <- merge(final_df, net_df1, by=c('subid'), all=TRUE)
final_df <- merge(final_df, net_df2, by=c('subid'), all=TRUE)

final_df <- final_df[, c('subid', 'Sex', 'Race', 'Ethnicity', 'Familys_Gross_Total_Income', 
                         'age_bdi', 'age_les', 'age_mri', 'age_bdi_minus_age_mri', 
                         'age_mri_minus_age_les', 'age_bdi_minus_age_les',
                         'threatening_2', 'unstable_2', 'bdi_sum_1', 'bdi_sum_2',
                         'exp_salienceb_pos_1', 'FC_pers_salienceb_pos_1',
                         'exp_dorsalattentionb_pos_1', 'FC_pers_dorsalattentionb_pos_1',
                         'exp_salienceb_pos_2', 'FC_pers_salienceb_pos_2',
                         'exp_dorsalattentionb_pos_2', 'FC_pers_dorsalattentionb_pos_2')] #

final_df <- final_df[which(final_df$subid %in% subjs_df$subid), ]

write.csv(final_df, paste0(basedir, 'combined/combined_data_', format(Sys.Date(), "%Y-%m-%d"), '.csv'), row.names=FALSE)






#
