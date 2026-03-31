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

#..................... March 31, 2026: All of this needs to be cleaned up for the paper
# Remove subjects who were pilots
final_df <- final_df[which(!(final_df$subid %in% c('MWMH001', 'MWMH102'))), ]
dim(final_df) # 

# Remove sessions that are missing an age for both mri and lab (means they weren't
# a member of that time point)
final_df <- final_df[which(!(is.na(final_df$age_lab) & is.na(final_df$age_mri))), ]
dim(final_df) # 

########### Descriptive - before removing NAs

# Number of participants
length(unique(final_df$subid)) #

first_subids <- final_df[final_df$sesid == 1, 'subid']
second_subids <- final_df[final_df$sesid == 2, 'subid']

# Both time points
both_tps <- second_subids[second_subids %in% first_subids]
length(both_tps) #

# Just first time point
first_tp <- first_subids[!(first_subids %in% second_subids)]
length(first_tp) #

# Just second time point
second_tp <- second_subids[!(second_subids %in% first_subids)]
length(second_tp) #

table(final_df[final_df$sesid == 1, 'female']) #
table(final_df[final_df$sesid == 2, 'female']) #
summary(final_df[final_df$sesid == 1, 'age_lab']) #
summary(final_df[final_df$sesid == 2, 'age_lab']) #
table(final_df$ever) #

summary(final_df$num_pastyear)
table(final_df$num_pastyear)


########### Descriptive - before removing NAs

# T1
d1 <- final_df[final_df$sesid == 1, ]
sum(is.na(d1$depression)) #
sum(is.na(d1$age_mri)) #
sum(is.na(d1$black)) #
sum(is.na(d1$white)) #
sum(is.na(d1$hispanic)) #
sum(is.na(d1$BMIperc)) #
sum(is.na(d1$PubCat)) #
sum(is.na(d1$IPR)) #
sum(is.na(d1$num_pastyear)) #
sum(is.na(d1$female)) #
sum(is.na(d1$exp_b_pos)) #
sum(is.na(d1$FC_b_pos)) #

# T2
d2 <- final_df[final_df$sesid == 2, ]
sum(is.na(d2$depression)) #
sum(is.na(d2$age_mri)) #
sum(is.na(d2$black)) #
sum(is.na(d2$white)) #
sum(is.na(d2$hispanic)) #
sum(is.na(d2$IPR)) #
sum(is.na(d2$num_pastyear)) #
sum(is.na(d2$female)) #
sum(is.na(d2$exp_b_pos)) #
sum(is.na(d2$FC_b_pos)) #

d2[!is.na(d2$age_mri) & is.na(d2$exp_b_pos),]

# T1 who have complete T2
d2b <- d2[!is.na(d2$FC_b_pos), ] 
d1b <- d1[d1$subid %in% d2b$subid,]
sum(is.na(d1b$depression)) #
sum(is.na(d1b$age_mri)) #
sum(is.na(d1b$black)) #
sum(is.na(d1b$white)) #
sum(is.na(d1b$hispanic)) #
sum(is.na(d1b$BMIperc)) #
sum(is.na(d1b$PubCat)) #
sum(is.na(d1b$IPR)) #
sum(is.na(d1b$num_pastyear)) #
sum(is.na(d1b$female)) #
sum(is.na(d1b$exp_b_pos)) #
sum(is.na(d1b$FC_b_pos)) #

d1b_anyna <- d1b[is.na(d1b$age_mri) | is.na(d1b$exp_b_pos),]

# Subjects who have an age_mri but no SN metrics ()
d1b_anyna[!is.na(d1b_anyna$age_mri) & is.na(d1b_anyna$exp_b_pos), ]


##### Differences on baseline demographics between those
##### with complete data at ses-2 vs. not?
subids_complete_ses2 <- d2b$subid #
subids_incomplete_ses2 <- d1$subid[!(d1$subid %in% d2b$subid)] #

# black, white, hispanic, SES, female, age_lab
d1$completeses2 <- d1$subid %in% subids_complete_ses2

sum(is.na(d1$black)) #
chisq.test(d1$completeses2, d1$black) #
sum(is.na(d1$white)) #
chisq.test(d1$completeses2, d1$white) #s
white_tab <- table(d1$completeses2, d1$white)
#Among those who do not have complete data at the second time point, XX% are non-white
white_tab[row.names(white_tab) == FALSE, '0']/sum(white_tab[row.names(white_tab) == FALSE,])
#Among those who do have complete data at the second time point, XX% are non-white
white_tab[row.names(white_tab) == TRUE, '0']/sum(white_tab[row.names(white_tab) == TRUE,])
sum(is.na(d1$hispanic)) #
chisq.test(d1$completeses2, d1$hispanic) #
sum(is.na(d1$BMIperc)) #
t.test(d1[d1$completeses2 == 1, 'IPR'], d1[d1$completeses2 == 0, 'IPR']) #
sum(is.na(d1$female)) #
chisq.test(d1$completeses2, d1$female) #
sum(is.na(d1$age_lab)) #
t.test(d1[d1$completeses2 == 1, 'age_lab'], d1[d1$completeses2 == 0, 'age_lab']) #

##### Survey
survey_df <- na.omit(final_df[, c('subid', 'sesid', 'female', 'age_lab',
                                  'num_pastyear', 'RCADS_sum')])

# Number of participants
length(unique(survey_df$subid))

first_subids <- survey_df[survey_df$sesid == 1, 'subid']
second_subids <- survey_df[survey_df$sesid == 2, 'subid']

# Both time points
both_tps <- second_subids[second_subids %in% first_subids]
length(both_tps)

# Just first time point
first_tp <- first_subids[!(first_subids %in% second_subids)]
length(first_tp)

# Just second time point
second_tp <- second_subids[!(second_subids %in% first_subids)]
length(second_tp)

table(survey_df[survey_df$sesid == 1, 'female'])
table(survey_df[survey_df$sesid == 2, 'female'])
summary(survey_df[survey_df$sesid == 1, 'age_lab'])
summary(survey_df[survey_df$sesid == 2, 'age_lab'])
table(survey_df$ever)

summary(survey_df$num_pastyear)
table(survey_df$num_pastyear)


##### Survey + MRI
mri_df <- na.omit(final_df)

length(unique(mri_df$subid)) # 263

first_subids <- mri_df[mri_df$sesid == 1, 'subid']
second_subids <- mri_df[mri_df$sesid == 2, 'subid']

# Both time points
both_tps <- second_subids[second_subids %in% first_subids]
length(both_tps)

# Just first time point
first_tp <- first_subids[!(first_subids %in% second_subids)]
length(first_tp)

# Just second time point
second_tp <- second_subids[!(second_subids %in% first_subids)]
length(second_tp)

table(mri_df[mri_df$sesid == 1, 'female'])
table(mri_df[mri_df$sesid == 2, 'female'])
summary(mri_df[mri_df$sesid == 1, 'age_les'])
summary(mri_df[mri_df$sesid == 2, 'age_les'])
summary(mri_df[mri_df$sesid == 1, 'age_mri'])
summary(mri_df[mri_df$sesid == 2, 'age_mri'])
summary(mri_df[mri_df$sesid == 1, 'age_bdi'])
summary(mri_df[mri_df$sesid == 2, 'age_bdi'])
table(mri_df$ever)

summary(mri_df$num_pastyear)
table(mri_df$num_pastyear)

summary(mri_df$days_mri_minus_lab)
#..............

########### Write the data

final_df <- final_df[which(final_df$subid %in% subjs_df$subid), ]

write.csv(final_df, paste0(basedir, 'combined/combined_data_', format(Sys.Date(), "%Y-%m-%d"), '.csv'), row.names=FALSE)






#
