### Merge all data types for final sample
###
### Ellyn Butler
### March 26, 2026


# load data
basedir <- '/projects/b1108/studies/mwmh/data/processed/'
demo_df <- read.csv(paste0(basedir, 'demographic/demographics_2024-10-07.csv'))
viol_df <- read.csv(paste0(basedir, 'violence/violence_2022-10-06.csv'))
dep_df <- read.csv(paste0(basedir, 'clinical/depanx_2022-10-04.csv'))
#net_df <- read.csv(paste0(basedir, 'neuroimaging/tabulated/surf_network_metrics_2024-09-13.csv'))
net_df <- read.csv(paste0(basedir, 'neuroimaging/tabulated/surf_network_metrics_2_2024-10-31.csv'))

# Merge
final_df <- merge(viol_df, demo_df, by=c('subid', 'sesid'), all=TRUE)
final_df <- merge(final_df, dep_df, by=c('subid', 'sesid'), all=TRUE)
final_df <- merge(final_df, net_df, by=c('subid', 'sesid'), all=TRUE)

final_df$depression <- rowSums(final_df[, paste0('RCADS_', c(1, 4, 8, 10, 13, 16, 15, 18, 19, 21, 24), 'r')])

final_df <- final_df[, c('subid', 'sesid', 'black', 'white', 'hispanic', 'otherrace', 
                         'BMIperc', 'PubCat', 'IPR', 'female', 'age_lab', 'age_mri',
                         'days_mri_minus_lab', 'ever', 'num_pastyear',
                         'depression', 'RCADS_sum', 'exp_b_pos', 'FC_b_pos')] #BC_b_pos

# Remove subjects who were pilots
final_df <- final_df[which(!(final_df$subid %in% c('MWMH001', 'MWMH102'))), ]
dim(final_df) # 560

# Remove sessions that are missing an age for both mri and lab (means they weren't
# a member of that time point)
final_df <- final_df[which(!(is.na(final_df$age_lab) & is.na(final_df$age_mri))), ]
dim(final_df) # 534

########### Descriptive - before removing NAs

# Number of participants
length(unique(final_df$subid)) # 277

first_subids <- final_df[final_df$sesid == 1, 'subid']
second_subids <- final_df[final_df$sesid == 2, 'subid']

# Both time points
both_tps <- second_subids[second_subids %in% first_subids]
length(both_tps) # 257

# Just first time point
first_tp <- first_subids[!(first_subids %in% second_subids)]
length(first_tp) # 20

# Just second time point
second_tp <- second_subids[!(second_subids %in% first_subids)]
length(second_tp) # 0

table(final_df[final_df$sesid == 1, 'female']) # 177 (100)
table(final_df[final_df$sesid == 2, 'female']) # 168 (89)
summary(final_df[final_df$sesid == 1, 'age_lab']) # min=11.88, median=13.95, mean=13.96, max=15.34
summary(final_df[final_df$sesid == 2, 'age_lab']) # min=13.93, median=15.98, mean=16.00, max=17.54, numna=2
table(final_df$ever) # 0->237, 1->296

summary(final_df$num_pastyear)
table(final_df$num_pastyear)


########### Descriptive - before removing NAs

# T1
d1 <- final_df[final_df$sesid == 1, ]
sum(is.na(d1$depression)) #0
sum(is.na(d1$age_mri)) #12
sum(is.na(d1$black)) #0
sum(is.na(d1$white)) #0
sum(is.na(d1$hispanic)) #0
sum(is.na(d1$BMIperc)) #0
sum(is.na(d1$PubCat)) #0
sum(is.na(d1$IPR)) #0
sum(is.na(d1$num_pastyear)) #5
sum(is.na(d1$female)) #0
sum(is.na(d1$exp_b_pos)) #31
sum(is.na(d1$FC_b_pos)) #37... why do these last two not match?
#MWMH: 229, 210, 184, 164, 132, 104 have exp but not FC
#These are all subjects that need to be excluded due to 
#missing vertices (i.e., low variance vertices)

# T2
d2 <- final_df[final_df$sesid == 2, ]
sum(is.na(d2$depression)) #0
sum(is.na(d2$age_mri)) #28
sum(is.na(d2$black)) #0
sum(is.na(d2$white)) #0
sum(is.na(d2$hispanic)) #0
sum(is.na(d2$BMIperc)) #0
sum(is.na(d2$PubCat)) #0
sum(is.na(d2$IPR)) #0
sum(is.na(d2$num_pastyear)) #1
sum(is.na(d2$female)) #0
sum(is.na(d2$exp_b_pos)) #30
sum(is.na(d2$FC_b_pos)) #37... why do these last two not match?
#MWMH: 229, 210, 184, 173, 164, 132, 104 have exp but not FC
#These are all subjects that need to be excluded due to 
#missing vertices (i.e., low variance vertices)

d2[!is.na(d2$age_mri) & is.na(d2$exp_b_pos),]
# MWMH112 has an age_mri, so was clearly scheduled, but there is no
# raw neuroimaging data for them at ses-2
# MWMH154 has all of their resting state data, but errored out on 
# autorecon in fmriprep

# T1 who have complete T2
d2b <- d2[!is.na(d2$FC_b_pos), ] 
d1b <- d1[d1$subid %in% d2b$subid,]
sum(is.na(d1b$depression)) #0
sum(is.na(d1b$age_mri)) #3
sum(is.na(d1b$black)) #0
sum(is.na(d1b$white)) #0
sum(is.na(d1b$hispanic)) #0
sum(is.na(d1b$BMIperc)) #0
sum(is.na(d1b$PubCat)) #0
sum(is.na(d1b$IPR)) #0
sum(is.na(d1b$num_pastyear)) #5
sum(is.na(d1b$female)) #0
sum(is.na(d1b$exp_b_pos)) #17
sum(is.na(d1b$FC_b_pos)) #17

d1b_anyna <- d1b[is.na(d1b$age_mri) | is.na(d1b$exp_b_pos),]

# Subjects who have an age_mri but no SN metrics (14)
d1b_anyna[!is.na(d1b_anyna$age_mri) & is.na(d1b_anyna$exp_b_pos), ]
# MWMH169: Resting state data was not collected
# MWMH178: Resting state data was not collected
# MWMH215: Resting state data was not collected
# MWMH233: Resting state data was not collected
# MWMH235: Resting state data was not collected
# MWMH242: No functional data was collected
# MWMH247: Resting state and faces data were not collected
# MWMH253: Resting state and faces data were not collected
# MWMH267: Resting state data was not collected
# MWMH271: Resting state data was not collected
# MWMH278: Resting state data was not collected
# MWMH279: No functional data was collected
# MWMH299: Resting state data was not collected
# MWMH358: Resting state data was collected... Made it through fmriprep... but only had 31 TRs

##### Differences on baseline demographics between those
##### with complete data at ses-2 vs. not?
subids_complete_ses2 <- d2b$subid #220
subids_incomplete_ses2 <- d1$subid[!(d1$subid %in% d2b$subid)] #57

# black, white, hispanic, BMIperc, PubCat, IPR, female, age_lab
d1$completeses2 <- d1$subid %in% subids_complete_ses2

sum(is.na(d1$black)) #0
chisq.test(d1$completeses2, d1$black) #not
sum(is.na(d1$white)) #0
chisq.test(d1$completeses2, d1$white) #sig
white_tab <- table(d1$completeses2, d1$white)
#Among those who do not have complete data at the second time point, 77% are non-white
white_tab[row.names(white_tab) == FALSE, '0']/sum(white_tab[row.names(white_tab) == FALSE,])
#Among those who do have complete data at the second time point, 56% are non-white
white_tab[row.names(white_tab) == TRUE, '0']/sum(white_tab[row.names(white_tab) == TRUE,])
sum(is.na(d1$hispanic)) #0
chisq.test(d1$completeses2, d1$hispanic) #not
sum(is.na(d1$BMIperc)) #0
t.test(d1[d1$completeses2 == 1, 'BMIperc'], d1[d1$completeses2 == 0, 'BMIperc']) #not
sum(is.na(d1$PubCat)) #0
t.test(d1[d1$completeses2 == 1, 'PubCat'], d1[d1$completeses2 == 0, 'PubCat']) #not
sum(is.na(d1$IPR)) #0
t.test(d1[d1$completeses2 == 1, 'IPR'], d1[d1$completeses2 == 0, 'IPR']) #not
sum(is.na(d1$female)) #0
chisq.test(d1$completeses2, d1$female) #not
sum(is.na(d1$age_lab)) #0
t.test(d1[d1$completeses2 == 1, 'age_lab'], d1[d1$completeses2 == 0, 'age_lab']) #not

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
summary(mri_df[mri_df$sesid == 1, 'age_lab'])
summary(mri_df[mri_df$sesid == 2, 'age_lab'])
summary(mri_df[mri_df$sesid == 1, 'age_mri'])
summary(mri_df[mri_df$sesid == 2, 'age_mri'])
table(mri_df$ever)

summary(mri_df$num_pastyear)
table(mri_df$num_pastyear)

summary(mri_df$days_mri_minus_lab)


########### Write the data

write.csv(final_df, '/projects/b1108/projects/violence_sex_development/data/combined_data_2024-11-01.csv', row.names=FALSE)






#
