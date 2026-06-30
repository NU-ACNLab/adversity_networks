### This script provides descriptive statistics on missing data
###
### Ellyn Butler
### June 30, 2026


#Load libraries
library(dplyr)

basedir <- '/projects/b1108/projects/adversity_networks/data/processed/'
demo_df <- read.csv(paste0(basedir, 'demographic/demographic_2026-03-31.csv'))
clin_df <- read.csv(paste0(basedir, 'clinical/clinical_2026-05-10.csv'))
net_df <- read.csv(paste0(basedir, 'neuroimaging/tabulated/surf_network_metrics_2026-03-26.csv'))
fd_df <- read.csv(paste0(basedir, 'neuroimaging/tabulated/meanFD_2026-03-19.csv'))
qual_df <- read.csv(paste0(basedir, 'neuroimaging/tabulated/quality_2026-03-19.csv'))
subjs_df <- read.csv(paste0(basedir, 'neuroimaging/tabulated/prior_subjects_2026-03-23.csv'))

# Exclusions only made on the second time point
demo_df <- demo_df[which(demo_df$sesid == 2), ]
clin_df <- clin_df[which(clin_df$sesid == 2), ]
net_df <- net_df[which(net_df$sesid == 2), ]
fd_df <- fd_df[which(fd_df$sesid == 2), ]

#### Total number of subjects who at least began to fill out the BDI at time 2 = 319
full_df <- demo_df[which(!is.na(demo_df$age_bdi)),]
nrow(full_df)

# Minutes calc
qual_df$minutes <- ((qual_df$nTRs - qual_df$nRegressors)*2.05)/60

# Sum the number of minutes
full_df$minutes <- NA
for (i in 1:nrow(full_df)) {
    subid <- full_df[i, 'subid']
    full_df[i, 'minutes'] <- sum(qual_df[qual_df$subid == subid, 'minutes']) 
}

#### Number of these who are missing sex data
sum(is.na(full_df$Sex))

#### Number of these who are missing life event data
tmp_df <- merge(full_df, clin_df, all.x = TRUE)
nrow(tmp_df[which(is.na(tmp_df$threatening_2_rate) | is.na(tmp_df$unstable_2_rate)), ])

#### Number of these who are missing depression data
# All the people of the 319 who are missing depression data
nrow(tmp_df[which(is.na(tmp_df$bdi_sum_2)), ]) #7
# People of the 319 who are missing depression data and not missing life event data
nrow(tmp_df[which(is.na(tmp_df$bdi_sum_2) & (!is.na(tmp_df$threatening_2_rate) | !is.na(tmp_df$unstable_2_rate))), ]) #7

#### Number of these who are missing neuroimaging data
# All the people of the 319 who are missing neuroimaging
tmp_df2 <- merge(full_df, fd_df, all.x = TRUE)
nrow(tmp_df2[which(is.na(tmp_df2$meanFD)), ]) #46
# People of the 319 who are missing neuroimaging data and not missing depression or life event data
tmp_df3 <- merge(tmp_df2, tmp_df)
nrow(tmp_df3[which(is.na(tmp_df3$meanFD) & (!is.na(tmp_df3$bdi_sum_2) & (!is.na(tmp_df3$threatening_2_rate) | !is.na(tmp_df3$unstable_2_rate)))), ]) #45

#### Number of these who have insufficient degrees of freedom (minutes < 5)
nrow(full_df[which(full_df$minutes < 5),]) #10
tmp_df4 <- merge(full_df, tmp_df3)
nrow(tmp_df4[which(tmp_df4$minutes < 5 & (!is.na(tmp_df4$meanFD) & !is.na(tmp_df4$bdi_sum_2) & (!is.na(tmp_df4$threatening_2_rate) | !is.na(tmp_df4$unstable_2_rate)))), ]) #0

#### Number of these who have a mean framewise displacement > 0.5
nrow(tmp_df2[which(tmp_df2$meanFD > 0.5),]) #17
nrow(tmp_df4[which(tmp_df2$meanFD > 0.5 & (!is.na(tmp_df4$minutes < 5) & !is.na(tmp_df4$meanFD) & !is.na(tmp_df4$bdi_sum_2) & (!is.na(tmp_df4$threatening_2_rate) | !is.na(tmp_df4$unstable_2_rate)))), ]) #15
