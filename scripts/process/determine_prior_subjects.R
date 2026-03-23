### This script selects randomly either the first or
### second session for prior construction, among
### those subjects that have both sessions, and selects
### only the second session for the subjects that just
### have the second session available
### 
### Ellyn Butler
### March 19, 2026 - March 23, 2026

set.seed(2000)

indir <- '/projects/b1108/projects/adversity_networks/data/processed/'

qual_df <- read.csv(paste0(indir, 'neuroimaging/tabulated/quality_2026-03-19.csv'))
fd_df <- read.csv(paste0(indir, 'neuroimaging/tabulated/meanFD_2026-03-19.csv')) # N = 246 (length(df[df$sesid == 2, 'sesid']))
clin_df <- read.csv(paste0(indir, 'clinical/clinical_2026-03-23.csv')) # N = 

# Select only time 2 FD vals (exclusions will be made here)
fd_df2 <- fd_df[which(fd_df$sesid == 2), ]

# Select only full time 2 cases from clinical
clin_df2 <- clin_df[which(complete.cases(clin_df[, c('bdi_sum_2', 'threatening_2', 'unstable_2')])),] #nrow = 306

# Calculate the number of minutes remaining
qual_df$minutes <- ((qual_df$nTRs - qual_df$nRegressors)*2.05)/60

# Merge fd and clin
df <- merge(fd_df2, clin_df2) # N = 

# Sum the number of minutes
df$minutes <- NA
for (i in 1:nrow(df)) {
    subid <- qual_df$subid == df[i, 'subid']
    sesid <- qual_df$sesid == df[i, 'sesid']
    df[i, 'minutes'] <- sum(qual_df[which(subid & sesid), 'minutes']) 
}

# Filter out subjects with fewer than 5 minutes remaining
sum(df$minutes < 5) # number of these subjects = 1
# range(df$minutes)... 2.15250 29.04167
df <- df[df$minutes > 5,]

# Filter out subjects with meanFD > .5
sum(df$meanFD > .5) # number of these subjects = 14
df <- df[df$meanFD < .5, ] # N = 246

# Minutes distribution info
summary(df$minutes)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  10.49   27.58   28.58   27.46   28.87   29.04 

#### Separate out the sessions
d1 <- fd_df[which(fd_df$sesid == 1), ]
d2 <- df

subids <- d2$subid

temp_ids <- data.frame(subid = subids, sesid = NA)

subids_onlyses2 <- subids[!(subids %in% d1$subid)]

temp_ids[temp_ids$subid %in% subids_onlyses2, 'sesid'] <- 2

sesid1 = 0
sesid2 = length(subids_onlyses2) #4
for (subid in subids) {
    if (is.na(temp_ids[temp_ids$subid == subid, 'sesid'])) {
        # If we have reached the max for data from ses-1 or the subid
        # does not have ses-1
        if (sesid1 == nrow(d2/2)) {
            sesid = 2
            sesid2 = sesid2 + 1
        # If the subid has ses-1 and we haven't reached the max for
        # either data from ses-1 or ses-2
        } else if (subid %in% d1$subid & sesid1 < nrow(d2/2) & sesid2 < nrow(d2/2)) {
            sesid <- sample(1:2, 1)
            if (sesid == 1) {
                sesid1 = sesid1 + 1
            } else {
                sesid2 = sesid2 + 1
            } 
        # If we have reached the max for data from ses-2
        } else if (sesid2 == nrow(d2/2)) {
            sesid = 1
            sesid1 = sesid1 + 1
        }
        temp_ids[temp_ids$subid == subid, 'sesid'] = sesid
    }
}

write.csv(temp_ids, paste0(indir, 'neuroimaging/tabulated/prior_subjects_', format(Sys.Date(), "%Y-%m-%d"), '.csv'), row.names = FALSE)