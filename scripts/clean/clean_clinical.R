### This script cleans the raw clinical data to 
### make it usable for analyses
###
### Ellyn Butler
### March 20, 2026 - 

# Load libraries
library(tidyverse)
library(psych)
library(ggplot2)
library(ggcorrplot)

basedir <- '~/Documents/Northwestern/projects/adversity_networks/data/raw/clinical/'

####### RISE

# Load data
rise_bdi_df <- read.csv(paste0(basedir, 'ProjectRISE-17BDI_DATA_2026-03-20_1043.csv'))
rise_les_t2_df <- read.csv(paste0(basedir, 'ProjectRISE-102LEST2_DATA_2026-03-20_1049.csv'))
rise_les_t3_df <- read.csv(paste0(basedir, 'ProjectRISE-103LEST3_DATA_2026-03-20_1049.csv'))

rise_les_df <- merge(rise_les_t2_df[, c('rise_id', 'les_t2_44', 'les_t2_44_freq',
                     'les_t2_137', 'les_t2_137_freq', 'les_t2_138', 'les_t2_138_freq',
                     'les_t2_141', 'les_t2_141_freq', 'les_t2_17', 'les_t2_17_freq',
                     'les_t2_114', 'les_t2_114_freq', 'les_t2_115', 'les_t2_115_freq',
                     'les_t2_120', 'les_t2_120_freq',
                     'les_t2_45', 'les_t2_45_freq', 'les_t2_145', 'les_t2_145_freq',
                     'les_t2_133', 'les_t2_133_freq', 'les_t2_21', 'les_t2_21_freq',
                     'les_t2_140', 'les_t2_140_freq', 'les_t2_131', 'les_t2_131_freq',
                     'les_t2_117', 'les_t2_117_freq', 'les_t2_139', 'les_t2_139_freq')],
                     rise_les_t3_df[, c('rise_id', 'les_t3_44', 'les_t3_44_freq',
                     'les_t3_137', 'les_t3_137_freq', 'les_t3_138', 'les_t3_138_freq',
                     'les_t3_141', 'les_t3_141_freq', 'les_t3_17', 'les_t3_17_freq',
                     'les_t3_114', 'les_t3_114_freq', 'les_t3_115', 'les_t3_115_freq',
                     'les_t3_120', 'les_t3_120_freq',
                     'les_t3_45', 'les_t3_45_freq', 'les_t3_145', 'les_t3_145_freq',
                     'les_t3_133', 'les_t3_133_freq', 'les_t3_21', 'les_t3_21_freq',
                     'les_t3_140', 'les_t3_140_freq', 'les_t3_131', 'les_t3_131_freq',
                     'les_t3_117', 'les_t3_117_freq', 'les_t3_139', 'les_t3_139_freq')]
                     )
rise_les_df$subid <- rise_les_df$rise_id
rise_les_df$rise_id <- NULL

# Recode session
rise_bdi_df$sesid <- recode(rise_bdi_df$redcap_event_name, 't1s2_b1_arm_1' = 1,
                            't2_b1_arm_1' = NaN, 't3s2_b1_arm_1' = 2,
                            't4_b1_arm_1' = NaN, 't5s2_b1_arm_1' = NaN)
rise_bdi_df$redcap_event_name <- NULL    

# Threatening
#1) Family member, close friend, or partner was in an accident, attacked, or experienced violence
#les_t2_44/les_t2_44_freq
for (i in 1:nrow(rise_les_df)) {
    if (!is.na(rise_les_df[i, 'les_t2_44']) & is.na(rise_les_df[i, 'les_t2_44_freq'])) {
        rise_les_df[i, 'les_t2_44_freq'] <- 0
    }
    if (!is.na(rise_les_df[i, 'les_t3_44']) & is.na(rise_les_df[i, 'les_t3_44_freq'])) {
        rise_les_df[i, 'les_t3_44_freq'] <- 0
    }
}

#2) Witnessed a serious accident or act of violence
#les_t2_137/les_t2_137_freq
for (i in 1:nrow(rise_les_df)) {
    if (!is.na(rise_les_df[i, 'les_t2_137']) & is.na(rise_les_df[i, 'les_t2_137_freq'])) {
        rise_les_df[i, 'les_t2_137_freq'] <- 0
    }
    if (!is.na(rise_les_df[i, 'les_t3_137']) & is.na(rise_les_df[i, 'les_t3_137_freq'])) {
        rise_les_df[i, 'les_t3_137_freq'] <- 0
    }
}

#3) Received verbal threats of violence from a stranger or someone [the participant] knows
#les_t2_138/les_t2_138_freq
for (i in 1:nrow(rise_les_df)) {
    if (!is.na(rise_les_df[i, 'les_t2_138']) & is.na(rise_les_df[i, 'les_t2_138_freq'])) {
        rise_les_df[i, 'les_t2_138_freq'] <- 0
    }
    if (!is.na(rise_les_df[i, 'les_t3_138']) & is.na(rise_les_df[i, 'les_t3_138_freq'])) {
        rise_les_df[i, 'les_t3_138_freq'] <- 0
    }
}

#4) You were in an accident, attacked, or experienced violence
#les_t2_141/les_t2_141_freq
for (i in 1:nrow(rise_les_df)) {
    if (!is.na(rise_les_df[i, 'les_t2_141']) & is.na(rise_les_df[i, 'les_t2_141_freq'])) {
        rise_les_df[i, 'les_t2_141_freq'] <- 0
    }
    if (!is.na(rise_les_df[i, 'les_t3_141']) & is.na(rise_les_df[i, 'les_t3_141_freq'])) {
        rise_les_df[i, 'les_t3_141_freq'] <- 0
    }
}

#5) Significant fight or argument with family member (including your child), close friend, or partner. 
#les_t3_45/les_t3_45_freq
for (i in 1:nrow(rise_les_df)) {
    if (!is.na(rise_les_df[i, 'les_t2_45']) & is.na(rise_les_df[i, 'les_t2_45_freq'])) {
        rise_les_df[i, 'les_t2_45_freq'] <- 0
    }
    if (!is.na(rise_les_df[i, 'les_t3_45']) & is.na(rise_les_df[i, 'les_t3_45_freq'])) {
        rise_les_df[i, 'les_t3_45_freq'] <- 0
    }
}

#6) You experienced a natural disaster (e.g., earthquake, flood) or human threat (e.g., bombing). 
#les_t3_145/les_t3_145_freq
for (i in 1:nrow(rise_les_df)) {
    if (!is.na(rise_les_df[i, 'les_t2_145']) & is.na(rise_les_df[i, 'les_t2_145_freq'])) {
        rise_les_df[i, 'les_t2_145_freq'] <- 0
    }
    if (!is.na(rise_les_df[i, 'les_t3_145']) & is.na(rise_les_df[i, 'les_t3_145_freq'])) {
        rise_les_df[i, 'les_t3_145_freq'] <- 0
    }
}

#7) Serious illness, accident, or injury. 
#les_t3_133/les_t3_133_freq
for (i in 1:nrow(rise_les_df)) {
    if (!is.na(rise_les_df[i, 'les_t2_133']) & is.na(rise_les_df[i, 'les_t2_133_freq'])) {
        rise_les_df[i, 'les_t2_133_freq'] <- 0
    }
    if (!is.na(rise_les_df[i, 'les_t3_133']) & is.na(rise_les_df[i, 'les_t3_133_freq'])) {
        rise_les_df[i, 'les_t3_133_freq'] <- 0
    }
}

#8) Exposed to negative, toxic or dangerous situations at work or school.  
#les_t3_21/les_t3_21_freq
for (i in 1:nrow(rise_les_df)) {
    if (!is.na(rise_les_df[i, 'les_t2_21']) & is.na(rise_les_df[i, 'les_t2_21_freq'])) {
        rise_les_df[i, 'les_t2_21_freq'] <- 0
    }
    if (!is.na(rise_les_df[i, 'les_t3_21']) & is.na(rise_les_df[i, 'les_t3_21_freq'])) {
        rise_les_df[i, 'les_t3_21_freq'] <- 0
    }
}


#Sum
rise_les_df$threatening_original_2 <- rowSums(rise_les_df[, c('les_t2_44_freq', 'les_t2_137_freq',
                                   'les_t2_138_freq', 'les_t2_141_freq', 'les_t3_44_freq', 
                                   'les_t3_137_freq', 'les_t3_138_freq', 
                                   'les_t3_141_freq')])
table(rise_les_df$threatening_original_2)
#   0   1   2   3   5   7  11 
# 124  16   5   7   1   1   1 
#... that's not good. Will need to change definition

rise_les_df$threatening_2 <- rowSums(rise_les_df[, c('les_t2_44_freq', 'les_t2_137_freq',
                                   'les_t2_138_freq', 'les_t2_141_freq', 'les_t3_44_freq', 
                                   'les_t3_137_freq', 'les_t3_138_freq', 
                                   'les_t3_141_freq',
                                   'les_t2_45_freq', 'les_t2_145_freq',
                                   'les_t2_133_freq', 'les_t2_21_freq', 'les_t3_45_freq', 
                                   'les_t3_145_freq', 'les_t3_133_freq', 
                                   'les_t3_21_freq')])
table(rise_les_df$threatening_2)

# Unstable
#1) You or your immediate family did not have enough money for one or more necessities (i.e., health care, food, housing, heat, electricity, or necessary clothing)
#les_t2_17/les_t2_17_freq
for (i in 1:nrow(rise_les_df)) {
    if (!is.na(rise_les_df[i, 'les_t2_17']) & is.na(rise_les_df[i, 'les_t2_17_freq'])) {
        rise_les_df[i, 'les_t2_17_freq'] <- 0
    }
    if (!is.na(rise_les_df[i, 'les_t3_17']) & is.na(rise_les_df[i, 'les_t3_17_freq'])) {
        rise_les_df[i, 'les_t3_17_freq'] <- 0
    }
}

#2) Lived in dirty/unsafe conditions or frequently could not complete schoolwork/important tasks because living environment was so noisy or disruptive
#les_t2_114/les_t2_114_freq
for (i in 1:nrow(rise_les_df)) {
    if (!is.na(rise_les_df[i, 'les_t2_114']) & is.na(rise_les_df[i, 'les_t2_114_freq'])) {
        rise_les_df[i, 'les_t2_114_freq'] <- 0
    }
    if (!is.na(rise_les_df[i, 'les_t3_114']) & is.na(rise_les_df[i, 'les_t3_114_freq'])) {
        rise_les_df[i, 'les_t3_114_freq'] <- 0
    }
}

#3) Were unable to continue living in current residence or unable to find a new residence due to an unfortunate situation (e.g., money problems, family problems, disagreement with landlord; etc.)
#les_t2_115/les_t2_115_freq
for (i in 1:nrow(rise_les_df)) {
    if (!is.na(rise_les_df[i, 'les_t2_115']) & is.na(rise_les_df[i, 'les_t2_115_freq'])) {
        rise_les_df[i, 'les_t2_115_freq'] <- 0
    }
    if (!is.na(rise_les_df[i, 'les_t3_115']) & is.na(rise_les_df[i, 'les_t3_115_freq'])) {
        rise_les_df[i, 'les_t3_115_freq'] <- 0
    }
}

#4) Important piece of property was stolen, broken, severely damaged, lost, deteriorating, or you were a victim of identity theft
#les_t2_120/les_t2_120_freq
for (i in 1:nrow(rise_les_df)) {
    if (!is.na(rise_les_df[i, 'les_t2_120']) & is.na(rise_les_df[i, 'les_t2_120_freq'])) {
        rise_les_df[i, 'les_t2_120_freq'] <- 0
    }
    if (!is.na(rise_les_df[i, 'les_t3_120']) & is.na(rise_les_df[i, 'les_t3_120_freq'])) {
        rise_les_df[i, 'les_t3_120_freq'] <- 0
    }
}

#5) Normal sleep schedule was frequently disrupted due to negative conditions. 
#les_t3_131/les_t3_131_freq
for (i in 1:nrow(rise_les_df)) {
    if (!is.na(rise_les_df[i, 'les_t2_131']) & is.na(rise_les_df[i, 'les_t2_131_freq'])) {
        rise_les_df[i, 'les_t2_131_freq'] <- 0
    }
    if (!is.na(rise_les_df[i, 'les_t3_131']) & is.na(rise_les_df[i, 'les_t3_131_freq'])) {
        rise_les_df[i, 'les_t3_131_freq'] <- 0
    }
}

#6) Moved
#les_t3_117/les_t3_117_freq
for (i in 1:nrow(rise_les_df)) {
    if (!is.na(rise_les_df[i, 'les_t2_117']) & is.na(rise_les_df[i, 'les_t2_117_freq'])) {
        rise_les_df[i, 'les_t2_117_freq'] <- 0
    }
    if (!is.na(rise_les_df[i, 'les_t3_117']) & is.na(rise_les_df[i, 'les_t3_117_freq'])) {
        rise_les_df[i, 'les_t3_117_freq'] <- 0
    }
}

#7) Your apartment, house, or room was broken into. 
#les_t3_139/les_t3_139_freq
for (i in 1:nrow(rise_les_df)) {
    if (!is.na(rise_les_df[i, 'les_t2_139']) & is.na(rise_les_df[i, 'les_t2_139_freq'])) {
        rise_les_df[i, 'les_t2_139_freq'] <- 0
    }
    if (!is.na(rise_les_df[i, 'les_t3_139']) & is.na(rise_les_df[i, 'les_t3_139_freq'])) {
        rise_les_df[i, 'les_t3_139_freq'] <- 0
    }
}

#8) Your car was broken into, vandalized, hit, damaged, or stolen. 
#les_t3_140/les_t3_140_freq
for (i in 1:nrow(rise_les_df)) {
    if (!is.na(rise_les_df[i, 'les_t2_140']) & is.na(rise_les_df[i, 'les_t2_140_freq'])) {
        rise_les_df[i, 'les_t2_140_freq'] <- 0
    }
    if (!is.na(rise_les_df[i, 'les_t3_140']) & is.na(rise_les_df[i, 'les_t3_140_freq'])) {
        rise_les_df[i, 'les_t3_140_freq'] <- 0
    }
}

#Sum
rise_les_df$unstable_original_2 <- rowSums(rise_les_df[, c('les_t2_17_freq', 'les_t2_114_freq',
                                   'les_t2_115_freq', 'les_t2_120_freq', 'les_t3_17_freq', 
                                   'les_t3_114_freq', 'les_t3_115_freq', 
                                   'les_t3_120_freq')])
table(rise_les_df$unstable_original_2)
#   0   1   2   3   4  20 
# 128  15   6   4   1   1 
#... that's not good. Will need to change definition

rise_les_df$unstable_2 <- rowSums(rise_les_df[, c('les_t2_17_freq', 'les_t2_114_freq',
                                   'les_t2_115_freq', 'les_t2_120_freq', 'les_t3_17_freq', 
                                   'les_t3_114_freq', 'les_t3_115_freq', 
                                   'les_t3_120_freq', 
                                   'les_t2_131_freq', 'les_t2_117_freq',
                                   'les_t2_139_freq', 'les_t2_140_freq', 'les_t3_131_freq', 
                                   'les_t3_117_freq', 'les_t3_139_freq', 
                                   'les_t3_140_freq')])
table(rise_les_df$unstable_2)
#  0  1  2  3  4  8 10 11 21 25 51 
# 97 26 13  8  4  1  2  1  1  1  1 

rise_les_df <- rise_les_df[, c('subid', 'threatening_original_2', 'threatening_2',
                                 'unstable_original_2', 'unstable_2')]

# Depression
rise_bdi_df$bdi_sum <- rowSums(rise_bdi_df[, paste0('bdi_', 1:21)])
rise_bdi_df$subid <- rise_bdi_df$rise_id
rise_bdi_df <- rise_bdi_df[!is.na(rise_bdi_df$subid), ]

rise_bdi_df1 <- rise_bdi_df[which(rise_bdi_df$sesid == 1), ]
rise_bdi_df1$bdi_sum_1 <- rise_bdi_df1$bdi_sum
rise_bdi_df1 <- rise_bdi_df1[, c('subid', 'bdi_sum_1')]

rise_bdi_df2 <- rise_bdi_df[which(rise_bdi_df$sesid == 2), ]
rise_bdi_df2$bdi_sum_2 <- rise_bdi_df2$bdi_sum
rise_bdi_df2 <- rise_bdi_df2[, c('subid', 'bdi_sum_2')]

# Merge
rise_df <- merge(rise_bdi_df1, rise_bdi_df2, all.x = TRUE)
rise_df <- merge(rise_df, rise_les_df, all.x = TRUE)

####### CREST

# Load data
crest_bdi_df <- read.csv(paste0(basedir, 'ProjectCREST-15BDI_DATA_2026-03-20_1056.csv'))
crest_les_t2_df <- read.csv(paste0(basedir, 'ProjectCREST-122LEST2_DATA_2026-03-20_1057.csv'))
crest_les_t3_df <- read.csv(paste0(basedir, 'ProjectCREST-123LEST3_DATA_2026-03-20_1057.csv'))

crest_les_df <- merge(crest_les_t2_df[, c('crest_id', 'les_t2_44', 'les_t2_44_freq',
                     'les_t2_137', 'les_t2_137_freq', 'les_t2_138', 'les_t2_138_freq',
                     'les_t2_141', 'les_t2_141_freq', 'les_t2_17', 'les_t2_17_freq',
                     'les_t2_114', 'les_t2_114_freq', 'les_t2_115', 'les_t2_115_freq',
                     'les_t2_120', 'les_t2_120_freq',
                     'les_t2_45', 'les_t2_45_freq', 'les_t2_145', 'les_t2_145_freq',
                     'les_t2_133', 'les_t2_133_freq', 'les_t2_21', 'les_t2_21_freq',
                     'les_t2_140', 'les_t2_140_freq', 'les_t2_131', 'les_t2_131_freq',
                     'les_t2_117', 'les_t2_117_freq', 'les_t2_139', 'les_t2_139_freq')],
                     crest_les_t3_df[, c('crest_id', 'les_t3_44', 'les_t3_44_freq',
                     'les_t3_137', 'les_t3_137_freq', 'les_t3_138', 'les_t3_138_freq',
                     'les_t3_141', 'les_t3_141_freq', 'les_t3_17', 'les_t3_17_freq',
                     'les_t3_114', 'les_t3_114_freq', 'les_t3_115', 'les_t3_115_freq',
                     'les_t3_120', 'les_t3_120_freq',
                     'les_t3_45', 'les_t3_45_freq', 'les_t3_145', 'les_t3_145_freq',
                     'les_t3_133', 'les_t3_133_freq', 'les_t3_21', 'les_t3_21_freq',
                     'les_t3_140', 'les_t3_140_freq', 'les_t3_131', 'les_t3_131_freq',
                     'les_t3_117', 'les_t3_117_freq', 'les_t3_139', 'les_t3_139_freq')]
                     )
crest_les_df$subid <- crest_les_df$crest_id
crest_les_df$crest_id <- NULL

# Recode session
crest_bdi_df$sesid <- recode(crest_bdi_df$redcap_event_name, 't1_b2_arm_1' = 1,
                             't2_b1_arm_1' = NaN, 't3_b2_arm_1' = 2, 't4_b1_arm_1' = NaN,
                             't5_b2_arm_1' = NaN, 't6_b1_arm_1' = NaN)
crest_bdi_df$redcap_event_name <- NULL    

# Threatening
#1) Family member, close friend, or partner was in an accident, attacked, or experienced violence
#les_t2_44/les_t2_44_freq
for (i in 1:nrow(crest_les_df)) {
    if (!is.na(crest_les_df[i, 'les_t2_44']) & is.na(crest_les_df[i, 'les_t2_44_freq'])) {
        crest_les_df[i, 'les_t2_44_freq'] <- 0
    }
    if (!is.na(crest_les_df[i, 'les_t3_44']) & is.na(crest_les_df[i, 'les_t3_44_freq'])) {
        crest_les_df[i, 'les_t3_44_freq'] <- 0
    }
}

#2) Witnessed a serious accident or act of violence
#les_t2_137/les_t2_137_freq
for (i in 1:nrow(crest_les_df)) {
    if (!is.na(crest_les_df[i, 'les_t2_137']) & is.na(crest_les_df[i, 'les_t2_137_freq'])) {
        crest_les_df[i, 'les_t2_137_freq'] <- 0
    }
    if (!is.na(crest_les_df[i, 'les_t3_137']) & is.na(crest_les_df[i, 'les_t3_137_freq'])) {
        crest_les_df[i, 'les_t3_137_freq'] <- 0
    }
}

#3) Received verbal threats of violence from a stranger or someone [the participant] knows
#les_t2_138/les_t2_138_freq
for (i in 1:nrow(crest_les_df)) {
    if (!is.na(crest_les_df[i, 'les_t2_138']) & is.na(crest_les_df[i, 'les_t2_138_freq'])) {
        crest_les_df[i, 'les_t2_138_freq'] <- 0
    }
    if (!is.na(crest_les_df[i, 'les_t3_138']) & is.na(crest_les_df[i, 'les_t3_138_freq'])) {
        crest_les_df[i, 'les_t3_138_freq'] <- 0
    }
}

#4) You were in an accident, attacked, or experienced violence
#les_t2_141/les_t2_141_freq
for (i in 1:nrow(crest_les_df)) {
    if (!is.na(crest_les_df[i, 'les_t2_141']) & is.na(crest_les_df[i, 'les_t2_141_freq'])) {
        crest_les_df[i, 'les_t2_141_freq'] <- 0
    }
    if (!is.na(crest_les_df[i, 'les_t3_141']) & is.na(crest_les_df[i, 'les_t3_141_freq'])) {
        crest_les_df[i, 'les_t3_141_freq'] <- 0
    }
}

#5) Significant fight or argument with family member (including your child), close friend, or partner. 
#les_t3_45/les_t3_45_freq
for (i in 1:nrow(crest_les_df)) {
    if (!is.na(crest_les_df[i, 'les_t2_45']) & is.na(crest_les_df[i, 'les_t2_45_freq'])) {
        crest_les_df[i, 'les_t2_45_freq'] <- 0
    }
    if (!is.na(crest_les_df[i, 'les_t3_45']) & is.na(crest_les_df[i, 'les_t3_45_freq'])) {
        crest_les_df[i, 'les_t3_45_freq'] <- 0
    }
}

#6) You experienced a natural disaster (e.g., earthquake, flood) or human threat (e.g., bombing). 
#les_t3_145/les_t3_145_freq
for (i in 1:nrow(crest_les_df)) {
    if (!is.na(crest_les_df[i, 'les_t2_145']) & is.na(crest_les_df[i, 'les_t2_145_freq'])) {
        crest_les_df[i, 'les_t2_145_freq'] <- 0
    }
    if (!is.na(crest_les_df[i, 'les_t3_145']) & is.na(crest_les_df[i, 'les_t3_145_freq'])) {
        crest_les_df[i, 'les_t3_145_freq'] <- 0
    }
}

#7) Serious illness, accident, or injury. 
#les_t3_133/les_t3_133_freq
for (i in 1:nrow(crest_les_df)) {
    if (!is.na(crest_les_df[i, 'les_t2_133']) & is.na(crest_les_df[i, 'les_t2_133_freq'])) {
        crest_les_df[i, 'les_t2_133_freq'] <- 0
    }
    if (!is.na(crest_les_df[i, 'les_t3_133']) & is.na(crest_les_df[i, 'les_t3_133_freq'])) {
        crest_les_df[i, 'les_t3_133_freq'] <- 0
    }
}

#8) Exposed to negative, toxic or dangerous situations at work or school.  
#les_t3_21/les_t3_21_freq
for (i in 1:nrow(crest_les_df)) {
    if (!is.na(crest_les_df[i, 'les_t2_21']) & is.na(crest_les_df[i, 'les_t2_21_freq'])) {
        crest_les_df[i, 'les_t2_21_freq'] <- 0
    }
    if (!is.na(crest_les_df[i, 'les_t3_21']) & is.na(crest_les_df[i, 'les_t3_21_freq'])) {
        crest_les_df[i, 'les_t3_21_freq'] <- 0
    }
}


#Sum
crest_les_df$threatening_original_2 <- rowSums(crest_les_df[, c('les_t2_44_freq', 'les_t2_137_freq',
                                   'les_t2_138_freq', 'les_t2_141_freq', 'les_t3_44_freq', 
                                   'les_t3_137_freq', 'les_t3_138_freq', 
                                   'les_t3_141_freq')])
table(crest_les_df$threatening_original_2)

crest_les_df$threatening_2 <- rowSums(crest_les_df[, c('les_t2_44_freq', 'les_t2_137_freq',
                                   'les_t2_138_freq', 'les_t2_141_freq', 'les_t3_44_freq', 
                                   'les_t3_137_freq', 'les_t3_138_freq', 
                                   'les_t3_141_freq',
                                   'les_t2_45_freq', 'les_t2_145_freq',
                                   'les_t2_133_freq', 'les_t2_21_freq', 'les_t3_45_freq', 
                                   'les_t3_145_freq', 'les_t3_133_freq', 
                                   'les_t3_21_freq')])
table(crest_les_df$threatening_2)

# Unstable
#1) You or your immediate family did not have enough money for one or more necessities (i.e., health care, food, housing, heat, electricity, or necessary clothing)
#les_t2_17/les_t2_17_freq
for (i in 1:nrow(crest_les_df)) {
    if (!is.na(crest_les_df[i, 'les_t2_17']) & is.na(crest_les_df[i, 'les_t2_17_freq'])) {
        crest_les_df[i, 'les_t2_17_freq'] <- 0
    }
    if (!is.na(crest_les_df[i, 'les_t3_17']) & is.na(crest_les_df[i, 'les_t3_17_freq'])) {
        crest_les_df[i, 'les_t3_17_freq'] <- 0
    }
}

#2) Lived in dirty/unsafe conditions or frequently could not complete schoolwork/important tasks because living environment was so noisy or disruptive
#les_t2_114/les_t2_114_freq
for (i in 1:nrow(crest_les_df)) {
    if (!is.na(crest_les_df[i, 'les_t2_114']) & is.na(crest_les_df[i, 'les_t2_114_freq'])) {
        crest_les_df[i, 'les_t2_114_freq'] <- 0
    }
    if (!is.na(crest_les_df[i, 'les_t3_114']) & is.na(crest_les_df[i, 'les_t3_114_freq'])) {
        crest_les_df[i, 'les_t3_114_freq'] <- 0
    }
}

#3) Were unable to continue living in current residence or unable to find a new residence due to an unfortunate situation (e.g., money problems, family problems, disagreement with landlord; etc.)
#les_t2_115/les_t2_115_freq
for (i in 1:nrow(crest_les_df)) {
    if (!is.na(crest_les_df[i, 'les_t2_115']) & is.na(crest_les_df[i, 'les_t2_115_freq'])) {
        crest_les_df[i, 'les_t2_115_freq'] <- 0
    }
    if (!is.na(crest_les_df[i, 'les_t3_115']) & is.na(crest_les_df[i, 'les_t3_115_freq'])) {
        crest_les_df[i, 'les_t3_115_freq'] <- 0
    }
}

#4) Important piece of property was stolen, broken, severely damaged, lost, deteriorating, or you were a victim of identity theft
#les_t2_120/les_t2_120_freq
for (i in 1:nrow(crest_les_df)) {
    if (!is.na(crest_les_df[i, 'les_t2_120']) & is.na(crest_les_df[i, 'les_t2_120_freq'])) {
        crest_les_df[i, 'les_t2_120_freq'] <- 0
    }
    if (!is.na(crest_les_df[i, 'les_t3_120']) & is.na(crest_les_df[i, 'les_t3_120_freq'])) {
        crest_les_df[i, 'les_t3_120_freq'] <- 0
    }
}

#5) Normal sleep schedule was frequently disrupted due to negative conditions. 
#les_t3_131/les_t3_131_freq
for (i in 1:nrow(crest_les_df)) {
    if (!is.na(crest_les_df[i, 'les_t2_131']) & is.na(crest_les_df[i, 'les_t2_131_freq'])) {
        crest_les_df[i, 'les_t2_131_freq'] <- 0
    }
    if (!is.na(crest_les_df[i, 'les_t3_131']) & is.na(crest_les_df[i, 'les_t3_131_freq'])) {
        crest_les_df[i, 'les_t3_131_freq'] <- 0
    }
}

#6) Moved
#les_t3_117/les_t3_117_freq
for (i in 1:nrow(crest_les_df)) {
    if (!is.na(crest_les_df[i, 'les_t2_117']) & is.na(crest_les_df[i, 'les_t2_117_freq'])) {
        crest_les_df[i, 'les_t2_117_freq'] <- 0
    }
    if (!is.na(crest_les_df[i, 'les_t3_117']) & is.na(crest_les_df[i, 'les_t3_117_freq'])) {
        crest_les_df[i, 'les_t3_117_freq'] <- 0
    }
}

#7) Your apartment, house, or room was broken into. 
#les_t3_139/les_t3_139_freq
for (i in 1:nrow(crest_les_df)) {
    if (!is.na(crest_les_df[i, 'les_t2_139']) & is.na(crest_les_df[i, 'les_t2_139_freq'])) {
        crest_les_df[i, 'les_t2_139_freq'] <- 0
    }
    if (!is.na(crest_les_df[i, 'les_t3_139']) & is.na(crest_les_df[i, 'les_t3_139_freq'])) {
        crest_les_df[i, 'les_t3_139_freq'] <- 0
    }
}

#8) Your car was broken into, vandalized, hit, damaged, or stolen. 
#les_t3_140/les_t3_140_freq
for (i in 1:nrow(crest_les_df)) {
    if (!is.na(crest_les_df[i, 'les_t2_140']) & is.na(crest_les_df[i, 'les_t2_140_freq'])) {
        crest_les_df[i, 'les_t2_140_freq'] <- 0
    }
    if (!is.na(crest_les_df[i, 'les_t3_140']) & is.na(crest_les_df[i, 'les_t3_140_freq'])) {
        crest_les_df[i, 'les_t3_140_freq'] <- 0
    }
}

#Sum
crest_les_df$unstable_original_2 <- rowSums(crest_les_df[, c('les_t2_17_freq', 'les_t2_114_freq',
                                   'les_t2_115_freq', 'les_t2_120_freq', 'les_t3_17_freq', 
                                   'les_t3_114_freq', 'les_t3_115_freq', 
                                   'les_t3_120_freq')])
table(crest_les_df$unstable_original_2)

crest_les_df$unstable_2 <- rowSums(crest_les_df[, c('les_t2_17_freq', 'les_t2_114_freq',
                                   'les_t2_115_freq', 'les_t2_120_freq', 'les_t3_17_freq', 
                                   'les_t3_114_freq', 'les_t3_115_freq', 
                                   'les_t3_120_freq', 
                                   'les_t2_131_freq', 'les_t2_117_freq',
                                   'les_t2_139_freq', 'les_t2_140_freq', 'les_t3_131_freq', 
                                   'les_t3_117_freq', 'les_t3_139_freq', 
                                   'les_t3_140_freq')])
table(crest_les_df$unstable_2)

crest_les_df <- crest_les_df[, c('subid', 'threatening_original_2', 'threatening_2',
                                 'unstable_original_2', 'unstable_2')]

# Depression
crest_bdi_df$bdi_sum <- rowSums(crest_bdi_df[, paste0('bdi_', 1:21)])
crest_bdi_df$subid <- crest_bdi_df$crest_id

crest_bdi_df1 <- crest_bdi_df[which(crest_bdi_df$sesid == 1), ]
crest_bdi_df1$bdi_sum_1 <- crest_bdi_df1$bdi_sum
crest_bdi_df1 <- crest_bdi_df1[, c('subid', 'bdi_sum_1')]

crest_bdi_df2 <- crest_bdi_df[which(crest_bdi_df$sesid == 2), ]
crest_bdi_df2$bdi_sum_2 <- crest_bdi_df2$bdi_sum
crest_bdi_df2 <- crest_bdi_df2[, c('subid', 'bdi_sum_2')]

# Merge
crest_df <- merge(crest_bdi_df1, crest_bdi_df2, all = TRUE)
crest_df <- merge(crest_df, crest_les_df, all = TRUE)

####### Finalize and export

df <- rbind(rise_df, crest_df)
write.csv(df_full, paste0('~/Documents/Northwestern/projects/adversity_networks/data/processed/clinical/clinical_', format(Sys.Date(), "%Y-%m-%d"), '.csv'), row.names = FALSE)