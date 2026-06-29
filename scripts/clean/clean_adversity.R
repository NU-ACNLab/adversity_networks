### This script does a lot of what clean_clinical.R does, 
### but it outputs the item level data for just the
### adversity constructs
###
### Ellyn Butler
### June 27, 2026

# Load libraries
library(tidyverse)
library(psych)

basedir <- '~/Documents/Northwestern/projects/adversity_networks/data/raw/clinical/'

####### RISE

# Load data
rise_bdi_df <- read.csv(paste0(basedir, 'ProjectRISE-17BDI_DATA_2026-03-20_1043.csv'))
rise_les_t2_df <- read.csv(paste0(basedir, 'ProjectRISE-102LEST2_DATA_2026-03-20_1049.csv'))
rise_les_t3_df <- read.csv(paste0(basedir, 'ProjectRISE-103LEST3_DATA_2026-03-20_1049.csv'))

rise_les_df <- merge(rise_les_t2_df[, c('rise_id', 'les_t2_start_date', 'les_t2_44', 'les_t2_44_freq',
                     'les_t2_137', 'les_t2_137_freq', 'les_t2_138', 'les_t2_138_freq',
                     'les_t2_141', 'les_t2_141_freq', 'les_t2_17', 'les_t2_17_freq',
                     'les_t2_114', 'les_t2_114_freq', 'les_t2_115', 'les_t2_115_freq',
                     'les_t2_120', 'les_t2_120_freq','les_t2_145', 'les_t2_145_freq',
                     'les_t2_133', 'les_t2_133_freq', 'les_t2_21', 'les_t2_21_freq',
                     'les_t2_140', 'les_t2_140_freq', 'les_t2_131', 'les_t2_131_freq',
                     'les_t2_117', 'les_t2_117_freq', 'les_t2_139', 'les_t2_139_freq')],
                     rise_les_t3_df[, c('rise_id', 'les_t3_end_date', 'les_t3_44', 'les_t3_44_freq',
                     'les_t3_137', 'les_t3_137_freq', 'les_t3_138', 'les_t3_138_freq',
                     'les_t3_141', 'les_t3_141_freq', 'les_t3_17', 'les_t3_17_freq',
                     'les_t3_114', 'les_t3_114_freq', 'les_t3_115', 'les_t3_115_freq',
                     'les_t3_120', 'les_t3_120_freq', 'les_t3_145', 'les_t3_145_freq',
                     'les_t3_133', 'les_t3_133_freq', 'les_t3_21', 'les_t3_21_freq',
                     'les_t3_140', 'les_t3_140_freq', 'les_t3_131', 'les_t3_131_freq',
                     'les_t3_117', 'les_t3_117_freq', 'les_t3_139', 'les_t3_139_freq')]
                     )
rise_les_df$subid <- rise_les_df$rise_id
rise_les_df$rise_id <- NULL


# Time 
rise_les_df$les_t2_start_date <- as.Date(rise_les_df$les_t2_start_date)
rise_les_df$les_t3_end_date <- as.Date(rise_les_df$les_t3_end_date)
rise_les_df$years_t1_t3 <- as.numeric((rise_les_df$les_t3_end_date - rise_les_df$les_t2_start_date)/365)

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

#5) You experienced a natural disaster (e.g., earthquake, flood) or human threat (e.g., bombing). 
#les_t3_145/les_t3_145_freq
for (i in 1:nrow(rise_les_df)) {
    if (!is.na(rise_les_df[i, 'les_t2_145']) & is.na(rise_les_df[i, 'les_t2_145_freq'])) {
        rise_les_df[i, 'les_t2_145_freq'] <- 0
    }
    if (!is.na(rise_les_df[i, 'les_t3_145']) & is.na(rise_les_df[i, 'les_t3_145_freq'])) {
        rise_les_df[i, 'les_t3_145_freq'] <- 0
    }
}

#6) Serious illness, accident, or injury. 
#les_t3_133/les_t3_133_freq
for (i in 1:nrow(rise_les_df)) {
    if (!is.na(rise_les_df[i, 'les_t2_133']) & is.na(rise_les_df[i, 'les_t2_133_freq'])) {
        rise_les_df[i, 'les_t2_133_freq'] <- 0
    }
    if (!is.na(rise_les_df[i, 'les_t3_133']) & is.na(rise_les_df[i, 'les_t3_133_freq'])) {
        rise_les_df[i, 'les_t3_133_freq'] <- 0
    }
}

#7) Exposed to negative, toxic or dangerous situations at work or school.  
#les_t3_21/les_t3_21_freq
for (i in 1:nrow(rise_les_df)) {
    if (!is.na(rise_les_df[i, 'les_t2_21']) & is.na(rise_les_df[i, 'les_t2_21_freq'])) {
        rise_les_df[i, 'les_t2_21_freq'] <- 0
    }
    if (!is.na(rise_les_df[i, 'les_t3_21']) & is.na(rise_les_df[i, 'les_t3_21_freq'])) {
        rise_les_df[i, 'les_t3_21_freq'] <- 0
    }
}

# Rates
rise_les_df$family_member_rate <- (rise_les_df$les_t2_44_freq + rise_les_df$les_t2_44_freq)/rise_les_df$years_t1_t3
rise_les_df$witness_rate <- (rise_les_df$les_t2_137_freq + rise_les_df$les_t3_137_freq)/rise_les_df$years_t1_t3
rise_les_df$verbal_rate <- (rise_les_df$les_t2_138_freq + rise_les_df$les_t3_138_freq)/rise_les_df$years_t1_t3
rise_les_df$direct_rate <- (rise_les_df$les_t2_141_freq + rise_les_df$les_t3_141_freq)/rise_les_df$years_t1_t3
rise_les_df$mass_rate <- (rise_les_df$les_t2_145_freq + rise_les_df$les_t3_145_freq)/rise_les_df$years_t1_t3
rise_les_df$serious_rate <- (rise_les_df$les_t2_133_freq + rise_les_df$les_t3_133_freq)/rise_les_df$years_t1_t3
rise_les_df$dangerous_rate <- (rise_les_df$les_t2_21_freq + rise_les_df$les_t3_21_freq)/rise_les_df$years_t1_t3


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

# Rates
rise_les_df$lack_money_rate <- (rise_les_df$les_t2_17_freq + rise_les_df$les_t3_17_freq)/rise_les_df$years_t1_t3
rise_les_df$disruptive_living_rate <- (rise_les_df$les_t2_114_freq + rise_les_df$les_t3_114_freq)/rise_les_df$years_t1_t3
rise_les_df$unforeseen_housing_rate <- (rise_les_df$les_t2_115_freq + rise_les_df$les_t3_115_freq)/rise_les_df$years_t1_t3
rise_les_df$stolen_rate <- (rise_les_df$les_t2_120_freq + rise_les_df$les_t3_120_freq)/rise_les_df$years_t1_t3
rise_les_df$sleep_disrupted_rate <- (rise_les_df$les_t2_131_freq + rise_les_df$les_t3_131_freq)/rise_les_df$years_t1_t3
rise_les_df$moved_rate <- (rise_les_df$les_t2_117_freq + rise_les_df$les_t3_117_freq)/rise_les_df$years_t1_t3
rise_les_df$home_broken_rate <- (rise_les_df$les_t2_139_freq + rise_les_df$les_t3_139_freq)/rise_les_df$years_t1_t3
rise_les_df$car_broken_rate <- (rise_les_df$les_t2_140_freq + rise_les_df$les_t3_140_freq)/rise_les_df$years_t1_t3


####### CREST

# Load data
crest_bdi_df <- read.csv(paste0(basedir, 'ProjectCREST-15BDI_DATA_2026-03-20_1056.csv'))
crest_les_t2_df <- read.csv(paste0(basedir, 'ProjectCREST-122LEST2_DATA_2026-03-20_1057.csv'))
crest_les_t3_df <- read.csv(paste0(basedir, 'ProjectCREST-123LEST3_DATA_2026-03-20_1057.csv'))

crest_les_df <- merge(crest_les_t2_df[, c('crest_id', 'les_t2_start_date', 'les_t2_44', 'les_t2_44_freq',
                     'les_t2_137', 'les_t2_137_freq', 'les_t2_138', 'les_t2_138_freq',
                     'les_t2_141', 'les_t2_141_freq', 'les_t2_17', 'les_t2_17_freq',
                     'les_t2_114', 'les_t2_114_freq', 'les_t2_115', 'les_t2_115_freq',
                     'les_t2_120', 'les_t2_120_freq','les_t2_145', 'les_t2_145_freq',
                     'les_t2_133', 'les_t2_133_freq', 'les_t2_21', 'les_t2_21_freq',
                     'les_t2_140', 'les_t2_140_freq', 'les_t2_131', 'les_t2_131_freq',
                     'les_t2_117', 'les_t2_117_freq', 'les_t2_139', 'les_t2_139_freq')],
                     crest_les_t3_df[, c('crest_id', 'les_t3_end_date', 'les_t3_44', 'les_t3_44_freq',
                     'les_t3_137', 'les_t3_137_freq', 'les_t3_138', 'les_t3_138_freq',
                     'les_t3_141', 'les_t3_141_freq', 'les_t3_17', 'les_t3_17_freq',
                     'les_t3_114', 'les_t3_114_freq', 'les_t3_115', 'les_t3_115_freq',
                     'les_t3_120', 'les_t3_120_freq', 'les_t3_145', 'les_t3_145_freq',
                     'les_t3_133', 'les_t3_133_freq', 'les_t3_21', 'les_t3_21_freq',
                     'les_t3_140', 'les_t3_140_freq', 'les_t3_131', 'les_t3_131_freq',
                     'les_t3_117', 'les_t3_117_freq', 'les_t3_139', 'les_t3_139_freq')]
                     )
crest_les_df$subid <- crest_les_df$crest_id
crest_les_df$crest_id <- NULL

# Time 
crest_les_df$les_t2_start_date <- as.Date(crest_les_df$les_t2_start_date)
crest_les_df$les_t3_end_date <- as.Date(crest_les_df$les_t3_end_date)
crest_les_df$years_t1_t3 <- as.numeric((crest_les_df$les_t3_end_date - crest_les_df$les_t2_start_date)/365)

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

#5) You experienced a natural disaster (e.g., earthquake, flood) or human threat (e.g., bombing). 
#les_t3_145/les_t3_145_freq
for (i in 1:nrow(crest_les_df)) {
    if (!is.na(crest_les_df[i, 'les_t2_145']) & is.na(crest_les_df[i, 'les_t2_145_freq'])) {
        crest_les_df[i, 'les_t2_145_freq'] <- 0
    }
    if (!is.na(crest_les_df[i, 'les_t3_145']) & is.na(crest_les_df[i, 'les_t3_145_freq'])) {
        crest_les_df[i, 'les_t3_145_freq'] <- 0
    }
}

#6) Serious illness, accident, or injury. 
#les_t3_133/les_t3_133_freq
for (i in 1:nrow(crest_les_df)) {
    if (!is.na(crest_les_df[i, 'les_t2_133']) & is.na(crest_les_df[i, 'les_t2_133_freq'])) {
        crest_les_df[i, 'les_t2_133_freq'] <- 0
    }
    if (!is.na(crest_les_df[i, 'les_t3_133']) & is.na(crest_les_df[i, 'les_t3_133_freq'])) {
        crest_les_df[i, 'les_t3_133_freq'] <- 0
    }
}

#7) Exposed to negative, toxic or dangerous situations at work or school.  
#les_t3_21/les_t3_21_freq
for (i in 1:nrow(crest_les_df)) {
    if (!is.na(crest_les_df[i, 'les_t2_21']) & is.na(crest_les_df[i, 'les_t2_21_freq'])) {
        crest_les_df[i, 'les_t2_21_freq'] <- 0
    }
    if (!is.na(crest_les_df[i, 'les_t3_21']) & is.na(crest_les_df[i, 'les_t3_21_freq'])) {
        crest_les_df[i, 'les_t3_21_freq'] <- 0
    }
}

# Rates
crest_les_df$family_member_rate <- (crest_les_df$les_t2_44_freq + crest_les_df$les_t2_44_freq)/crest_les_df$years_t1_t3
crest_les_df$witness_rate <- (crest_les_df$les_t2_137_freq + crest_les_df$les_t3_137_freq)/crest_les_df$years_t1_t3
crest_les_df$verbal_rate <- (crest_les_df$les_t2_138_freq + crest_les_df$les_t3_138_freq)/crest_les_df$years_t1_t3
crest_les_df$direct_rate <- (crest_les_df$les_t2_141_freq + crest_les_df$les_t3_141_freq)/crest_les_df$years_t1_t3
crest_les_df$mass_rate <- (crest_les_df$les_t2_145_freq + crest_les_df$les_t3_145_freq)/crest_les_df$years_t1_t3
crest_les_df$serious_rate <- (crest_les_df$les_t2_133_freq + crest_les_df$les_t3_133_freq)/crest_les_df$years_t1_t3
crest_les_df$dangerous_rate <- (crest_les_df$les_t2_21_freq + crest_les_df$les_t3_21_freq)/crest_les_df$years_t1_t3


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

# Rates
crest_les_df$lack_money_rate <- (crest_les_df$les_t2_17_freq + crest_les_df$les_t3_17_freq)/crest_les_df$years_t1_t3
crest_les_df$disruptive_living_rate <- (crest_les_df$les_t2_114_freq + crest_les_df$les_t3_114_freq)/crest_les_df$years_t1_t3
crest_les_df$unforeseen_housing_rate <- (crest_les_df$les_t2_115_freq + crest_les_df$les_t3_115_freq)/crest_les_df$years_t1_t3
crest_les_df$stolen_rate <- (crest_les_df$les_t2_120_freq + crest_les_df$les_t3_120_freq)/crest_les_df$years_t1_t3
crest_les_df$sleep_disrupted_rate <- (crest_les_df$les_t2_131_freq + crest_les_df$les_t3_131_freq)/crest_les_df$years_t1_t3
crest_les_df$moved_rate <- (crest_les_df$les_t2_117_freq + crest_les_df$les_t3_117_freq)/crest_les_df$years_t1_t3
crest_les_df$home_broken_rate <- (crest_les_df$les_t2_139_freq + crest_les_df$les_t3_139_freq)/crest_les_df$years_t1_t3
crest_les_df$car_broken_rate <- (crest_les_df$les_t2_140_freq + crest_les_df$les_t3_140_freq)/crest_les_df$years_t1_t3


##### Merge
rise_les_df <- rise_les_df[, c('subid', 'family_member_rate', 'witness_rate', 'verbal_rate', 'direct_rate',
                               'mass_rate', 'serious_rate', 'dangerous_rate', 'lack_money_rate', 
                               'disruptive_living_rate', 'unforeseen_housing_rate', 'stolen_rate', 
                               'sleep_disrupted_rate', 'moved_rate', 'home_broken_rate', 'car_broken_rate')]
crest_les_df <- crest_les_df[, c('subid', 'family_member_rate', 'witness_rate', 'verbal_rate', 'direct_rate',
                               'mass_rate', 'serious_rate', 'dangerous_rate', 'lack_money_rate', 
                               'disruptive_living_rate', 'unforeseen_housing_rate', 'stolen_rate', 
                               'sleep_disrupted_rate', 'moved_rate', 'home_broken_rate', 'car_broken_rate')]
les_df <- rbind(rise_les_df, crest_les_df)
write.csv(les_df, paste0('~/Documents/Northwestern/projects/adversity_networks/data/processed/clinical/adversity_', format(Sys.Date(), "%Y-%m-%d"), '.csv'), row.names = FALSE)