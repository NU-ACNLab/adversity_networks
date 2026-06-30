### This code focuses on session 2 for the analyses
###
### Ellyn Butler
### March 26, 2026 - July 1, 2026

set.seed(2000)

library(reshape2)
library(MASS)
library(dplyr)
library(sjPlot)
library(ggplot2)
library(ggpubr)
library(sjmisc)
library(sjlabelled)
library(mlr)
library(BayesFactor)
library(mediation)

#library(lmerTest) #confint

df <- read.csv('~/Documents/Northwestern/projects/adversity_networks/data/processed/combined/combined_data_2026-06-01.csv')
df <- df[which(!is.na(df$exp_salienceb_pos_2)), ] #Lost 50132, 50185, 100055 to `id_engagements()` errors

# Basics
dim(df) # N = 243
range(df$age_mri) # 14.23562 - 19.41370
mean(df$threatening_2_rate > 0) # 39.918%
mean(df[df$Sex == 'Female', 'threatening_2_rate']) # 1.053
mean(df[df$Sex == 'Male', 'threatening_2_rate']) # 0.598

df$threatening_2_rate <- scale(df$threatening_2_rate)
rate_mod <- lm(threatening_2_rate ~ Sex, data = df) # Males experience fewer instances of threatening events than females on average
t.test(threatening_2_rate ~ Sex, data = df) #t = 2.386, df = 227.656, p-value = 0.018
summary(rate_mod)

mean(df$unstable_2_rate > 0) # 29.63%
mean(df[df$Sex == 'Female', 'unstable_2_rate']) # 1.068
mean(df[df$Sex == 'Male', 'unstable_2_rate']) # 1.136

df$unstable_2_rate <- scale(df$unstable_2_rate)
rate_mod <- lm(unstable_2_rate ~ Sex, data = df)
t.test(unstable_2_rate ~ Sex, data = df) #t = -0.135, df = 159,576, p-value = 0.893
summary(rate_mod)


##### Cleaning up
df$Sex <- factor(df$Sex)
df$Sex <- relevel(df$Sex, ref = 'Male')

df$Familys_Gross_Total_Income[
  df$Familys_Gross_Total_Income == 'Not known'
] <- NA
df$Familys_Gross_Total_Income <- ordered(df$Familys_Gross_Total_Income, c('$20,000 - $34,999',
                                    '$35,000 - $49,999', '$50,000 - $74,999', '$75,000 - $99,999', 
                                    '$100,000 - $149,999', '$150,000 - $199,999', '$200,000 - $249,999', 
                                    '$250,000 - $299,999', '$300,000 and higher'))
df$Familys_Gross_Total_Income <- as.numeric(df$Familys_Gross_Total_Income)

df <- df |>
  mutate(Race = case_match(
    Race,
    'Caucasian/White'             ~ 'Caucasian/White',
    'African American or Black'   ~ 'African American or Black',
    c('Asian-American or Asian',
      'Biracial/Multiracial',
      'Native Hawaiian or Other Pacific Islander',
      'Other  {demo_race_other}') ~ 'Other',
    .default = Race
  ))
df$Race <- factor(df$Race) 
df$Race <- relevel(df$Race, ref = 'Other')

vars <- c('age_bdi', 'bdi_sum_1', 'bdi_sum_2', 'exp_salienceb_pos_1',
          'FC_pers_salienceb_pos_1', 'exp_dorsalattentionb_pos_1', 'FC_pers_dorsalattentionb_pos_1',
          'exp_salienceb_pos_2', 'FC_pers_salienceb_pos_2', 'exp_dorsalattentionb_pos_2',
          'FC_pers_dorsalattentionb_pos_2', 'Familys_Gross_Total_Income')
df[, vars] <- scale(df[, vars])



##### Modeling
# (4) - tests if there are main effects and interactions for threatening/unstable events 
# and sex on depression (two different models)
# Base
threat_dep_mod <- lm(bdi_sum_2 ~ Sex*threatening_2_rate, data = df) # Hypothesis 1
unstable_dep_mod <- lm(bdi_sum_2 ~ Sex*unstable_2_rate, data = df) # Hypothesis 2

# Covariates: Outcome at time 1
threat_dep_mod1 <- lm(bdi_sum_2 ~ Sex*threatening_2_rate + bdi_sum_1, data = df)
unstable_dep_mod1 <- lm(bdi_sum_2 ~ Sex*unstable_2_rate + bdi_sum_1, data = df)

# Covariates: Demographics
threat_dep_mod2 <- lm(bdi_sum_2 ~ Sex*threatening_2_rate + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
unstable_dep_mod2 <- lm(bdi_sum_2 ~ Sex*unstable_2_rate + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)

# Covariates: Demographics + Outcome at time 1
threat_dep_mod3 <- lm(bdi_sum_2 ~ Sex*threatening_2_rate + bdi_sum_1 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
unstable_dep_mod3 <- lm(bdi_sum_2 ~ Sex*unstable_2_rate + bdi_sum_1 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)

# Tables
tab_model(threat_dep_mod, threat_dep_mod1, threat_dep_mod2, threat_dep_mod3) 
tab_model(unstable_dep_mod, unstable_dep_mod1, unstable_dep_mod2, unstable_dep_mod3)

# Follow up
cor.test(df[df$Sex == 'Female', 'threatening_2_rate'], df[df$Sex == 'Female', 'bdi_sum_2']) #r = 0.156, t = 1.8595, df = 139, p-value = 0.06507
cor.test(df[df$Sex == 'Male', 'threatening_2_rate'], df[df$Sex == 'Male', 'bdi_sum_2']) #r = 0.344, t = 3.665, df = 100, p-value = 0.000398
cor.test(df[df$Sex == 'Female', 'unstable_2_rate'], df[df$Sex == 'Female', 'bdi_sum_2']) #r = 0.168, t = 2.0147, df = 139, p-value = 0.04587
cor.test(df[df$Sex == 'Male', 'unstable_2_rate'], df[df$Sex == 'Male', 'bdi_sum_2']) #r = -0.046, t = -0.45968, df = 100, p-value = 0.6467

# (5) - tests if there are main effects and interactions for threatening/unstable events 
# and sex on salience/dorsal attention network expansion/FC (eight different models)

# Base
threat_salexp_mod <- lm(exp_salienceb_pos_2 ~ Sex*threatening_2_rate, data = df)
threat_salFC_mod <- lm(FC_pers_salienceb_pos_2 ~ Sex*threatening_2_rate, data = df)
threat_dorexp_mod <- lm(exp_dorsalattentionb_pos_2 ~ Sex*threatening_2_rate, data = df) # Hypothesis 3a
threat_dorFC_mod <- lm(FC_pers_dorsalattentionb_pos_2 ~ Sex*threatening_2_rate, data = df) # Hypothesis 3b
unstable_salexp_mod <- lm(exp_salienceb_pos_2 ~ Sex*unstable_2_rate, data = df) # Hypothesis 4a
unstable_salFC_mod <- lm(FC_pers_salienceb_pos_2 ~ Sex*unstable_2_rate, data = df) # Hypothesis 4b
unstable_dorexp_mod <- lm(exp_dorsalattentionb_pos_2 ~ Sex*unstable_2_rate, data = df)
unstable_dorFC_mod <- lm(FC_pers_dorsalattentionb_pos_2 ~ Sex*unstable_2_rate, data = df)

# Covariates: Outcome at time 1
threat_salexp_mod1 <- lm(exp_salienceb_pos_2 ~ Sex*threatening_2_rate + exp_salienceb_pos_1, data = df)
threat_salFC_mod1 <- lm(FC_pers_salienceb_pos_2 ~ Sex*threatening_2_rate + FC_pers_salienceb_pos_1, data = df)
threat_dorexp_mod1 <- lm(exp_dorsalattentionb_pos_2 ~ Sex*threatening_2_rate + exp_dorsalattentionb_pos_1, data = df)
threat_dorFC_mod1 <- lm(FC_pers_dorsalattentionb_pos_2 ~ Sex*threatening_2_rate + FC_pers_dorsalattentionb_pos_1, data = df)
unstable_salexp_mod1 <- lm(exp_salienceb_pos_2 ~ Sex*unstable_2_rate + exp_salienceb_pos_1, data = df)
unstable_salFC_mod1 <- lm(FC_pers_salienceb_pos_2 ~ Sex*unstable_2_rate + FC_pers_salienceb_pos_1, data = df)
unstable_dorexp_mod1 <- lm(exp_dorsalattentionb_pos_2 ~ Sex*unstable_2_rate + exp_dorsalattentionb_pos_1, data = df)
unstable_dorFC_mod1 <- lm(FC_pers_dorsalattentionb_pos_2 ~ Sex*unstable_2_rate + FC_pers_dorsalattentionb_pos_1, data = df)

# Covariates: Demographics
threat_salexp_mod2 <- lm(exp_salienceb_pos_2 ~ Sex*threatening_2_rate + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
threat_salFC_mod2 <- lm(FC_pers_salienceb_pos_2 ~ Sex*threatening_2_rate + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
threat_dorexp_mod2 <- lm(exp_dorsalattentionb_pos_2 ~ Sex*threatening_2_rate + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
threat_dorFC_mod2 <- lm(FC_pers_dorsalattentionb_pos_2 ~ Sex*threatening_2_rate + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
unstable_salexp_mod2 <- lm(exp_salienceb_pos_2 ~ Sex*unstable_2_rate + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
unstable_salFC_mod2 <- lm(FC_pers_salienceb_pos_2 ~ Sex*unstable_2_rate + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
unstable_dorexp_mod2 <- lm(exp_dorsalattentionb_pos_2 ~ Sex*unstable_2_rate + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
unstable_dorFC_mod2 <- lm(FC_pers_dorsalattentionb_pos_2 ~ Sex*unstable_2_rate + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)

# Covariates: Demographics + Outcome at time 1
threat_salexp_mod3 <- lm(exp_salienceb_pos_2 ~ Sex*threatening_2_rate + exp_salienceb_pos_1 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
threat_salFC_mod3 <- lm(FC_pers_salienceb_pos_2 ~ Sex*threatening_2_rate + FC_pers_salienceb_pos_1 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
threat_dorexp_mod3 <- lm(exp_dorsalattentionb_pos_2 ~ Sex*threatening_2_rate + exp_dorsalattentionb_pos_1 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
threat_dorFC_mod3 <- lm(FC_pers_dorsalattentionb_pos_2 ~ Sex*threatening_2_rate + FC_pers_dorsalattentionb_pos_1 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
unstable_salexp_mod3 <- lm(exp_salienceb_pos_2 ~ Sex*unstable_2_rate + exp_salienceb_pos_1 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
unstable_salFC_mod3 <- lm(FC_pers_salienceb_pos_2 ~ Sex*unstable_2_rate + FC_pers_salienceb_pos_1 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
unstable_dorexp_mod3 <- lm(exp_dorsalattentionb_pos_2 ~ Sex*unstable_2_rate + exp_dorsalattentionb_pos_1 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
unstable_dorFC_mod3 <- lm(FC_pers_dorsalattentionb_pos_2 ~ Sex*unstable_2_rate + FC_pers_dorsalattentionb_pos_1 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)

# Tables... non sig (except for sex and baseline main effects, basically)
tab_model(threat_salexp_mod, threat_salexp_mod1, threat_salexp_mod2, threat_salexp_mod3) #
tab_model(threat_salFC_mod, threat_salFC_mod1, threat_salFC_mod2, threat_salFC_mod3) #
tab_model(threat_dorexp_mod, threat_dorexp_mod1, threat_dorexp_mod2, threat_dorexp_mod3) #
tab_model(threat_dorFC_mod, threat_dorFC_mod1, threat_dorFC_mod2, threat_dorFC_mod3) #
tab_model(unstable_salexp_mod, unstable_salexp_mod1, unstable_salexp_mod2, unstable_salexp_mod3) #
tab_model(unstable_salFC_mod, unstable_salFC_mod1, unstable_salFC_mod2, unstable_salFC_mod3) #
tab_model(unstable_dorexp_mod, unstable_dorexp_mod1, unstable_dorexp_mod2, unstable_dorexp_mod3) #
tab_model(unstable_dorFC_mod, unstable_dorFC_mod1, unstable_dorFC_mod2, unstable_dorFC_mod3) #

# (6) - tests if there are main effects and interactions for sex and salience/dorsal 
# attention network expansion/FC on depression (four different models)

# Base
salexp_dep_mod <- lm(bdi_sum_2 ~ Sex*exp_salienceb_pos_2, data = df)
salFC_dep_mod <- lm(bdi_sum_2 ~ Sex*FC_pers_salienceb_pos_2, data = df)
dorexp_dep_mod <- lm(bdi_sum_2 ~ Sex*exp_dorsalattentionb_pos_2, data = df)
dorFC_dep_mod <- lm(bdi_sum_2 ~ Sex*FC_pers_dorsalattentionb_pos_2, data = df)

# Covariates: Outcome at time 1
salexp_dep_mod1 <- lm(bdi_sum_2 ~ Sex*exp_salienceb_pos_2 + bdi_sum_1, data = df)
salFC_dep_mod1 <- lm(bdi_sum_2 ~ Sex*FC_pers_salienceb_pos_2 + bdi_sum_1, data = df)
dorexp_dep_mod1 <- lm(bdi_sum_2 ~ Sex*exp_dorsalattentionb_pos_2 + bdi_sum_1, data = df)
dorFC_dep_mod1 <- lm(bdi_sum_2 ~ Sex*FC_pers_dorsalattentionb_pos_2 + bdi_sum_1, data = df)

# Covariates: Demographics
salexp_dep_mod2 <- lm(bdi_sum_2 ~ Sex*exp_salienceb_pos_2 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
salFC_dep_mod2 <- lm(bdi_sum_2 ~ Sex*FC_pers_salienceb_pos_2 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
dorexp_dep_mod2 <- lm(bdi_sum_2 ~ Sex*exp_dorsalattentionb_pos_2 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
dorFC_dep_mod2 <- lm(bdi_sum_2 ~ Sex*FC_pers_dorsalattentionb_pos_2 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)

# Covariates: Demographics + Outcome at time 1
salexp_dep_mod3 <- lm(bdi_sum_2 ~ Sex*exp_salienceb_pos_2 + bdi_sum_1 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
salFC_dep_mod3 <- lm(bdi_sum_2 ~ Sex*FC_pers_salienceb_pos_2 + bdi_sum_1 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
dorexp_dep_mod3 <- lm(bdi_sum_2 ~ Sex*exp_dorsalattentionb_pos_2 + bdi_sum_1 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
dorFC_dep_mod3 <- lm(bdi_sum_2 ~ Sex*FC_pers_dorsalattentionb_pos_2 + bdi_sum_1 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)

# Tables... non sig (except for sex and baseline main effects)
tab_model(salexp_dep_mod, salexp_dep_mod1, salexp_dep_mod2, salexp_dep_mod3) #
tab_model(salFC_dep_mod, salFC_dep_mod1, salFC_dep_mod2, salFC_dep_mod3) #
tab_model(dorexp_dep_mod, dorexp_dep_mod1, dorexp_dep_mod2, dorexp_dep_mod3) #
tab_model(dorFC_dep_mod, dorFC_dep_mod1, dorFC_dep_mod2, dorFC_dep_mod3) #

# (7) - tests if after controlling for the mediators (salience/dorsal attention network
# expansion/FC), the association between the exposure variables (threatening/unstable events) 
# and the outcome (depression) remain for each sex (eight models)

# Base 
threat_salexp_dep_mod <- lm(bdi_sum_2 ~ Sex*threatening_2_rate + exp_salienceb_pos_2 + Sex:exp_salienceb_pos_2, data = df)
threat_salFC_dep_mod <- lm(bdi_sum_2 ~ Sex*threatening_2_rate + FC_pers_salienceb_pos_2 + Sex:FC_pers_salienceb_pos_2, data = df)
threat_dorexp_dep_mod <- lm(bdi_sum_2 ~ Sex*threatening_2_rate + exp_dorsalattentionb_pos_2 + Sex:exp_dorsalattentionb_pos_2, data = df)
threat_dorFC_dep_mod <- lm(bdi_sum_2 ~ Sex*threatening_2_rate + FC_pers_dorsalattentionb_pos_2 + Sex:FC_pers_dorsalattentionb_pos_2, data = df)
unstable_salexp_dep_mod <- lm(bdi_sum_2 ~ Sex*unstable_2_rate + exp_salienceb_pos_2 + Sex:exp_salienceb_pos_2, data = df)
unstable_salFC_dep_mod <- lm(bdi_sum_2 ~ Sex*unstable_2_rate + FC_pers_salienceb_pos_2 + Sex:FC_pers_salienceb_pos_2, data = df)
unstable_dorexp_dep_mod <- lm(bdi_sum_2 ~ Sex*unstable_2_rate + exp_dorsalattentionb_pos_2 + Sex:exp_dorsalattentionb_pos_2, data = df)
unstable_dorFC_dep_mod <- lm(bdi_sum_2 ~ Sex*unstable_2_rate + FC_pers_dorsalattentionb_pos_2 + Sex:FC_pers_dorsalattentionb_pos_2, data = df)

# Covariates: Outcome at time 1
threat_salexp_dep_mod1 <- lm(bdi_sum_2 ~ Sex*threatening_2_rate + exp_salienceb_pos_2 + Sex:exp_salienceb_pos_2 + bdi_sum_1, data = df)
threat_salFC_dep_mod1 <- lm(bdi_sum_2 ~ Sex*threatening_2_rate + FC_pers_salienceb_pos_2 + Sex:FC_pers_salienceb_pos_2 + bdi_sum_1, data = df)
threat_dorexp_dep_mod1 <- lm(bdi_sum_2 ~ Sex*threatening_2_rate + exp_dorsalattentionb_pos_2 + Sex:exp_dorsalattentionb_pos_2 + bdi_sum_1, data = df)
threat_dorFC_dep_mod1 <- lm(bdi_sum_2 ~ Sex*threatening_2_rate + FC_pers_dorsalattentionb_pos_2 + Sex:FC_pers_dorsalattentionb_pos_2 + bdi_sum_1, data = df)
unstable_salexp_dep_mod1 <- lm(bdi_sum_2 ~ Sex*unstable_2_rate + exp_salienceb_pos_2 + Sex:exp_salienceb_pos_2 + bdi_sum_1, data = df)
unstable_salFC_dep_mod1 <- lm(bdi_sum_2 ~ Sex*unstable_2_rate + FC_pers_salienceb_pos_2 + Sex:FC_pers_salienceb_pos_2 + bdi_sum_1, data = df)
unstable_dorexp_dep_mod1 <- lm(bdi_sum_2 ~ Sex*unstable_2_rate + exp_dorsalattentionb_pos_2 + Sex:exp_dorsalattentionb_pos_2 + bdi_sum_1, data = df)
unstable_dorFC_dep_mod1 <- lm(bdi_sum_2 ~ Sex*unstable_2_rate + FC_pers_dorsalattentionb_pos_2 + Sex:FC_pers_dorsalattentionb_pos_2 + bdi_sum_1, data = df)

# Covariates: Demographics
threat_salexp_dep_mod2 <- lm(bdi_sum_2 ~ Sex*threatening_2_rate + exp_salienceb_pos_2 + Sex:exp_salienceb_pos_2 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
threat_salFC_dep_mod2 <- lm(bdi_sum_2 ~ Sex*threatening_2_rate + FC_pers_salienceb_pos_2 + Sex:FC_pers_salienceb_pos_2 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
threat_dorexp_dep_mod2 <- lm(bdi_sum_2 ~ Sex*threatening_2_rate + exp_dorsalattentionb_pos_2 + Sex:exp_dorsalattentionb_pos_2 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
threat_dorFC_dep_mod2 <- lm(bdi_sum_2 ~ Sex*threatening_2_rate + FC_pers_dorsalattentionb_pos_2 + Sex:FC_pers_dorsalattentionb_pos_2 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
unstable_salexp_dep_mod2 <- lm(bdi_sum_2 ~ Sex*unstable_2_rate + exp_salienceb_pos_2 + Sex:exp_salienceb_pos_2 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
unstable_salFC_dep_mod2 <- lm(bdi_sum_2 ~ Sex*unstable_2_rate + FC_pers_salienceb_pos_2 + Sex:FC_pers_salienceb_pos_2 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
unstable_dorexp_dep_mod2 <- lm(bdi_sum_2 ~ Sex*unstable_2_rate + exp_dorsalattentionb_pos_2 + Sex:exp_dorsalattentionb_pos_2 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
unstable_dorFC_dep_mod2 <- lm(bdi_sum_2 ~ Sex*unstable_2_rate + FC_pers_dorsalattentionb_pos_2 + Sex:FC_pers_dorsalattentionb_pos_2 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)

# Covariates: Demographics + Outcome at time 1
threat_salexp_dep_mod3 <- lm(bdi_sum_2 ~ Sex*threatening_2_rate + exp_salienceb_pos_2 + Sex:exp_salienceb_pos_2 + bdi_sum_1 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
threat_salFC_dep_mod3 <- lm(bdi_sum_2 ~ Sex*threatening_2_rate + FC_pers_salienceb_pos_2 + Sex:FC_pers_salienceb_pos_2 + bdi_sum_1 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
threat_dorexp_dep_mod3 <- lm(bdi_sum_2 ~ Sex*threatening_2_rate + exp_dorsalattentionb_pos_2 + Sex:exp_dorsalattentionb_pos_2 + bdi_sum_1 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
threat_dorFC_dep_mod3 <- lm(bdi_sum_2 ~ Sex*threatening_2_rate + FC_pers_dorsalattentionb_pos_2 + Sex:FC_pers_dorsalattentionb_pos_2 + bdi_sum_1 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
unstable_salexp_dep_mod3 <- lm(bdi_sum_2 ~ Sex*unstable_2_rate + exp_salienceb_pos_2 + Sex:exp_salienceb_pos_2 + bdi_sum_1 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
unstable_salFC_dep_mod3 <- lm(bdi_sum_2 ~ Sex*unstable_2_rate + FC_pers_salienceb_pos_2 + Sex:FC_pers_salienceb_pos_2 + bdi_sum_1 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
unstable_dorexp_dep_mod3 <- lm(bdi_sum_2 ~ Sex*unstable_2_rate + exp_dorsalattentionb_pos_2 + Sex:exp_dorsalattentionb_pos_2 + bdi_sum_1 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)
unstable_dorFC_dep_mod3 <- lm(bdi_sum_2 ~ Sex*unstable_2_rate + FC_pers_dorsalattentionb_pos_2 + Sex:FC_pers_dorsalattentionb_pos_2 + bdi_sum_1 + Race + Ethnicity + age_mri + Familys_Gross_Total_Income, data = df)

# Tables
tab_model(threat_salexp_dep_mod, threat_salexp_dep_mod1, threat_salexp_dep_mod2, threat_salexp_dep_mod3) #does remain #
tab_model(threat_salFC_dep_mod, threat_salFC_dep_mod1, threat_salFC_dep_mod2, threat_salFC_dep_mod3) #does remain #
tab_model(threat_dorexp_dep_mod, threat_dorexp_dep_mod1, threat_dorexp_dep_mod2, threat_dorexp_dep_mod3) #does remain #
tab_model(threat_dorFC_dep_mod, threat_dorFC_dep_mod1, threat_dorFC_dep_mod2, threat_dorFC_dep_mod3) #does remain
tab_model(unstable_salexp_dep_mod, unstable_salexp_dep_mod1, unstable_salexp_dep_mod2, unstable_salexp_dep_mod3) #was never present
tab_model(unstable_salFC_dep_mod, unstable_salFC_dep_mod1, unstable_salFC_dep_mod2, unstable_salFC_dep_mod3) #was never present
tab_model(unstable_dorexp_dep_mod, unstable_dorexp_dep_mod1, unstable_dorexp_dep_mod2, unstable_dorexp_dep_mod3) #wasn't originally present
tab_model(unstable_dorFC_dep_mod, unstable_dorFC_dep_mod1, unstable_dorFC_dep_mod2, unstable_dorFC_dep_mod3) #was never present
