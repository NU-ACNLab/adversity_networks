### This code focuses on session 2 for the analyses
###
### Ellyn Butler
### March 26, 2026 - March 31, 2026

set.seed(2000)

library(reshape2)
library(MASS)
library(dplyr)
library(sjPlot)
library(ggplot2)
library(ggpubr)
library(sjmisc)
library(sjlabelled)

#library(lmerTest) #confint

df <- read.csv('~/Documents/Northwestern/projects/adversity_networks/data/processed/combined/combined_data_2026-03-31.csv')

dim(df) # N = 246
range(df$age_mri) # 14.23562 - 19.41370
mean(df$threatening_2 > 0) # 47.56%
mean(df[df$Sex == 'Female', 'threatening_2']) # 1.79
mean(df[df$Sex == 'Male', 'threatening_2']) # 0.90
rate_mod <- lm(threatening_2 ~ Sex, data = df)
t.test(threatening_2 ~ Sex, data = df) #t = 2.8653, df = 225.5, p-value = 0.00456
summary(rate_mod)

mean(df$unstable_2 > 0) # 29.67%
mean(df[df$Sex == 'Female', 'unstable_2']) # 1.19
mean(df[df$Sex == 'Male', 'unstable_2']) # 1.38
rate_mod <- lm(unstable_2 ~ Sex, data = df)
t.test(unstable_2 ~ Sex, data = df) #t = -0.30127, df = 144.13, p-value = 0.7636
summary(rate_mod)

################### Plot... later
# Violence rates
viol_df <- read.csv('~/Documents/Northwestern/studies/mwmh/data/processed/violence/violence_2022-10-06.csv')

viol_df <- merge(viol_df, df)
nf <- nrow(viol_df[viol_df$female == 1, ])
nm <- nrow(viol_df[viol_df$female == 0, ])

pastyear_df <- data.frame(Variable=paste0('ETV', 1:7),
                      Violence=c('Family Hurt or Killed', 'Friends Hurt or Killed',
                        'Saw Attacked Knife', 'Saw Shot', 'Shoved Kicked Punched',
                        'Attacked Knife', 'Shot At'),
                      Sex=c(rep('Female', 7), rep('Male', 7)),
                      Sum=c(sum(viol_df[viol_df$female == 1, 'etv1_pastyear'])/nf,
                            sum(viol_df[viol_df$female == 1, 'etv2_pastyear'])/nf,
                            sum(viol_df[viol_df$female == 1, 'etv3_pastyear'])/nf,
                            sum(viol_df[viol_df$female == 1, 'etv4_pastyear'])/nf,
                            sum(viol_df[viol_df$female == 1, 'etv5_pastyear'])/nf,
                            sum(viol_df[viol_df$female == 1, 'etv6_pastyear'])/nf,
                            sum(viol_df[viol_df$female == 1, 'etv7_pastyear'])/nf,
                            sum(viol_df[viol_df$female == 0, 'etv1_pastyear'])/nm,
                            sum(viol_df[viol_df$female == 0, 'etv2_pastyear'])/nm,
                            sum(viol_df[viol_df$female == 0, 'etv3_pastyear'])/nm,
                            sum(viol_df[viol_df$female == 0, 'etv4_pastyear'])/nm,
                            sum(viol_df[viol_df$female == 0, 'etv5_pastyear'])/nm,
                            sum(viol_df[viol_df$female == 0, 'etv6_pastyear'])/nm,
                            sum(viol_df[viol_df$female == 0, 'etv7_pastyear'])/nm)
                      )


pastyear_df$Violence <- ordered(pastyear_df$Violence, c('Family Hurt or Killed', 'Friends Hurt or Killed',
  'Saw Attacked Knife', 'Saw Shot',
  'Attacked Knife', 'Shot At', 'Shoved Kicked Punched'))

prop_pastyear_plot <- ggplot(pastyear_df, aes(x=Violence, y=Sum, fill=Violence)) +
  facet_grid(. ~ Sex) + theme_linedraw() + geom_bar(stat='identity', position='dodge') +
  theme(legend.position='none', axis.title.x=element_blank(), axis.title.y=element_text(size=7),
		panel.spacing=unit(.1, 'lines'), axis.text.y=element_text(size=6),
    axis.text.x = element_text(angle=45, hjust=1, size=6)) +
  ylab('Average number of times in the past year') +
  scale_fill_manual(values=c('deepskyblue3', 'steelblue1', 'springgreen3',
  'palegreen1', 'pink1', 'violetred1', 'firebrick2'))

# Export
jpeg('~/Documents/Northwestern/projects/violence_sex_development/plots/pastyear_violence_ses-2.jpg', res=300, units='mm', width=120, height=75)
prop_pastyear_plot
dev.off() 

# Sex effects
viol_df$Sex <- recode(viol_df$female, `1`='Female', `0`='Male')
viol_df$Sex <- ordered(viol_df$Sex, c('Male', 'Female'))

viol_plot <- ggplot(viol_df, aes(threatening_2, depression, color=Sex)) +
      geom_smooth(method = 'lm') +
      geom_point(alpha = 0.3, ) + theme_linedraw() +
      ylab('Depression') +
      xlab('Number of Violent Events in the Past Year') +
      scale_color_manual(values=c('#334FFF', '#FF333F'))

exp_plot <- ggplot(viol_df, aes(exp_b_pos, depression, color=Sex)) +
      geom_smooth(method = 'lm') +
      geom_point(alpha = 0.3) + theme_linedraw() +
      ylab('Depression') +
      xlab('Salience Network Expansion') +
      scale_color_manual(values=c('#334FFF', '#FF333F'))

fc_plot <- ggplot(viol_df, aes(FC_b_pos, depression, color=Sex)) +
      geom_smooth(method = 'lm') +
      geom_point(alpha = 0.3) + theme_linedraw() +
      ylab('Depression') +
      xlab('Salience Network Connectivity') +
      scale_color_manual(values=c('#334FFF', '#FF333F'))

combined <- ggarrange(
  viol_plot, exp_plot, fc_plot, 
  nrow = 1, ncol = 3, common.legend = TRUE, legend = 'bottom',
  labels = c('A', 'B', 'C')
  ) 

# Export
base_dir <- '~/Documents/Northwestern/projects/violence_sex_development/plots/'
png(paste0(base_dir, 'interaction_plots.png'), width=3000, height=1100, res=300)
combined 
dev.off()

################### Model
d1 <- dall[dall$sesid == 1, ]
d1 <- d1[, c('subid', 'BMIperc', 'PubCat')]
names(d1) <- c('subid', 'BMIperc_1', 'PubCat_1')
df <- merge(df, d1)
vars <- c('depression',  'threatening_2', 'exp_b_pos', 'FC_b_pos', 'BMIperc_1', 'PubCat_1', 'age_mri', 'IPR')
df[, vars] <- scale(df[, vars])

df$BMIperc_2 <- df$BMIperc
df$PubCat_2 <- df$PubCat
df$BMIperc <- df$BMIperc_1
df$PubCat <- df$PubCat_1

# use these for poster
boo1 <- lm(depression ~ threatening_2*female, d=df) #sig: female, and femaleXthreatening_2 ... R^2 same across scaled and unscaled, corrs all 1
cor.test(df[df$female == 0, 'depression'], df[df$female == 0,  'threatening_2'])
cor.test(df[df$female == 1, 'depression'], df[df$female == 1,  'threatening_2'])
boo2 <- lm(exp_b_pos ~ threatening_2*female, d=df) #sig: none
boo2.2 <- lm(FC_b_pos ~ threatening_2*female, d=df) #sig: female
boo3 <- lm(depression ~ exp_b_pos*female, d=df) #sig: exp_b_pos, female
cor.test(df[df$female == 0, 'depression'], df[df$female == 0, 'exp_b_pos'])
cor.test(df[df$female == 1, 'depression'], df[df$female == 1, 'exp_b_pos'])
boo3.2 <- lm(depression ~ FC_b_pos*female, d=df) #sig: FC_b_pos, female, FC_b_posXfemale
cor.test(df[df$female == 0, 'depression'], df[df$female == 0, 'FC_b_pos'])
cor.test(df[df$female == 1, 'depression'], df[df$female == 1, 'FC_b_pos'])
tab_model(boo1, boo2, boo2.2, show.ci = FALSE)
tab_model(boo3, boo3.2, show.ci = FALSE)

# sensitivity
goo1 <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + threatening_2*female, d=df) #sig: white, hispanic, female, and femaleXthreatening_2
goo2 <- lm(exp_b_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + threatening_2*female, d=df) #sig: black
goo2.2 <- lm(FC_b_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + threatening_2*female, d=df) #sig: black, female
goo3 <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_b_pos*female, d=df) #sig: hispanic
goo3.2 <- lm(depression ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_b_pos*female, d=df) #sig: hispanic, female

##### Control for T1

# Impute brain values for subjects that have T2 but not T1 data
d1b <- dall[dall$sesid == 1 & dall$subid %in% df$subid, ]
d1b <- d1b[, !(names(d1b) %in% c('sesid', 'otherrace'))]
dfb <- df
dfb <- dfb[, !(names(dfb) %in% c('sesid', 'otherrace'))]
same <- c('subid', 'black', 'white', 'hispanic', 'female', 'IPR')
names(d1b)[!(names(d1b) %in% same)] <- paste0(names(d1b)[!(names(d1b) %in% same)], '_1')
names(dfb)[!(names(dfb) %in% same)] <- paste0(names(dfb)[!(names(dfb) %in% same)], '_2')


# How many missing values for each variable?
# Three subjects did not complete the MRI visit; 5 did not indicate how many
# times they had been exposed to violence in the past year; and 14 did not 
# have usable resting state data (look more into this)
sum(is.na(d1b$age_mri_1)) #3
sum(is.na(d1b$threatening_2_1)) #5
sum(is.na(d1b$depression_1)) #0
sum(is.na(d1b$exp_b_pos_1)) #17
sum(is.na(d1b$FC_b_pos_1)) #17
d1b[is.na(d1b$exp_b_pos_1) & !is.na(d1b$age_mri_1), 'subid']
#"MWMH169" - missing resting state data, but has task data
#"MWMH178" - missing resting state data, but has task data
#"MWMH215" - missing resting state data, but has task data
#"MWMH233" - missing resting state data, but has task data
#"MWMH235" - missing resting state data, but has task data
#"MWMH242" - missing all functional data
#"MWMH247" - missing resting state data and faces task, but has avoid task
#"MWMH253" - missing resting state data and faces task, but has avoid task
#"MWMH267" - missing resting state data, but has task data
#"MWMH271" - missing resting state data, but has task data
#"MWMH278" - missing resting state data, but has task data (missing tsvs)
#"MWMH279" - missing all functional data
#"MWMH299" - missing resting state data, but has task data
#"MWMH358" - has resting state, but only 31 TRs, so excluded from processing

# Assume age_mri_1 would have been two years before age_mri_2
subids <- d1b[is.na(d1b$age_mri_1), 'subid']
d1b[is.na(d1b$age_mri_1), 'age_mri_1'] <- dfb[dfb$subid %in% subids, 'age_mri_2'] - 2

# Predict exp_b_pos_1
t.test(exp_b_pos_1 ~ black, data = d1b) #sig... lower p-val than white
t.test(exp_b_pos_1 ~ white, data = d1b) #sig
t.test(exp_b_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$exp_b_pos_1), 'exp_b_pos_1'], d1b[!is.na(d1b$exp_b_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$exp_b_pos_1), 'exp_b_pos_1'], d1b[!is.na(d1b$exp_b_pos_1), 'PubCat_1']) #sig
cor.test(d1b[!is.na(d1b$exp_b_pos_1), 'exp_b_pos_1'], d1b[!is.na(d1b$exp_b_pos_1), 'IPR']) #sig
t.test(exp_b_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$exp_b_pos_1), 'exp_b_pos_1'], d1b[!is.na(d1b$exp_b_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$exp_b_pos_1), 'exp_b_pos_1'], d1b[!is.na(d1b$exp_b_pos_1), 'depression_1']) #not
exp_mod <- lm(exp_b_pos_1 ~ black + IPR + PubCat_1 + female, data = d1b) #PubCat_1 not over female, IPR not conditionally sig
exp_mod <- lm(exp_b_pos_1 ~ black + female, data = d1b) 

d1b[is.na(d1b$exp_b_pos_1), 'exp_b_pos_1'] <- predict(exp_mod, d1b[is.na(d1b$exp_b_pos_1), ])

# Predict FC_b_pos_1
t.test(FC_b_pos_1 ~ black, data = d1b) #not
t.test(FC_b_pos_1 ~ white, data = d1b) #not
t.test(FC_b_pos_1 ~ hispanic, data = d1b) #not
cor.test(d1b[!is.na(d1b$FC_b_pos_1), 'FC_b_pos_1'], d1b[!is.na(d1b$FC_b_pos_1), 'BMIperc_1']) #not
cor.test(d1b[!is.na(d1b$FC_b_pos_1), 'FC_b_pos_1'], d1b[!is.na(d1b$FC_b_pos_1), 'PubCat_1']) #not
cor.test(d1b[!is.na(d1b$FC_b_pos_1), 'FC_b_pos_1'], d1b[!is.na(d1b$FC_b_pos_1), 'IPR']) #not
t.test(FC_b_pos_1 ~ female, data = d1b) #sig
cor.test(d1b[!is.na(d1b$FC_b_pos_1), 'FC_b_pos_1'], d1b[!is.na(d1b$FC_b_pos_1), 'age_lab_1']) #not
cor.test(d1b[!is.na(d1b$FC_b_pos_1), 'FC_b_pos_1'], d1b[!is.na(d1b$FC_b_pos_1), 'depression_1']) #not
fc_mod <- lm(FC_b_pos_1 ~ female, data = d1b)
d1b[is.na(d1b$FC_b_pos_1), 'FC_b_pos_1'] <- predict(fc_mod, d1b[is.na(d1b$FC_b_pos_1), ])

d1b <- d1b[, !(names(d1b) %in% c('IPR', 'black', 'white', 'hispanic', 'female'))]
df <- merge(d1b, dfb) #keep the subjects that have complete session 2 data

# Controlling for T1 data
vars <- c('depression_2', 'threatening_2', 'age_mri_2', 'depression_1', 'BMIperc_2', 'PubCat_2', 'exp_b_pos_1', 'FC_b_pos_1')
df[, vars] <- scale(df[, vars])
df$threatening_2 <- df$threatening_2_2
df$exp_b_pos <- df$exp_b_pos_2
df$FC_b_pos <- df$FC_b_pos_2
foo1 <- lm(depression_2 ~ threatening_2*female + depression_1, d=df) #sig: threatening_2_2, female, depression_1, threatening_2_2:female
foo2 <- lm(exp_b_pos ~ threatening_2*female + exp_b_pos_1, d=df) #sig: exp_b_pos_1
foo2.2 <- lm(FC_b_pos ~ threatening_2*female + FC_b_pos_1, d=df) #sig: female, FC_b_pos_1
foo3 <- lm(depression_2 ~ exp_b_pos*female + depression_1, d=df) #sig: exp_b_pos_2 (interesting!), depression_1
foo3.2 <- lm(depression_2 ~ FC_b_pos*female + depression_1, d=df) #sig: FC_b_pos_2 (interesting!), female, depression_1, FC_b_pos_2:female (interesting!)

# sensitivity... 11/11/2024: Considering moving to baseline BMIperc and PubCat... make clear which in paper
df$age_mri <- df$age_mri_2
df$BMIperc <- df$BMIperc_1
df$PubCat <- df$PubCat_1
noo1 <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + threatening_2*female + depression_1, d=df) #sig: threatening_2_2, female, threatening_2_2:female, white, hispanic, BMIperc, IPR 
noo2 <- lm(exp_b_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + threatening_2*female + exp_b_pos_1, d=df) #sig: 
noo2.2 <- lm(FC_b_pos ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + threatening_2*female + FC_b_pos_1, d=df) #sig: 
noo3 <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + exp_b_pos*female + depression_1, d=df) #sig: 
noo3.2 <- lm(depression_2 ~ age_mri + black + white + hispanic + BMIperc + PubCat + IPR + FC_b_pos*female + depression_1, d=df) #sig: 


# Tables
# 1
tab_model(boo1, foo1, goo1, noo1)
tab_model(boo2, foo2, goo2, noo2)
tab_model(boo2.2, foo2.2, goo2.2, noo2.2)
tab_model(boo3, foo3, goo3, noo3)
tab_model(boo3.2, foo3.2, goo3.2, noo3.2)