### This script plots adversity rates by sex
###
### Ellyn Butler
### June 27, 2026

# Load packages
library(ggpubr)
library(ggplot2)

# Load data
comb_df <- read.csv('~/Documents/Northwestern/projects/adversity_networks/data/processed/combined/combined_data_2026-06-01.csv')
comb_df <- comb_df[which(!is.na(comb_df$exp_salienceb_pos_2)),]
adv_df <- read.csv('~/Documents/Northwestern/projects/adversity_networks/data/processed/clinical/adversity_2026-06-27.csv')


adv_df <- merge(adv_df, comb_df)
nf <- nrow(adv_df[adv_df$Sex == 'Female', ])
nm <- nrow(adv_df[adv_df$Sex == 'Male', ])

threat_df <- data.frame(Threat=rep(c('Close other accident/violence', 'Witnessed accident/violence', 'Verbal threats of violence',
                               'Experienced accident/violence', 'Natural disaster/human threat', 'Illness/accident/injury',
                               'Negative/toxic/dangerous'), 2),
                      Sex=c(rep('Female', 7), rep('Male', 7)),
                      Average=c(sum(adv_df[adv_df$Sex == 'Female', 'family_member_rate'])/nf,
                            sum(adv_df[adv_df$Sex == 'Female', 'witness_rate'])/nf,
                            sum(adv_df[adv_df$Sex == 'Female', 'verbal_rate'])/nf,
                            sum(adv_df[adv_df$Sex == 'Female', 'direct_rate'])/nf,
                            sum(adv_df[adv_df$Sex == 'Female', 'mass_rate'])/nf,
                            sum(adv_df[adv_df$Sex == 'Female', 'serious_rate'])/nf,
                            sum(adv_df[adv_df$Sex == 'Female', 'dangerous_rate'])/nf,
                            sum(adv_df[adv_df$Sex == 'Male', 'family_member_rate'])/nm,
                            sum(adv_df[adv_df$Sex == 'Male', 'witness_rate'])/nm,
                            sum(adv_df[adv_df$Sex == 'Male', 'verbal_rate'])/nm,
                            sum(adv_df[adv_df$Sex == 'Male', 'direct_rate'])/nm,
                            sum(adv_df[adv_df$Sex == 'Male', 'mass_rate'])/nm,
                            sum(adv_df[adv_df$Sex == 'Male', 'serious_rate'])/nm,
                            sum(adv_df[adv_df$Sex == 'Male', 'dangerous_rate'])/nm)
                      )


threat_df$Threat <- ordered(threat_df$Threat, c('Witnessed accident/violence', 'Close other accident/violence', 
                      'Experienced accident/violence', 'Verbal threats of violence',
                      'Natural disaster/human threat', 'Illness/accident/injury', 'Negative/toxic/dangerous'))

threat_plot <- ggplot(threat_df, aes(x=Threat, y=Average, fill=Threat)) +
  facet_grid(. ~ Sex) + theme_linedraw() + geom_bar(stat='identity', position='dodge') +
  theme(legend.position='none', axis.title.x=element_blank(), axis.title.y=element_text(size=7),
		panel.spacing=unit(.1, 'lines'), axis.text.y=element_text(size=6),
    axis.text.x = element_text(angle=55, hjust=1, size=6)) +
  ylab('Average rate per year for ~ the past year') +
  scale_fill_manual(values=c('deepskyblue3', 'steelblue1', 'springgreen3',
  'palegreen1', 'pink1', 'violetred1', 'firebrick2'))

unstable_df <- data.frame(Unstable=rep(c('Not enough $ for essentials', 'Disruptive home', 'Home not available',
                              'Property theft/damage', 'Negative conditions disrupt sleep', 'Moved', 
                              'Home broken into', 'Car damaged/stolen'), 2),
                      Sex=c(rep('Female', 8), rep('Male', 8)),
                      Average=c(sum(adv_df[adv_df$Sex == 'Female', 'lack_money_rate'])/nf,
                            sum(adv_df[adv_df$Sex == 'Female', 'disruptive_living_rate'])/nf,
                            sum(adv_df[adv_df$Sex == 'Female', 'unforeseen_housing_rate'])/nf,
                            sum(adv_df[adv_df$Sex == 'Female', 'stolen_rate'])/nf,
                            sum(adv_df[adv_df$Sex == 'Female', 'sleep_disrupted_rate'])/nf,
                            sum(adv_df[adv_df$Sex == 'Female', 'moved_rate'])/nf,
                            sum(adv_df[adv_df$Sex == 'Female', 'home_broken_rate'])/nf,
                            sum(adv_df[adv_df$Sex == 'Female', 'car_broken_rate'])/nf,
                            sum(adv_df[adv_df$Sex == 'Male', 'lack_money_rate'])/nm,
                            sum(adv_df[adv_df$Sex == 'Male', 'disruptive_living_rate'])/nm,
                            sum(adv_df[adv_df$Sex == 'Male', 'unforeseen_housing_rate'])/nm,
                            sum(adv_df[adv_df$Sex == 'Male', 'sleep_disrupted_rate'])/nm,
                            sum(adv_df[adv_df$Sex == 'Male', 'mass_rate'])/nm,
                            sum(adv_df[adv_df$Sex == 'Male', 'moved_rate'])/nm,
                            sum(adv_df[adv_df$Sex == 'Male', 'home_broken_rate'])/nm,
                            sum(adv_df[adv_df$Sex == 'Male', 'car_broken_rate'])/nm)
                      )


unstable_df$Unstable <- ordered(unstable_df$Unstable, c('Disruptive home', 'Negative conditions disrupt sleep',
                                'Home not available', 'Moved', 'Not enough $ for essentials', 'Property theft/damage', 
                                'Home broken into', 'Car damaged/stolen'))

unstable_plot <- ggplot(unstable_df, aes(x=Unstable, y=Average, fill=Unstable)) +
  facet_grid(. ~ Sex) + theme_linedraw() + geom_bar(stat='identity', position='dodge') +
  theme(legend.position='none', axis.title.x=element_blank(), axis.title.y=element_text(size=7),
		panel.spacing=unit(.1, 'lines'), axis.text.y=element_text(size=6),
    axis.text.x = element_text(angle=55, hjust=1, size=6)) +
  ylab('Average rate per year for ~ the past year') +
  scale_fill_manual(values=c('deepskyblue3', 'steelblue1', 'springgreen3',
  'palegreen1', 'pink1', 'violetred1', 'firebrick2', 'red4'))

adversity_plot <- ggarrange(
  threat_plot, unstable_plot, 
  nrow = 2, ncol = 1, #common.legend = TRUE, legend = 'bottom',
  labels = c('A', 'B')
  ) 

# Export
jpeg('~/Documents/Northwestern/projects/adversity_networks/plots/adversity_plot.jpg', res=300, units='mm', width=180, height=180)
adversity_plot
dev.off() 