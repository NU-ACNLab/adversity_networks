### This script plots adversity rates by sex
###
### Ellyn Butler
### June 27, 2026

# advence rates
demo_df <- read.csv('~/Northwestern/projects/adversity_networks/data/processed/demographic/demographic_2026-03-31.csv')
adv_df <- read.csv('~/Documents/Northwestern/studies/mwmh/data/processed/advence/advence_2022-10-06.csv')

adv_df <- merge(adv_df, demo_df)
nf <- nrow(adv_df[adv_df$Sex == 'Female', ])
nm <- nrow(adv_df[adv_df$Sex == 'Male', ])

threat_df <- data.frame(Variable=paste0('ETV', 1:7),
                      threat=c('Family Hurt or Killed', 'Friends Hurt or Killed',
                        'Saw Attacked Knife', 'Saw Shot', 'Shoved Kicked Punched',
                        'Attacked Knife', 'Shot At'),
                      Sex=c(rep('Female', 7), rep('Male', 7)),
                      Sum=c(sum(adv_df[adv_df$female == 1, 'etv1_pastyear'])/nf,
                            sum(adv_df[adv_df$female == 1, 'etv2_pastyear'])/nf,
                            sum(adv_df[adv_df$female == 1, 'etv3_pastyear'])/nf,
                            sum(adv_df[adv_df$female == 1, 'etv4_pastyear'])/nf,
                            sum(adv_df[adv_df$female == 1, 'etv5_pastyear'])/nf,
                            sum(adv_df[adv_df$female == 1, 'etv6_pastyear'])/nf,
                            sum(adv_df[adv_df$female == 1, 'etv7_pastyear'])/nf,
                            sum(adv_df[adv_df$female == 0, 'etv1_pastyear'])/nm,
                            sum(adv_df[adv_df$female == 0, 'etv2_pastyear'])/nm,
                            sum(adv_df[adv_df$female == 0, 'etv3_pastyear'])/nm,
                            sum(adv_df[adv_df$female == 0, 'etv4_pastyear'])/nm,
                            sum(adv_df[adv_df$female == 0, 'etv5_pastyear'])/nm,
                            sum(adv_df[adv_df$female == 0, 'etv6_pastyear'])/nm,
                            sum(adv_df[adv_df$female == 0, 'etv7_pastyear'])/nm)
                      )


threat_df$threat <- ordered(pastyear_df$threat, c('Family Hurt or Killed', 'Friends Hurt or Killed',
  'Saw Attacked Knife', 'Saw Shot', 'Attacked Knife', 'Shot At', 'Shoved Kicked Punched'))

threat_plot <- ggplot(pastyear_df, aes(x=advence, y=Sum, fill=advence)) +
  facet_grid(. ~ Sex) + theme_linedraw() + geom_bar(stat='identity', position='dodge') +
  theme(legend.position='none', axis.title.x=element_blank(), axis.title.y=element_text(size=7),
		panel.spacing=unit(.1, 'lines'), axis.text.y=element_text(size=6),
    axis.text.x = element_text(angle=45, hjust=1, size=6)) +
  ylab('Average number of times in the past year') +
  scale_fill_manual(values=c('deepskyblue3', 'steelblue1', 'springgreen3',
  'palegreen1', 'pink1', 'advetred1', 'firebrick2'))

instability_plot <- ggplot(pastyear_df, aes(x=advence, y=Sum, fill=advence)) +
  facet_grid(. ~ Sex) + theme_linedraw() + geom_bar(stat='identity', position='dodge') +
  theme(legend.position='none', axis.title.x=element_blank(), axis.title.y=element_text(size=7),
		panel.spacing=unit(.1, 'lines'), axis.text.y=element_text(size=6),
    axis.text.x = element_text(angle=45, hjust=1, size=6)) +
  ylab('Average number of times in the past year') +
  scale_fill_manual(values=c('deepskyblue3', 'steelblue1', 'springgreen3',
  'palegreen1', 'pink1', 'advetred1', 'firebrick2'))

# Export
jpeg('~/Documents/Northwestern/projects/adversity_networks/plots/pastyear_advence_ses-2.jpg', res=300, units='mm', width=120, height=150)
adversity_plot
dev.off() 