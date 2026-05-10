# Needs to be adapted for this project


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