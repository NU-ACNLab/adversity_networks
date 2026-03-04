### This script creates a graph illustrating how 
### conditioning on earlier depression may change
### association between DAN features and current
### depression
###
### Ellyn Butler
### June 2, 2025

set.seed(2025)

library(ggplot2)
library(dplyr)

df <- data.frame(dep1 = rep(NA, 300),
                 dep2 = rep(NA, 300),
                 dan = rep(NA, 300))

df$dep1 <- rnorm(300, 0, 1)
df$dep2 <- df$dep1 + rnorm(300, 0, 1.8)
df$dep2 <- (df$dep2 - mean(df$dep2))/sd(df$dep2)

df$dan <- -df$dep2 + + rnorm(300, 0, 2)
df$dan <- (df$dan - mean(df$dan))/sd(df$dan)

df$high <- df$dep1 > 0

df[df$high == TRUE, 'dan'] <- -df[df$high == TRUE, 'dan'] - 1.4
df[df$high == FALSE, 'dan'] <- -df[df$high == FALSE, 'dan'] + 1.4

df[df$high == TRUE, 'dep2'] <- df[df$high == TRUE, 'dep2'] + rnorm(length(df[df$high == TRUE, 'dep2']), .6, .4)

df[df$high == FALSE, 'dep2'] <- df[df$high == FALSE, 'dep2'] + rnorm(length(df[df$high == FALSE, 'dep2']), -.6, .4)

df$high <- ifelse(df$high, 'High', 'Low')

cond_plot <- ggplot(df, aes(x = dan, y = dep2, color = high)) +
    theme_linedraw() + geom_point() + 
    geom_smooth(method = 'lm') + xlab('DAN Connectivity (T2)') +
    ylab('Depression (T2)') + labs(color = 'Depression (T1)') +
    theme(legend.position = 'bottom')

jpeg('~/Documents/Northwestern/projects/adversity_networks/plots/simulation_conditioning.jpg', res=400, units='mm', width=100, height=100)
cond_plot
dev.off()