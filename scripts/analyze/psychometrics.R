### This script looks as the dimensionality of the BDI-2
### to see if it is justified to use just a sum score
###
### Ellyn Butler
### June 29, 2026

# Load packages
library(psych) # v2.6.5
library(ggcorrplot) # v0.1.4.1
library(ggplot2) # v4.0.3

# Load data
comb_df <- read.csv('~/Documents/Northwestern/projects/adversity_networks/data/processed/combined/combined_data_2026-06-01.csv')
df <- read.csv('~/Documents/Northwestern/projects/adversity_networks/data/processed/clinical/bdi_2026-06-29.csv')
comb_df <- comb_df[!is.na(comb_df$exp_salienceb_pos_2), ]

df <- merge(df, comb_df)


### BDI
bdi_fa1 <- fa(df[, paste0('bdi_', 1:21, '_2')], nfactors = 1)
bdi_fa2 <- fa(df[, paste0('bdi_', 1:21, '_2')], nfactors = 2)

# Get chi-square and df from each model
chi1 <- bdi_fa1$STATISTIC
df1  <- bdi_fa1$dof

chi2 <- bdi_fa2$STATISTIC
df2  <- bdi_fa2$dof

# Chi-square difference test
chi_diff <- chi1 - chi2
df_diff  <- df1 - df2

p_value <- pchisq(chi_diff, df = df_diff, lower.tail = FALSE) # implies that a two factor model is a better fit

# Plotting
cormat <- round(cor(df[, paste0('bdi_', 1:21, '_2')]), 2)
corplot <- ggcorrplot(cormat, lab = TRUE, lab_size = 2) + ggtitle('BDI') #1 factor looks reasonable

omega_out <- omega(cormat) # alpha = 0.89, omega_h = 0.66
omega_out$omega_h
omega_out$alpha

eigenvalues1 <- eigen(cormat)$values
eigen_df1 <- data.frame(matrix(NA, nrow=length(eigenvalues1), ncol=2))
names(eigen_df1) <- c('compnum', 'eigen')
eigen_df1$compnum <- 1:21
eigen_df1$eigen <- eigenvalues1

bdi_scree <- ggplot(eigen_df1, aes(x=compnum, y=eigen)) +
    geom_line(stat='identity') + geom_point() +  theme_minimal() +
    xlab('Component Number') + ylab('Eigenvalues of Components') +
    scale_y_continuous(limits=c(0, 10)) + ggtitle('BDI') +
    theme(plot.title = element_text(size = 12), axis.title = element_text(size = 10),
      axis.text = element_text(size = 6)) #really 1 factor

    # Successive eigen values: 6.998, 1.994, 1.372, 1.232, 1.058