# (not completed - just needs to be)


subjs_df <- read.csv('~/Documents/Northwestern/projects/adversity_networks/data/processed/neuroimaging/tabulated/prior_subjects_2025-09-19.csv')
df <- merge(subjs_df, df)

### BDI
bdi_fa1 <- fa(df[, paste0('bdi_', 1:21)], n.factors = 1)
bdi_fa2 <- fa(df[, paste0('bdi_', 1:21)], n.factors = 2)
anova(bdi_fa1, bdi_fa2) # > Implies one factor solution... but weird identical?

cormat <- round(cor(df[, paste0('bdi_', 1:21)]), 2)
corplot <- ggcorrplot(cormat, lab = TRUE, lab_size = 2) + ggtitle('BDI') #1 factor looks reasonable

alpha(cormat)
omega(cormat)

eigenvalues1 <- eigen(cormat)$values
eigen_df1 <- data.frame(matrix(NA, nrow=length(eigenvalues1), ncol=2))
names(eigen_df1) <- c('compnum', 'eigen')
eigen_df1$compnum <- 1:21
eigen_df1$eigen <- eigenvalues1

bdi_scree <- ggplot(eigen_df1, aes(x=compnum, y=eigen)) +
    geom_line(stat='identity') + geom_point() +  theme_minimal() +
    xlab('Component Number') + ylab('Eigenvalues of Components') +
    scale_y_continuous(limits=c(0, 10)) + ggtitle('BDI') +
    theme(plot.title = element_text(size=12), axis.title = element_text(size=10),
      axis.text = element_text(size=6)) #really 1 factor