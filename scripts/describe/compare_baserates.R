### This script compares the base rates for 
### threatening and unstable events for the
### original and modified definitions.
###
### Ellyn Butler
### April 6, 2026 - May 10, 2026

df <- read.csv('/projects/b1108/projects/adversity_networks/data/processed/clinical/clinical_2026-05-10.csv')
final_subjs_df <- read.csv('/projects/b1108/projects/adversity_networks/data/processed/neuroimaging/tabulated/prior_subjects_2026-03-23.csv')
df <- merge(df, final_subjs_df)

# % of sample with old definition experienced threatening between T1 and T3
(sum(df$threatening_original_2 > 0)/nrow(df))*100 #21.13821

# % of sample with new definition experienced threatening between T1 and T3
(sum(df$threatening_2 > 0)/nrow(df))*100 #39.43089

# % of sample with old definition experienced unstable between T1 and T3
(sum(df$unstable_original_2 > 0)/nrow(df))*100 #11.78862

# % of sample with new definition experienced unstable between T1 and T3
(sum(df$unstable_2 > 0)/nrow(df))*100 #29.6748