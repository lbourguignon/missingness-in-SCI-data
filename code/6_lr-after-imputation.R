################################################################################
# SCI - Handling missing data project 
# L. Bourguignon & L.P. Lukas 
# First version : 06.07.2021
# Last update : 14.06.2023
# ------------------------------------------------------------------------------
# RUNNING LR AFTER IMPUTATION
################################################################################

################################################################################
# Sourcing additional scripts
################################################################################

source("./0_functions.R")

################################################################################
# Argument loading
################################################################################

args = commandArgs(trailingOnly=TRUE)
outcome <- args[1]

#simulations <- c('SygenAll', 'SygenCompleteNAanalysis', 'EMSCI', 'balanced')
simulations <- c('SygenCompleteNAanalysis')
variables <- c('lower01', outcome, 'ais1')
#variables <- c('lower01')
patterns = c('MCAR', 'MAR', 'MNAR')

path = '/cluster/scratch/blucie/NA_SCI/subsets_imputed/'

################################################################################
# Main
################################################################################

df_results <- data.frame(matrix(, nrow=0, ncol=0))
df_results_mask <- df_results

for (sim in simulations){
  #for (i in c(1:2)){
  for (i in c(1:500)){
    #print(i)
    df_imputed_all <- read.csv(paste0(path, sim,'/', outcome, '/subset_', i, '_imputed_mice_n500.csv'))
    df_imputed_all[df_imputed_all == ""] <- NA
    
    for (var in variables){
      for (pat in patterns){
        
        imputations = c('case_deletion', 'lr', 'knn', 'RF', 'SVM_linear', 'SVM_rbf', 'mean', 'baseline')
        if (var == outcome){
          imputations = append(imputations, c('norm.predict', 'pmm', 'rf', 'last_obs'))
        } else if (var == 'lower01'){
          imputations = append(imputations, c('norm.predict', 'pmm', 'rf'))
        } else if (var == 'ais1') {
          imputations = append(imputations, 'polr')
        }
        
        for (imp in imputations){
          print(paste(i, var, pat, imp))
          #print(names(df_imputed_all))
          if (imp == 'case_deletion'){
            data_temp <- prepare_df_lm(df_imputed_all, var, pat, variables)
            print(names(data_temp))
            extracted_summary <- run_lm(data_temp, var, pat, variables)
            extracted_summary_mask <- extracted_summary
          } else if (imp == 'baseline') {
            lm_baseline <- summary(lm(formula = paste0(outcome, " ~ lower01 + level + ais1 + age + sexcd"), data = df_imputed_all))
            extracted_summary <- extract_summary(lm_baseline, imp)
            extracted_summary_mask <- extracted_summary
          } else {
            data_temp <- prepare_df_lm(df_imputed_all, var, pat, variables, imp)
            extracted_summary <- run_lm(data_temp, var, pat, variables, imp)
            extracted_summary_mask <- run_lm_mask(data_temp, var, pat, variables, imp, df_imputed_all)
          }
          
          extracted_summary <- reshape_summary(extracted_summary, sim, i, var, pat, imp, mask = F)
          extracted_summary_mask <- reshape_summary(extracted_summary_mask, sim, i, var, pat, imp, mask = T) 
          df_results <- rbind(df_results, extracted_summary)
          df_results_mask <- rbind(df_results_mask, extracted_summary_mask)
          
        }
      }
    }
  }
}

save_output(df_results, paste0('lr-output_all-subsets_', outcome))
save_output(df_results_mask, paste0('lr-output_all-subsets_mask_', outcome))
