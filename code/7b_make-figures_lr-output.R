###############################################################################
# SCI - Handling missing data project 
# L. Bourguignon & L.P. Lukas 
# First version : 23.03.2021
# Last update : 14.06.2023
# ------------------------------------------------------------------------------
# VISUALISE OUTPUT OF THE LINEAR REGRESSION AFTER IMPUTATION OF NAS
################################################################################

################################################################################
# Sourcing additional scripts
################################################################################

source("./0_functions.R")

################################################################################
# Argument loading
################################################################################

args = commandArgs(trailingOnly=TRUE)
mask <- args[1]

path = '/cluster/scratch/blucie/NA_SCI/subsets_imputed/'

################################################################################
# Main
################################################################################

if (mask == F){
  df_results_raw <- read.csv(paste0(path, 'lr-output_all-subsets_lower52_version2.csv'))
  df_results_raw$imputation <- factor(df_results_raw$imputation, 
                                      levels = c("baseline", "last_obs", "case_deletion", "mean", 
                                                 "lr", "knn", "SVM_linear", "SVM_rbf", 
                                                 "RF", 'norm.predict', 'pmm', 'rf', 'polr'))
} else if (mask == T){
  df_results_raw <- read.csv(paste0(path, 'lr-output_all-subsets_mask_lower52_version2.csv'))
  df_results_raw$imputation <- factor(df_results_raw$imputation, 
                                      levels = c("baseline_mask", "last_obs_mask", "case_deletion_mask", "mean_mask", 
                                                 "lr_mask", "knn_mask", "SVM_linear_mask", "SVM_rbf_mask", 
                                                 "RF_mask", 'norm.predict_mask', 'pmm_mask', 'rf_mask', 'polr_mask'),
                                      labels = c('baseline', "last_obs", "case_deletion", 'mean', 'lr', 'knn', 'SVM_linear', 'SVM_rbf',
                                                  'RF', 'norm.predict', 'pmm','rf', 'polr'))
}

df_results_raw <- df_results_raw[ , ! names(df_results_raw) %in% c("X")]
variablesNA <- levels(factor(df_results_raw$var_with_NA))
patterns <- levels(factor(df_results_raw$pattern_NA))

df_results <- df_results_raw[complete.cases(df_results_raw$imputation), ]

for (var in variablesNA){
  
  for (pat in patterns){
    name <- paste0(var, "_", pat)
    
    df_temp <- filter(df_results, var_with_NA %in% var, 
                      pattern_NA %in% pat,
                      variables %in% c('AIS A-AIS B', 'AIS A-AIS C', 'AIS A-AIS D', 'lower01'))
    
    #df_temp <- filter(df_temp, 
    #                  simulation %in% c("balanced", "EMSCI", "SygenAll"),
    #                  imputation %in% c('baseline', "last_obs", 'case_deletion', 'mean', 'RF', 'rf'))
    
    # New facet label names for variables variable
    variables.labs <- c('AIS A-AIS B', 'AIS A-AIS C', 'AIS A-AIS D', 'LEMS_01')
    names(variables.labs) <- c('AIS A-AIS B', 'AIS A-AIS C', 'AIS A-AIS D', 'lower01')
    
    df_temp$imputation <- factor(df_temp$imputation, 
                                          levels = c('baseline', 'case_deletion', "last_obs", 'mean', 'RF', 'rf', 'lr', 'knn', 'SVM_linear', 'SVM_rbf',
                                                     'norm.predict', 'pmm', 'polr'),
                                          labels = c('Baseline', 'Case deletion', 
                                                     'LOCF',
                                                     "Mean",
                                                     "Random forest",
                                                     "Random forest (mice)",
                                                     "Linear regression", "k-nearest neighbors", "SVM with a linear kernel", "SVM with a RBF kernel",
                                                     "Linear regression (mice)", "Predictive mean matching (mice)", "Polytomous regression (mice)"))
    dummy <- df_temp %>% 
      select(imputation, pvalues, variables, simulation) %>%
      group_by(imputation, variables, simulation) %>%
      summarise(mean.pvalue = log(mean(pvalues)))
    
    plot_temp <- df_temp %>% 
      full_join(dummy, by = c("imputation", "variables", "simulation")) %>%
      ggplot(aes(x = imputation, y = coef)) +
      geom_violin(aes(x = imputation, y = coef, group = imputation, fill = mean.pvalue)) +
      #geom_half_point(side = 'r', aes(x = imputation, y = coef, group = imputation)) +
      facet_grid(variables ~ simulation, scales = 'free', 
                 labeller=labeller(variables = variables.labs)) +
      labs(title = paste(var, pat), x = "Imputation method", y = "LR-estimated effect size") +
      geom_boxplot(width = 0.1) +
      theme_light() + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
      scale_fill_gradient2(low = "blue", high = "red", midpoint=log(0.05)) +
      theme(strip.text = element_text(size = 16, face='bold'),
            axis.text.x = element_text(size = 12),
            axis.title = element_text(size=16, face='bold')) +
      stat_compare_means(label = "p.signif", 
                         method = "wilcox.test", 
                         paired = T, 
                         ref.group = "Baseline") +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))
    #plot_temp
    
    if (mask == T){
      filename = paste0('./important_output/figures_lr/estimated-effect-sizes-after-imputation_', name, '_n500_mask_lower52.png')
    } else {
      filename = paste0('./important_output/figures_lr/estimated-effect-sizes-after-imputation_', name, '_n500_lower52.png')
    }
    ggsave(filename, plot = plot_temp, width=29.7, height=21.0, unit="cm")
  }
}
