################################################################################
# SCI - Handling missing data project 
# L. Bourguignon & L.P. Lukas 
# First version : 23.03.2021
# Last update : 14.06.2023
# ------------------------------------------------------------------------------
# VISUALISE METRICS COMPARING TRUE AND IMPUTED VALUES
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

if (outcome == 'lower52'){
  variables <- c('ais1', 'lower01', 'lower52')
} else if (outcome == 'lower26'){
  variables <- c('lower26')
}

simulations <- c('SygenAll', 'SygenCompleteNAanalysis', 'EMSCI', 'balanced')
patterns <- c('MCAR', 'MAR', 'MNAR')

################################################################################
# Main
################################################################################

for (sim in simulations){
  for (var in variables){
    
    if (var == 'ais1'){
      metrics <- c('MCC', 'kappa')
    } else {
      metrics <- c('MAE', 'RMSE', 'R2')
    }
    
    if (outcome == 'lower52' && var != 'lower52'){
       df_metrics <- read.csv(paste0('/cluster/scratch/blucie/NA_SCI/metrics/', 
                                  paste0(sim, '_', var),
                                  '_metrics_withmice_n500_', outcome, '.csv'))
    }
    if (outcome == 'lower26' || var == 'lower52'){
       df_metrics <- read.csv(paste0('/cluster/scratch/blucie/NA_SCI/metrics/',
                                    paste0(sim, '_', var),
                                    '_metrics_withmice_n500.csv'))
    }
 
    df_metrics$model_imputation <- factor(df_metrics$model_imputation, 
                                          levels = c('mean', 'last_obs', "knn", "lr", "SVM_linear", "SVM_rbf", 'RF',
                                                     'polr', 'rf', 'pmm', 'norm.predict'),
                                          labels = c('Mean', 'LOCF', "k-NN", "Linear regression",
                                                     "SVM with\nlinear kernel", "SVM with\nRBF kernel",
                                                     "Random forest", 'Proportional odds\nlogistic regression (mice)',
                                                     'Random forest\n(mice)', 'predictive mean\nmatching (mice)',
                                                      'Norm predict\n(mice)'))
    
    for (pat in patterns){
      for (met in metrics){
        name <- paste0(pat, '_', met)
        df_metrics_sub <- subset(df_metrics, pattern == pat & metric == met)
        plot <- df_metrics_sub %>% 
          group_by(model_imputation) %>%
          mutate(mean = mean(as.numeric(value))) %>%
          mutate(meanMsd = mean(value)-sd(value)) %>%
          mutate(meanPsd = mean(value)+sd(value)) %>%
          ggplot(aes(x=value)) +
          facet_grid(cols = vars(model_imputation), rows=vars(metric)) + 
          theme_bw() +
          xlab(NULL) +
          geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 40) +
          geom_density(alpha=.05, fill="#FF6666") +
          geom_vline(aes(xintercept=mean), colour='red') +
          geom_vline(aes(xintercept=meanMsd), colour='red', linetype='dashed') +
          geom_vline(aes(xintercept=meanPsd), colour='red', linetype='dashed')
        
        if (met == 'R2'){
          plot <- plot + xlim(-1, 1)
        }
        
        ggsave(paste0('/cluster/home/blucie/SCI/missing_data/code/important_output/figures_metrics/', sim, '/', var, '/', name, '.png'), plot = plot, width = 25, height = 4, units = "cm")
        
        plot_rank <- df_metrics_sub %>% 
          ggplot(aes(x=rank)) +
          facet_grid(cols = vars(model_imputation), rows=vars(metric)) +
          theme_bw() +
          xlab(NULL) +
          geom_histogram(colour="black", fill="white", binwidth=1)
        
        if (met == 'R2'){
          plot_rank <- plot_rank + scale_x_reverse(lim = c(max(df_metrics_sub$rank) + 0.5, 0.5),
                            breaks = as.numeric(levels(factor(df_metrics_sub$rank))),
                            labels = as.numeric(rev(levels(factor(df_metrics_sub$rank)))))
        } 

        ggsave(paste0('/cluster/home/blucie/SCI/missing_data/code/important_output/figures_metrics/', sim, '/', var, '/', name, '_rank.png'), plot = plot_rank, width = 25, height = 4, units = 'cm')
      }
    }
  }
}
