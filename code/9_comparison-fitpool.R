###############################################################################
# SCI - Handling missing data project 
# L. Bourguignon & L.P. Lukas 
# First version : 23.03.2023
# Last update : 14.06.2023
# ------------------------------------------------------------------------------
# VISUALISE DISTRIBUTIONS AFTER IMPUTATION
################################################################################

################################################################################
# Functions
################################################################################


reshape_summary <- function(extracted_summary, sim, i, var, pat, imp, mask){
  extracted_summary$simulation <- c(rep(sim, dim(extracted_summary)[1]))
  extracted_summary$subset_nb <- c(rep(i, dim(extracted_summary)[1]))
  extracted_summary$var_with_NA <- c(rep(var, dim(extracted_summary)[1]))
  extracted_summary$pattern_NA <- c(rep(pat, dim(extracted_summary)[1]))
  if (mask == F){
    extracted_summary$imputation <- c(rep(imp, dim(extracted_summary)[1]))
  } else if (mask == T){
    extracted_summary$imputation <- c(rep(paste0(imp, '_mask'), dim(extracted_summary)[1]))
  }
  return(extracted_summary)
}

make_df <- function(test_CIplot, variable_interest, pattern){
  test_CIplot_sub <- dplyr::filter(test_CIplot,
                                   var_with_NA %in% c(variable_with_NA),
                                   simulation %in% c(sim),
                                   variables %in% c(variable_interest),
                                   pattern_NA == pattern)
  
  mean_raw_bias <- c()
  lower_raw_bias <- c()
  upper_raw_bias <- c()
  percentage_bias <- c()
  coverage_rate <- c()
  average_width <- c()
  RMSE_vec <- c()
  
  for (imp in imputations){
    controls <- dplyr::filter(test_CIplot_sub,
                              order %in% c("poolfit"),
                              imputation %in% imp)[['coef']]
    #print(controls)
    cases <- dplyr::filter(test_CIplot_sub,
                           order %in% c("fitpool"),
                           imputation %in% imp)[['coef']]
    #print(cases)
    diff <- cases - controls
    mean_raw_bias <- append(mean_raw_bias, mean(diff))
    lower_raw_bias <- append(lower_raw_bias, quantile(diff, probs=c(0.025)))
    upper_raw_bias <- append(upper_raw_bias, quantile(diff, probs=c(0.975)))
    percentage_bias <- append(percentage_bias, mean(100*abs(diff/controls)))
    coverage_rate <- append(coverage_rate, mean(quantile(diff, probs=c(0.025)) < controls & controls < quantile(diff, probs=c(0.975))))
    average_width <- append(average_width, mean(quantile(diff, probs=c(0.975)) - quantile(diff, probs=c(0.025))))
    RMSE_vec <- append(RMSE_vec, mean(sqrt(diff^2)))
  }
  
  estimates <- data.frame(imputation = imputations,
                          RB = mean_raw_bias,
                          lower = lower_raw_bias,
                          upper = upper_raw_bias,
                          PB = percentage_bias,
                          CR = coverage_rate,
                          CW = average_width,
                          RMSE_beta = RMSE_vec
  )
  
  estimates$problem = estimates$lower > 0 | estimates$upper < 0
  estimates$imputation <- factor(estimates$imputation, 
                                 levels = c('norm.predict', 'pmm', 'rf', 'polr', 'RF', 'SVM_rbf', 'SVM_linear', 
                                            'lr', 'knn', 'mean', 'case_deletion', 'last_obs'))
  levels(estimates$imputation) <- list(`Norm predict (mice)` = 'norm.predict', 
                                       `Predictive mean matching (mice)` = 'pmm', 
                                       `Random forest (mice)` = 'rf', 
                                       `Polytomous regression (mice)` = 'polr',
                                       `Random forest` = 'RF', 
                                       `SVM with RBF kernel` = 'SVM_rbf', 
                                       `SVM with linear kernel` = 'SVM_linear', 
                                       `Linear regression` = 'lr', 
                                       `k-NN` = 'knn', 
                                       `Mean` = 'mean', 
                                       `Complete case analysis` = 'case_deletion',
                                       `Last observation carried forward` ='last_obs')

  return(estimates)
}


################################################################################
# Declare variables
################################################################################

outcome <- 'lower52'
simulations <- c('SygenCompleteNAanalysis', 'balanced')
variables <- c('ais1', 'lower01', outcome)
patterns = c('MCAR', 'MAR', 'MNAR')
variable_with_NA <- 'lower01'
problemColors <- c("TRUE"="red", "FALSE"="darkgrey")
colorScale <- scale_colour_manual(name="problem", values=problemColors)
sim <- 'SygenCompleteNAanalysis'
imputations <- c('pmm', 'rf', 'norm.predict')

################################################################################
# Prepare datafrmae with fitpool results
################################################################################

df_results_fitpool <- data.frame(matrix(, nrow=0, ncol=0))

for (sim in simulations){
 path = paste0('/cluster/scratch/blucie/NA_SCI/fitpool_results/', sim, '/', outcome, '/')
 for (i in c(1:500)){
   for (pat in patterns){
     for (var in variables){
       
       if (var == 'ais1'){
         imputations <- c('polr')
       } else {
         imputations <- c('pmm', 'norm.predict', 'rf')
       }
       for (imp in imputations){
         temp_df = read.csv(paste0(path, 'subset_', i, '_imputed_mice_n500_', imp, '_', var, '_', pat, '.csv'))
         
         temp_df$term[temp_df$term == 'levelthoracic'] <- 'cervical-thoracic'
         temp_df$term[temp_df$term == 'ais1AIS B'] <- 'AIS A-AIS B'
         temp_df$term[temp_df$term == 'ais1AIS C'] <- 'AIS A-AIS C'
         temp_df$term[temp_df$term == 'ais1AIS D'] <- 'AIS A-AIS D'
         
         temp_df_sub <- temp_df %>% select(term, estimate, p.value)
         temp_df_sub <- temp_df_sub %>% 
           rename(
             variables = term,
             coef = estimate,
             pvalues = p.value
           )
         
         temp_sum <- reshape_summary(temp_df_sub, sim, i, var, pat, imp, mask=F)
         
         df_results_fitpool <- rbind(df_results_fitpool, temp_sum)
         
       }
     }
   }
 }
}

df_results_fitpool$order <- 'fitpool'

################################################################################
# Combine fitpool and poolfit results
################################################################################

df_results_poolfit <- read.csv('/cluster/scratch/blucie/NA_SCI/subsets_imputed/lr-output_all-subsets_lower52_version2.csv')

df_results_poolfit$order <- 'poolfit'

df_results_poolfit <- subset(df_results_poolfit, select = -c(model))

df_all <- rbind(df_results_poolfit, df_results_fitpool)

#write.csv(df_all, '/cluster/home/blucie/SCI/missing_data/code/important_output/df_CI_plots/df_comparison_order.csv')

#df_all <- read.csv('/cluster/home/blucie/SCI/missing_data/code/important_output/df_CI_plots/df_comparison_order.csv')

df_all$variables <- str_remove(df_all$variables, "_MCAR")
df_all$variables <- str_remove(df_all$variables, "_MAR")
df_all$variables <- str_remove(df_all$variables, "_MNAR")
df_all$variables <- str_remove(df_all$variables, "_polr")
df_all$variables <- str_replace(df_all$variables, "ais1", 'AIS A-')
df_all$variables <- str_replace(df_all$variables, "level", 'cervical-')
df_all$variables <- str_replace(df_all$variables, "sexcd", 'male-female')

################################################################################
# Plotting
################################################################################

estimates_lower01_MCAR <- make_df(df_all, 'lower01', 'MCAR')
plot_estimates_lower01_MCAR <- make_plot(estimates_lower01_MCAR, 'lower01', c(-1,1.1)) +
  ylab('MCAR') +
  ggtitle('LEMS at baseline') +
  theme(axis.title.y = element_text(size=12, face="bold"),
        plot.title = element_text(size=12, face="bold"))

estimates_AISB_MCAR <- make_df(df_all, 'AIS A-AIS B', 'MCAR')
plot_estimates_AISB_MCAR <- make_plot(estimates_AISB_MCAR, 'AIS A-AIS B', c(-1,1.1)) +
  ggtitle('AIS A - AIS B') +
  theme(axis.title.y = element_text(size=12, face="bold"),
        plot.title = element_text(size=12, face="bold"))

estimates_AISC_MCAR <- make_df(df_all, 'AIS A-AIS C', 'MCAR')
plot_estimates_AISC_MCAR <- make_plot(estimates_AISC_MCAR, 'AIS A-AIS C', c(-1,10))+
  ggtitle('AIS A - AIS C') +
  theme(axis.title.y = element_text(size=12, face="bold"),
        plot.title = element_text(size=12, face="bold"))

estimates_AISD_MCAR <- make_df(df_all, 'AIS A-AIS D', 'MCAR')
plot_estimates_AISD_MCAR <- make_plot(estimates_AISD_MCAR, 'AIS A-AIS D', c(-1,26))+
  ggtitle('AIS A - AIS D') +
  theme(axis.title.y = element_text(size=12, face="bold"),
        plot.title = element_text(size=12, face="bold"))

estimates_lower01_MAR <- make_df(df_all, 'lower01', 'MAR')
plot_estimates_lower01_MAR <- make_plot(estimates_lower01_MAR, 'lower01', c(-1,1.1))+
  ylab('MAR') +
  theme(axis.title.y = element_text(size=12, face="bold"),
        plot.title = element_text(size=12, face="bold"))
estimates_AISB_MAR <- make_df(df_all, 'AIS A-AIS B', 'MAR')
plot_estimates_AISB_MAR <- make_plot(estimates_AISB_MAR, 'AIS A-AIS B', c(-1,1.1))
estimates_AISC_MAR <- make_df(df_all, 'AIS A-AIS C', 'MAR')
plot_estimates_AISC_MAR <- make_plot(estimates_AISC_MAR, 'AIS A-AIS C', c(-1,10))
estimates_AISD_MAR <- make_df(df_all, 'AIS A-AIS D', 'MAR')
plot_estimates_AISD_MAR <- make_plot(estimates_AISD_MAR, 'AIS A-AIS D', c(-1,26))

estimates_lower01_MNAR <- make_df(df_all, 'lower01', 'MNAR')
plot_estimates_lower01_MNAR <- make_plot(estimates_lower01_MNAR, 'lower01', c(-1,1.1))+
  ylab('MNAR') +
  theme(axis.title.y = element_text(size=12, face="bold"),
        plot.title = element_text(size=12, face="bold"))
estimates_AISB_MNAR <- make_df(df_all, 'AIS A-AIS B', 'MNAR')
plot_estimates_AISB_MNAR <- make_plot(estimates_AISB_MNAR, 'AIS A-AIS B', c(-1,1.1))
estimates_AISC_MNAR <- make_df(df_all, 'AIS A-AIS C', 'MNAR')
plot_estimates_AISC_MNAR <- make_plot(estimates_AISC_MNAR, 'AIS A-AIS C', c(-1,10))
estimates_AISD_MNAR <- make_df(df_all, 'AIS A-AIS D', 'MNAR')
plot_estimates_AISD_MNAR <- make_plot(estimates_AISD_MNAR, 'AIS A-AIS D', c(-1,26))

P <- plot_grid(
  plot_estimates_lower01_MCAR, plot_estimates_AISB_MCAR, plot_estimates_AISC_MCAR, plot_estimates_AISD_MCAR,
  plot_estimates_lower01_MAR, plot_estimates_AISB_MAR, plot_estimates_AISC_MAR, plot_estimates_AISD_MAR,
  plot_estimates_lower01_MNAR, plot_estimates_AISB_MNAR, plot_estimates_AISC_MNAR, plot_estimates_AISD_MNAR,
  ncol = 4 , rel_widths = c(2.5, 1, 1, 1), scale=0.9
) + #perhaps reduce this for a bit more space
  draw_label("Mean difference between estimates in fit-pool versus pool-fit scenarios", fontface='bold', x=0.5, y=  0, vjust=-0.5, angle= 0) +
  draw_label("Imputation method", x=0, y=0.5, vjust= 1.5, angle=90, fontface='bold')

