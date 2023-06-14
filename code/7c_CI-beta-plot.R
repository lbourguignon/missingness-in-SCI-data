
#data_lr_v2 = read.csv('/Volumes/blucie/PhD/1_SCI/6_Missing_data/lr-after-imputation/500subsets/lr-output_all-subsets_lower52_version2.csv')
#data_lr_v2 = read.csv('/Users/blucie/lr-output_all-subsets_lower52_testn10_version2.csv')

library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)

args = commandArgs(trailingOnly=TRUE)
variable_with_NA <- args[1]
outcome <- args[2]
sim <- args[3]
variables <- c('lower01', outcome, 'ais1')
beta_coefficients <- c('lower01', "AIS A-AIS B", "AIS A-AIS C", "AIS A-AIS D")
patterns = c('MCAR', 'MAR', 'MNAR')
data_path = '/cluster/scratch/blucie/NA_SCI/subsets_imputed/'

data_lr_v2 <- read.csv(paste0(data_path, 'lr-output_all-subsets_', outcome, '_version2.csv'))

imputations = c('case_deletion', 'lr', 'knn', 'RF', 'SVM_linear', 'SVM_rbf', 'mean')
if (variable_with_NA == outcome){
  imputations = append(imputations, c('norm.predict', 'pmm', 'rf', 'last_obs'))
} else if (variable_with_NA == 'lower01'){
  imputations = append(imputations, c('norm.predict', 'pmm', 'rf'))
} else if (variable_with_NA == 'ais1') {
  imputations = append(imputations, 'polr')
}

problemColors <- c("TRUE"="red", "FALSE"="darkgrey")
colorScale <- scale_colour_manual(name="problem", values=problemColors)

test_CIplot <- dplyr::filter(data_lr_v2,
                             simulation == sim, 
                             var_with_NA %in% c(variable_with_NA))

make_df <- function(test_CIplot, variable_interest, pattern){
  test_CIplot_sub <- dplyr::filter(test_CIplot, 
                                   variables %in% c(variable_interest),
                                   pattern_NA == pattern)
  controls <- dplyr::filter(test_CIplot_sub, imputation %in% c("baseline"))[['coef']]
  mean_raw_bias <- c()
  lower_raw_bias <- c()
  upper_raw_bias <- c()
  percentage_bias <- c()
  coverage_rate <- c()
  average_width <- c()
  RMSE_vec <- c()
  for (imp in imputations){
    cases <- dplyr::filter(test_CIplot_sub, imputation %in% imp)[['coef']]
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
  write.csv(estimates, paste0("./important_output/df_CI_plots/", sim,
                              "_outcome-", outcome, 
                              '_NA-', variable_with_NA, 
                              '_explan-variable-', variable_interest,
                              '_pattern-', pattern,
                              "_metrics-CI.csv"), row.names = FALSE)
  return(estimates)
}

make_plot <- function(estimates, variable_interest, lim_x){
  finalTop <- ggplot(data=estimates, aes(x=RB, y=imputation)) +
    
    # add error bars, parameterized by other columns of 'estimates'
    geom_errorbarh(aes(xmin=lower, xmax=upper, color=problem, height = .2)) +
    
    # add point estimate, colored according to the problem column of 'estimate'  
    geom_point(aes(color=problem))  + 
    
    # draw a vertical line at the true difference in mean.
    geom_vline(xintercept = 0, color="black") +
    
    # color the "problem" status according to a scale we set up separately
    colorScale +
    
    # Get rid of gridlines, axes
    theme_minimal() +
    
    # Some theme changes. Get rid of the legend.
    theme(legend.position = "none") +
    xlim(lim_x[1], lim_x[2]) +
    xlab('') +
    ylab('') +
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12))#,
          #axis.title.x = element_text(size=14, face="bold"),
          #axis.title.y = element_text(size=14, face="bold"),
          #axis.title.x = element_blank(),
          #axis.title.y = element_blank())
  
  if (!(variable_interest == 'lower01')){
    finalTop <- finalTop +
      theme(axis.text.y = element_blank())
  }
  
  return(finalTop)
}

estimates_lower01_MCAR <- make_df(test_CIplot, 'lower01', 'MCAR')
plot_estimates_lower01_MCAR <- make_plot(estimates_lower01_MCAR, 'lower01', c(-1,1.1)) +
  ylab('MCAR') +
  ggtitle('LEMS at baseline') + 
  theme(axis.title.y = element_text(size=12, face="bold"),
        plot.title = element_text(size=12, face="bold"))

estimates_AISB_MCAR <- make_df(test_CIplot, 'AIS A-AIS B', 'MCAR')
plot_estimates_AISB_MCAR <- make_plot(estimates_AISB_MCAR, 'AIS A-AIS B', c(-12,4)) +
  ggtitle('AIS A - AIS B') + 
  theme(axis.title.y = element_text(size=12, face="bold"),
        plot.title = element_text(size=12, face="bold"))

estimates_AISC_MCAR <- make_df(test_CIplot, 'AIS A-AIS C', 'MCAR')
plot_estimates_AISC_MCAR <- make_plot(estimates_AISC_MCAR, 'AIS A-AIS C', c(-18.5,10))+
  ggtitle('AIS A - AIS C') + 
  theme(axis.title.y = element_text(size=12, face="bold"),
        plot.title = element_text(size=12, face="bold"))

estimates_AISD_MCAR <- make_df(test_CIplot, 'AIS A-AIS D', 'MCAR')
plot_estimates_AISD_MCAR <- make_plot(estimates_AISD_MCAR, 'AIS A-AIS D', c(-40,31))+
  ggtitle('AIS A - AIS D') + 
  theme(axis.title.y = element_text(size=12, face="bold"),
        plot.title = element_text(size=12, face="bold"))

estimates_lower01_MAR <- make_df(test_CIplot, 'lower01', 'MAR')
plot_estimates_lower01_MAR <- make_plot(estimates_lower01_MAR, 'lower01', c(-1,1.1))+
  ylab('MAR') +
  theme(axis.title.y = element_text(size=12, face="bold"),
        plot.title = element_text(size=12, face="bold"))
estimates_AISB_MAR <- make_df(test_CIplot, 'AIS A-AIS B', 'MAR')
plot_estimates_AISB_MAR <- make_plot(estimates_AISB_MAR, 'AIS A-AIS B', c(-12,4)) 
estimates_AISC_MAR <- make_df(test_CIplot, 'AIS A-AIS C', 'MAR')
plot_estimates_AISC_MAR <- make_plot(estimates_AISC_MAR, 'AIS A-AIS C', c(-18.5,10)) 
estimates_AISD_MAR <- make_df(test_CIplot, 'AIS A-AIS D', 'MAR')
plot_estimates_AISD_MAR <- make_plot(estimates_AISD_MAR, 'AIS A-AIS D', c(-40,31))

estimates_lower01_MNAR <- make_df(test_CIplot, 'lower01', 'MNAR')
plot_estimates_lower01_MNAR <- make_plot(estimates_lower01_MNAR, 'lower01', c(-1,1.1))+
  ylab('MNAR') +
  theme(axis.title.y = element_text(size=12, face="bold"),
        plot.title = element_text(size=12, face="bold"))
estimates_AISB_MNAR <- make_df(test_CIplot, 'AIS A-AIS B', 'MNAR')
plot_estimates_AISB_MNAR <- make_plot(estimates_AISB_MNAR, 'AIS A-AIS B', c(-12,4)) 
estimates_AISC_MNAR <- make_df(test_CIplot, 'AIS A-AIS C', 'MNAR')
plot_estimates_AISC_MNAR <- make_plot(estimates_AISC_MNAR, 'AIS A-AIS C', c(-18.5,10)) 
estimates_AISD_MNAR <- make_df(test_CIplot, 'AIS A-AIS D', 'MNAR')
plot_estimates_AISD_MNAR <- make_plot(estimates_AISD_MNAR, 'AIS A-AIS D', c(-40,31))

P <- plot_grid(
  plot_estimates_lower01_MCAR, plot_estimates_AISB_MCAR, plot_estimates_AISC_MCAR, plot_estimates_AISD_MCAR,
  plot_estimates_lower01_MAR, plot_estimates_AISB_MAR, plot_estimates_AISC_MAR, plot_estimates_AISD_MAR,
  plot_estimates_lower01_MNAR, plot_estimates_AISB_MNAR, plot_estimates_AISC_MNAR, plot_estimates_AISD_MNAR,
  ncol = 4 , rel_widths = c(2.5, 1, 1, 1), scale=0.9
) + #perhaps reduce this for a bit more space
  draw_label("Mean difference between estimates before and after imputation", fontface='bold', x=0.5, y=  0, vjust=-0.5, angle= 0) +
  draw_label("Imputation method", x=0, y=0.5, vjust= 1.5, angle=90, fontface='bold')


filename = paste0("./important_output/CI_plots/", sim,
                  "_outcome-", outcome, 
                  '_NA-', variable_with_NA, 
                  "_CI.png")
ggsave(filename, plot = P, width=29.7, height=21.0, unit="cm")

