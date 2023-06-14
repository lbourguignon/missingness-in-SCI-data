###############################################################################
# SCI - Handling missing data project 
# L. Bourguignon & L.P. Lukas 
# First version : 23.03.2023
# Last update : 14.06.2023
# ------------------------------------------------------------------------------
# VISUALISE OUTPUT OF THE LINEAR REGRESSION AFTER IMPUTATION - Confidence intervals
################################################################################

################################################################################
# Sourcing additional scripts
################################################################################

source("./0_functions.R")

################################################################################
# Argument loading
################################################################################

args = commandArgs(trailingOnly=TRUE)
variable_with_NA <- args[1]
outcome <- args[2]
sim <- args[3]
variables <- c('lower01', outcome, 'ais1')
beta_coefficients <- c('lower01', "AIS A-AIS B", "AIS A-AIS C", "AIS A-AIS D")
patterns = c('MCAR', 'MAR', 'MNAR')
data_path = '/cluster/scratch/blucie/NA_SCI/subsets_imputed/'
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

################################################################################
# Data preparation
################################################################################

data_lr_v2 <- read.csv(paste0(data_path, 'lr-output_all-subsets_', outcome, '_version2.csv'))

test_CIplot <- dplyr::filter(data_lr_v2,
                             simulation == sim, 
                             var_with_NA %in% c(variable_with_NA))

################################################################################
# Main
################################################################################

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

