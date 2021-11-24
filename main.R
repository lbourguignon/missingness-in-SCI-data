################################################################################
# SCI - Handling missing data project 
# L. Bourguignon & L.P. Lukas 
# First version : 06.07.2021
# Last update : 31.10.2021
# ------------------------------------------------------------------------------
# MAIN FILE
################################################################################

################################################################################
# Packages and seed
################################################################################

library(finalfit)  
library(tidyr)
library(ggplot2)
library(mice)
library(nnet)
library(ggpubr)
library(rlang)
library(stringr)
library(dplyr)
library(reshape2)
library(naniar)
library(networkD3)
library(rpart)
library(rpart.plot)

set.seed(1)

################################################################################
# Data loading
################################################################################

data_path = '/Volumes/borgwardt/Data/SCI/'
sygen_raw <- read.csv(paste(data_path, 'Sygen/JohnKramersProject_DATA_2019-10-07_0111.csv', sep = ''))

################################################################################
# Sourcing additional scripts
################################################################################

source("/Users/blucie/PhD/1_SCI/6_Missing_data/code/functions.R")
source("/Users/blucie/PhD/1_SCI/6_Missing_data/code/visual_exploration.R")

################################################################################
# Prepare Sygen data - step 1 (visual inspection of the raw data)
################################################################################

sygen_analysis <- subset_col(c('ptid', 'age', 'sexcd', 'splvl', 'ais1', 'lower01', 'lower52'), sygen_raw)

sygen_analysis <- sygen_analysis %>%
  mutate(level = case_when(
    grepl('C', splvl, fixed = TRUE) ~ "cervical",
    grepl('T', splvl, fixed = TRUE) ~ "thoracic"
  ))

dim(sygen_analysis) # n = 797
sygen_analysis[sygen_analysis == ''] <- NA

## Numeric variables
cols.num <- c("age", "lower01", "lower52")
sygen_analysis[cols.num] <- sapply(sygen_analysis[cols.num], as.numeric)

## Factor variables
cols.fac <- c("ais1", "splvl", "sexcd", "level")
sygen_analysis[cols.fac] <- lapply(sygen_analysis[cols.fac], factor)

################################################################################
# Visualise raw Sygen data
################################################################################

## Visualise original missingness patterns

missing_dist
missing_dist_fill
missing_heatmap

# ------------------------------------------------------------------------------

## Visualise the distributions of the different variables

ais_sygen_raw # AIS grade, i.e. severity
age_sygen_raw # Age distribution
sex_sygen_raw # Sex 
lems01_sygen_raw # Distribution of LEMS at week 01
lems52_sygen_raw # Distribution of LEMS at week 52
nli_sygen_raw # Level of injury
jitterLEMS_byAIS_sygen_raw # Relationship between LEMS at week 01 and week 52, coloured by AIS grade
distLEMS_byAIS_sygen_raw # Change in LEMS distribution over time, by AIS grade

################################################################################
# Prepare Sygen data - step 2 (selecting only complete cases for analysis)
################################################################################

sygen_analysis_subset <- sygen_analysis[complete.cases(sygen_analysis), ]
dim(sygen_analysis_subset) # n = 546

row.names(sygen_analysis_subset) <- NULL

## sanity checks
#sygen_analysis_subset %>%
#  missing_plot()

################################################################################
# Introduce missingness in column of your choice with pattern of your choice
################################################################################

sygen_analysis_subset <- introduce_missingness(data = sygen_analysis_subset, 
                                               cols = c('ais1', 'lower01', 'lower52'),
                                               patterns = c('MCAR', 'MAR', 'MNAR'),
                                               prop = 0.3)

## Numeric variables
cols.num <- c("age", "lower01", "lower01_MCAR", "lower01_MAR", "lower01_MNAR",
              "lower52", "lower52_MCAR", "lower52_MAR", "lower52_MNAR")
sygen_analysis_subset[cols.num] <- sapply(sygen_analysis_subset[cols.num], as.numeric)

## Factor variables

cols.fac <- c("ais1", "splvl", "sexcd", 'ais1_MCAR', 'ais1_MAR', "ais1_MNAR", 'level')
sygen_analysis_subset[cols.fac] <- sapply(sygen_analysis_subset[cols.fac], as.factor)

# Transform AIS grade columns back to AIS and not numbers
sygen_analysis_subset$ais1_MCAR <- aisgrade_levelfactor(sygen_analysis_subset$ais1_MCAR)
sygen_analysis_subset$ais1_MAR <- aisgrade_levelfactor(sygen_analysis_subset$ais1_MAR)

row.names(sygen_analysis_subset) <- NULL

# ------------------------------------------------------------------------------
## Sanity checks

head(sygen_analysis_subset)
# Print column names
names(sygen_analysis_subset)
# Proportion of missingness introduced
sapply(sygen_analysis_subset, function(y) sum(length(which(is.na(y)))))
sapply(sygen_analysis_subset, function(col)sum(is.na(col))/length(col))
# Type of each column
sapply(sygen_analysis_subset, class)

################################################################################
# Evaluate the effect of different imputation strategies
################################################################################

result_imputation_ais <- result_imputations(data = sygen_analysis_subset,
                                 var = 'ais1',
                                 patterns = c('MCAR', 'MAR', 'MNAR'),
                                 imp = c('case_deletion', 'majority', 'regression'))
result_imputation_lems01 <- result_imputations(data = sygen_analysis_subset,
                                    var = 'lower01',
                                    patterns = c('MCAR', 'MAR', 'MNAR'),
                                    imp = c('case_deletion', 'mean', 'regression'))
result_imputation_lems52 <- result_imputations(data = sygen_analysis_subset,
                                    var = 'lower52',
                                    patterns = c('MCAR', 'MAR', 'MNAR'),
                                    imp = c('case_deletion', 'mean', 'regression', 'last_obs_forward'),
                                    raw = sygen_raw) #additional parameter is necessary in function for the last_obs_forward imputation strategy

plots_ais <- plot_imputation_results(results_data = result_imputation_ais[1][[1]],
                                     dist_data = result_imputation_ais[2][[1]], 
                                     var = 'ais1',
                                     patterns = c('MCAR', 'MAR', 'MNAR'),
                                     imp = c('case_deletion', 'majority', 'regression'))
plots_ais[[1]]
plots_ais[[2]]

plots_lems01 <- plot_imputation_results(results_data = result_imputation_lems01[1][[1]],
                                        dist_data = result_imputation_lems01[2][[1]], 
                                        var = 'lower01',
                                        patterns = c('MCAR', 'MAR', 'MNAR'),
                                        imp = c('case_deletion', 'mean', 'regression'))
plots_lems01[[1]]
plots_lems01[[2]]


plots_lems52 <- plot_imputation_results(results_data = result_imputation_lems52[1][[1]],
                                        dist_data = result_imputation_lems52[2][[1]], 
                                        var = 'lower52',
                                        patterns = c('MCAR', 'MAR', 'MNAR'),
                                        imp = c('case_deletion', 'mean', 'regression', 'last_obs_forward'))
plots_lems52[[1]]
plots_lems52[[2]]
