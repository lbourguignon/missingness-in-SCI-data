################################################################################
# SCI - Handling missing data project
# L. Bourguignon
# First version : 06.07.2021
# Last update : 27.07.2021
################################################################################

################################################################################
# Packages and seed
################################################################################

#install.packages("finalfit")
library(finalfit) 

set.seed(1)

################################################################################
# Data loading
################################################################################

data_path = '/Volumes/borgwardt/Data/SCI/'
sygen_raw <- read.csv(paste(data_path, 'Sygen/JohnKramersProject_DATA_2019-10-07_0111.csv', sep = ''))

################################################################################
# Functions
################################################################################

subset_col <- function(vec, df){
  return(df[vec])
}

vector_var <- function(vec, week, order){
  if (order == 'before'){
    return(paste(week, vec, sep=""))
  }
  if (order == 'after'){
    return(paste(vec, week, sep=""))
  }
}

generate_missing_col_CAR <- function(data, prop, col){
  vec <- c(sygen_analysis_subset[[col]])
  is.na(vec) <- sample(length(vec), prop*length(vec))
  return (vec)
}

generate_missing_col_AR <- function(data, prop, col){
  data_female <- data[data$sexcd == 1,]
  data_male <- data[data$sexcd == 2,]
  vec_female <- c(data_female[[col]])
  vec_male <- c(data_male[[col]])
  is.na(vec_female) <- sample(length(vec_female), 0.1*length(data_female$sexcd))
  is.na(vec_male) <- sample(length(vec_male), 0.35*length(data_male$sexcd))
  data_female$missing_col <- vec_female
  data_male$missing_col <- vec_male
  df_combined <- rbind(data_female, data_male)
  #data = merge(data, df_combined[c('ptid', 'missing_col')])
  df_combined <- df_combined[c('ptid', 'missing_col')]
  colnames(df_combined)[which(names(df_combined) == "missing_col")] <- paste0(col, '_MAR')
  return (df_combined)
}

generate_missing_col_NAR <- function(data, col){
  if (col == 'ais1'){
    data[col][data[col] == 'AIS D'] <- NA
    data[col][data[col] == 'AIS C'] <- NA
    vec <- c(data[[col]])
  } else if (col == 'lower01'){
    data[col][data[col] > 0] <- NA
    vec <- c(data[[col]])
  } else if (col == 'lower52'){
    data[col][data[col] > 15] <- NA
    vec <- c(data[[col]])
  }
  return (vec)
}

################################################################################
# Report number of missing data depending on different scenarii in Sygen data
################################################################################

sygen_vec_upper_mot <- c('finexl', 'finexr', 'fooevl', 'fooevr', 
                         'indfil', 'indfir','knefll', 'kneflr', 
                         'shlabl', 'shlabr', 'thuopl', 'thuopr',
                         'wriral', 'wrirar')

sygen_vec_lower_mot <- c('ankdol', 'ankdor', 'ankpll', 'ankplr',
                         'elbexl', 'elbexr', 'elbfll', 'elbflr', 
                         'finabl', 'finabr','finfll', 'finflr', 
                         'gretol', 'gretor','hipfll', 'hipflr', 
                         'kneetr', 'kneexl', 'wrextl', 'wrextr')

sygen_levels <- c('c2', 'c3', 'c4', 'c4', 'c6', 'c7', 'c8',
                  't1', 't2', 't3', 't4', 't5', 't6', 't7', 't8', 't9', 't10', 't11', 't12',
                  'l1', 'l2', 'l3', 'l4', 'l5',
                  's1', 's2', 's3', 's45')

timepoints <- c('00', '01', '04', '08', '16', '26', '52')

# Number of patients per LEMS score per timepoint (including NAs)
for (col in vector_var(timepoints, 'lower', 'before')){
  print(col)
  print(table(sygen_raw[col], exclude = NULL))
}

# Number of patients per UEMS score per timepoint (including NAs)
for (col in vector_var(timepoints, 'upper', 'before')){
  print(col)
  print(table(sygen_raw[col], exclude = NULL))
}

# Number of patients per PinPrick score per timepoint (including NAs)
for (col in vector_var(timepoints[-1], 'ppscor', 'before')){
  print(col)
  print(table(sygen_raw[col], exclude = NULL))
}

# Number of patients with all PP scores available
#temp <- subset_col(vector_var(timepoints, 'ppscor', 'before'), sygen_raw)
#dim(temp[complete.cases(temp), ])

# df_temp = subset_col(c('ppscor01'), sygen_raw)
# df_temp['uems_outcome'] = get_outcome_vec('ppscor')
# dim(df_temp[complete.cases(df_temp), ])
# 
# sygen_week01_pp <- subset_col(append(vector_var(sygen_levels, 'ppl52', 'after'), 
#                                      vector_var(sygen_levels, 'ppr52', 'after')),
#                               sygen_raw)
# dim(sygen_week01_pp[complete.cases(sygen_week01_pp), ])
# sygen_week01_lt <- subset_col(append(vector_var(sygen_levels, 'ltl52', 'after'), 
#                                      vector_var(sygen_levels, 'ltr52', 'after')), 
#                               sygen_raw)
# dim(sygen_week01_lt[complete.cases(sygen_week01_lt), ])
# 
# sygen_week01_upper_mot <- subset_col(append(vector_var(sygen_vec_upper_mot, '52', 'after'), 
#                                             c('ptid')), sygen_raw)
# dim(sygen_week01_upper_mot[complete.cases(sygen_week01_upper_mot), ])
# 
# sygen_week01_lower_mot <- subset_col(append(vector_var(sygen_vec_lower_mot, '52', 'after'), 
#                                             c('ptid')), sygen_raw)
# dim(sygen_week01_lower_mot[complete.cases(sygen_week01_lower_mot), ])
# 
# sygen_week01_lower_mot <- subset_col(append(vector_var(sygen_vec_upper_mot, '00', 'after'), 
#                                             c('ptid')), sygen_raw)
# for (score in sygen_vec_upper_mot){
#   print(score)
#   sygen_week01_lower_mot[paste0(score, 'outcome')] <- get_outcome_vec(sco)
# }
# dim(sygen_week01_lower_mot[complete.cases(sygen_week01_lower_mot), ])
# 
# 
# 
# sygen_week01_pp <- subset_col(append(vector_var(sygen_levels, 'ppl01', 'after'), 
#                                      vector_var(sygen_levels, 'ppr01', 'after')), 
#                               sygen_raw)
# for (sco in sygen_levels){
#   scorel <- paste0(sco, 'ppl')
#   scorer <- paste0(sco, 'ppr')
#   sygen_week01_pp[paste0(scorel, 'outcomel')] <- get_outcome_vec(scorel)
#   sygen_week01_pp[paste0(scorer, 'outcomer')] <- get_outcome_vec(scorer)
# }
# dim(sygen_week01_pp[complete.cases(sygen_week01_pp), ])

################################################################################
# Visualise missing data in raw Sygen cohort
################################################################################

sygen_analysis <- subset_col(c('ptid', 'age', 'sexcd', 
                                      'splvl', 'ais1', 'lower01', 'lower52'), 
                                    sygen_raw)
dim(sygen_analysis) # n = 797
sygen_analysis[sygen_analysis == ''] <- NA

explanatory = c("age", "sexcd", "splvl", "lower01", "ais1")
dependent = "lower52"

# ref for interpretation of the folloming plot: 
# https://cran.r-project.org/web/packages/finalfit/vignettes/missing.html

sygen_analysis %>% 
  missing_pairs(dependent, explanatory)
sygen_analysis %>% 
  missing_pairs(dependent, explanatory, position = "fill", )

# x-axis represent patients, light blue = NA
sygen_analysis %>%
  missing_plot()

################################################################################
# Filter out missing values to use only complete cases in subsequent analyses 
# --> baseline model
################################################################################

sygen_analysis_subset <- sygen_analysis[complete.cases(sygen_analysis), ]
dim(sygen_analysis_subset) # n = 546

row.names(sygen_analysis_subset) <- NULL

# sanity checks
sygen_analysis_subset %>%
  missing_plot()

head(sygen_analysis_subset)

################################################################################
# Introduce missing data (one column per variable per pattern of NAs)
# ------------------------------------------------------------------------------
# MCAR = missing completely at random
# MAR = missing at random
# MNAR = missing not at random
################################################################################

# ------------------------------------------------------------------------------
# Create columns with 30% of data missing completely at random
# ---> easiest and over simplified scenario
# ------------------------------------------------------------------------------

## ais grade --> example of missing data in confounder variable
sygen_analysis_subset$ais1_MCAR <- generate_missing_col_CAR(sygen_analysis_subset, 0.3, 'ais1')
table(sygen_analysis_subset$ais1_MCAR, useNA = 'always')

## lems at week 1 --> example of missing data in explanatory variable
sygen_analysis_subset$lower01_MCAR <- generate_missing_col_CAR(sygen_analysis_subset, 0.3, 'lower01')
table(sygen_analysis_subset$lower01_MCAR, useNA = 'always')

## lems at week 52 --> example of missing data in outcome variable
sygen_analysis_subset$lower52_MCAR <- generate_missing_col_CAR(sygen_analysis_subset, 0.3, 'lower52')
table(sygen_analysis_subset$lower52_MCAR, useNA = 'always')

head(sygen_analysis_subset)

#-------------------------------------------------------------------------------
# Create columns with 30% of data missing at random
# --> conditional on sex, i.e. more of one sex missing
# ------------------------------------------------------------------------------

## ais grade --> example of missing data in confounder variable
df_temp <- generate_missing_col_AR(sygen_analysis_subset, 0.3, 'ais1')
sygen_analysis_subset = merge(sygen_analysis_subset, df_temp)
table(sygen_analysis_subset$ais1_MAR, useNA = 'always')

## lems at week 1 --> example of missing data in explanatory variable
df_temp <- generate_missing_col_AR(sygen_analysis_subset, 0.3, 'lower01')
sygen_analysis_subset = merge(sygen_analysis_subset, df_temp)
table(sygen_analysis_subset$lower01_MAR, useNA = 'always')

## lems at week 52 --> example of missing data in outcome variable
df_temp <- generate_missing_col_AR(sygen_analysis_subset, 0.3, 'lower52')
sygen_analysis_subset = merge(sygen_analysis_subset, df_temp)
table(sygen_analysis_subset$lower52_MAR, useNA = 'always')

#-------------------------------------------------------------------------------
# Create columns with 30% of data missing not at random
# --> patients with higher score at week 52 are more likely to have a NA
#-------------------------------------------------------------------------------

## ais grade --> example of missing data in confounder variable
sygen_analysis_subset$ais1_MNAR <- generate_missing_col_NAR(sygen_analysis_subset, 'ais1')
table(sygen_analysis_subset$ais1_MNAR, useNA = 'always')

## lems at week 1 --> example of missing data in explanatory variable
sygen_analysis_subset$lower01_MNAR <- generate_missing_col_NAR(sygen_analysis_subset, 'lower01')
table(sygen_analysis_subset$lower01_MNAR, useNA = 'always')

## lems at week 52 --> example of missing data in outcome variable
sygen_analysis_subset$lower52_MNAR <- generate_missing_col_NAR(sygen_analysis_subset, 'lower52')
table(sygen_analysis_subset$lower52_MNAR, useNA = 'always')

#-------------------------------------------------------------------------------
# Convert the different to the correct type : factor or numeric variables
#-------------------------------------------------------------------------------

## Numeric variables
cols.num <- c("age", 
              "lower01", "lower01_MCAR", "lower01_MAR", "lower01_MNAR",
              "lower52", "lower52_MCAR", "lower52_MAR", "lower52_MNAR")
sygen_analysis_subset[cols.num] <- sapply(sygen_analysis_subset[cols.num], 
                                          as.numeric)

## Factor variables
cols.fac <- c("ais1", "splvl", "sexcd", 'ais1_MCAR', 'ais1_MAR', "ais1_MNAR")
sygen_analysis_subset[cols.fac] <- lapply(sygen_analysis_subset[cols.fac], 
                                          factor)

## Check type of each column
sapply(sygen_analysis_subset, class)

# # In the raw data
# cols.num <- c("age", "lower01", "lower52")
# sygen_raw[cols.num] <- sapply(sygen_raw[cols.num], as.numeric)
# ## Factor variables
# cols.fac <- c("ais1", "splvl", "sexcd")
# sygen_raw[cols.fac] <- lapply(sygen_raw[cols.fac], factor)
# ## Check type of each column
# sapply(sygen_raw, class)

#-------------------------------------------------------------------------------

row.names(sygen_analysis_subset) <- NULL

#-------------------------------------------------------------------------------

################################################################################
# Visual inspection of missingness introduced
################################################################################

dependent = "lower52"
explanatory_subset = c("age", "sexcd", "splvl")

#-------------------------------------------------------------------------------
# 0 -- baseline
#-------------------------------------------------------------------------------

sygen_analysis_subset %>% missing_plot()

explanatory_full = c(explanatory_subset, 'lower01', 'ais1')
sygen_analysis_subset %>% missing_pairs(dependent, explanatory)
#sygen_analysis_subset %>% missing_pairs(dependent, explanatory, position = "fill", )

#-------------------------------------------------------------------------------
# 1A -- MCAR in a confounding variable (ais1_MCAR)
#-------------------------------------------------------------------------------

explanatory_full = c(explanatory_subset, 'lower01', 'ais1_MCAR')

#sygen_analysis_subset %>% missing_pairs(dependent, explanatory_full)
sygen_analysis_subset %>% missing_pairs(dependent, explanatory_full, position = "fill", )

#-------------------------------------------------------------------------------
# 1B -- MAR in a confounding variable (ais1_MAR)
#-------------------------------------------------------------------------------

explanatory_full = c(explanatory_subset, 'lower01', 'ais1_MAR')

#sygen_analysis_subset %>% missing_pairs(dependent, explanatory_full)
sygen_analysis_subset %>% missing_pairs(dependent, explanatory_full, position = "fill", )

#-------------------------------------------------------------------------------
# 1C -- MAR in a confounding variable (ais1_MNAR)
#-------------------------------------------------------------------------------

explanatory_full = c(explanatory_subset, 'lower01', 'ais1_MNAR')

#sygen_analysis_subset %>% missing_pairs(dependent, explanatory_full)
sygen_analysis_subset %>% missing_pairs(dependent, explanatory_full, position = "fill", )

#-------------------------------------------------------------------------------
# 2A -- MCAR in an explanatory variable (lower01_MCAR)
#-------------------------------------------------------------------------------

explanatory_full = c(explanatory_subset, 'lower01_MCAR', 'ais1')

#sygen_analysis_subset %>% missing_pairs(dependent, explanatory_full)
sygen_analysis_subset %>% missing_pairs(dependent, explanatory_full, position = "fill", )

#-------------------------------------------------------------------------------
# 2B -- MAR in an explanatory variable (lower01_MAR)
#-------------------------------------------------------------------------------

explanatory_full = c(explanatory_subset, 'lower01_MAR', 'ais1')

#sygen_analysis_subset %>% missing_pairs(dependent, explanatory_full)
sygen_analysis_subset %>% missing_pairs(dependent, explanatory_full, position = "fill", )

#-------------------------------------------------------------------------------
# 2C -- MNAR in an explanatory variable (lower01_MNAR)
#-------------------------------------------------------------------------------

explanatory_full = c(explanatory_subset, 'lower01_MNAR', 'ais1')

#sygen_analysis_subset %>% missing_pairs(dependent, explanatory_full)
sygen_analysis_subset %>% missing_pairs(dependent, explanatory_full, position = "fill", )

#-------------------------------------------------------------------------------
# 3A -- MCAR in an outcome variable (lower52_MCAR)
#-------------------------------------------------------------------------------

explanatory_full = c(explanatory_subset, 'lower01', 'ais1')
dependent <- 'lower52_MCAR'

#sygen_analysis_subset %>% missing_pairs(dependent, explanatory_full)
sygen_analysis_subset %>% missing_pairs(dependent, explanatory_full, position = "fill", )

#-------------------------------------------------------------------------------
# 3B -- MAR in an outcome variable (lower52_MAR)
#-------------------------------------------------------------------------------

dependent <- 'lower52_MAR'

#sygen_analysis_subset %>% missing_pairs(dependent, explanatory_full)
sygen_analysis_subset %>% missing_pairs(dependent, explanatory_full, position = "fill", )

#-------------------------------------------------------------------------------
# 3C -- MNAR in an outcome variable (lower52_MNAR)
#-------------------------------------------------------------------------------

dependent <- 'lower52_MNAR'

#sygen_analysis_subset %>% missing_pairs(dependent, explanatory_full)
sygen_analysis_subset %>% missing_pairs(dependent, explanatory_full, position = "fill", )

################################################################################
# Imputation step
################################################################################

################################################################################
# Statistical analyses
################################################################################

#-------------------------------------------------------------------------------
# 0 -- Baseline results with complete data
#-------------------------------------------------------------------------------

summary(lm(lower52 ~ age + sexcd + splvl + lower01 + ais1, data = sygen_analysis_subset))

#-------------------------------------------------------------------------------
# 1A -- MCAR in a confounding variable (ais1_MCAR)
#-------------------------------------------------------------------------------

# Case deletion
summary(lm(lower52 ~ age + sexcd + splvl + lower01 + ais1_MCAR, data = sygen_analysis_subset))

# Mean imputation

# Regression imputation

# Last observation carried forward

# Maximum likelihood

# Expectation-Maximization

# Multiple imputation

#-------------------------------------------------------------------------------
# 1B -- MAR in a confounding variable (ais1_MAR)
#-------------------------------------------------------------------------------

# Case deletion
summary(lm(lower52 ~ age + sexcd + splvl + lower01 + ais1_MAR, data = sygen_analysis_subset))

# Mean imputation

# Regression imputation

# Last observation carried forward

# Maximum likelihood

# Expectation-Maximization

# Multiple imputation

#-------------------------------------------------------------------------------
# 1C -- MAR in a confounding variable (ais1_MNAR)
#-------------------------------------------------------------------------------

# Case deletion
summary(lm(lower52 ~ age + sexcd + splvl + lower01 + ais1_MNAR, data = sygen_analysis_subset))

# Mean imputation

# Regression imputation

# Last observation carried forward

# Maximum likelihood

# Expectation-Maximization

# Multiple imputation

#-------------------------------------------------------------------------------
# 2A -- MCAR in an explanatory variable (lower01_MCAR)
#-------------------------------------------------------------------------------

# Case deletion
summary(lm(lower52 ~ age + sexcd + splvl + lower01_MCAR + ais1, data = sygen_analysis_subset))

# Mean imputation

# Regression imputation

# Last observation carried forward

# Maximum likelihood

# Expectation-Maximization

# Multiple imputation

#-------------------------------------------------------------------------------
# 2B -- MAR in an explanatory variable (lower01_MAR)
#-------------------------------------------------------------------------------

# Case deletion
summary(lm(lower52 ~ age + sexcd + splvl + lower01_MAR + ais1, data = sygen_analysis_subset))

# Mean imputation

# Regression imputation

# Last observation carried forward

# Maximum likelihood

# Expectation-Maximization

# Multiple imputation

#-------------------------------------------------------------------------------
# 2C -- MNAR in an explanatory variable (lower01_MNAR)
#-------------------------------------------------------------------------------

# Case deletion
summary(lm(lower52 ~ age + sexcd + splvl + lower01_MNAR + ais1, data = sygen_analysis_subset))

# Mean imputation

# Regression imputation

# Last observation carried forward

# Maximum likelihood

# Expectation-Maximization

# Multiple imputation

#-------------------------------------------------------------------------------
# 3A -- MCAR in an outcome variable (lower52_MCAR)
#-------------------------------------------------------------------------------

# Case deletion
summary(lm(lower52_MCAR ~ age + sexcd + splvl + lower01 + ais1, data = sygen_analysis_subset))

# Mean imputation

# Regression imputation

# Last observation carried forward

# Maximum likelihood

# Expectation-Maximization

# Multiple imputation

#-------------------------------------------------------------------------------
# 3B -- MAR in an outcome variable (lower52_MAR)
#-------------------------------------------------------------------------------

# Case deletion
summary(lm(lower52_MAR ~ age + sexcd + splvl + lower01 + ais1, data = sygen_analysis_subset))

# Mean imputation

# Regression imputation

# Last observation carried forward

# Maximum likelihood

# Expectation-Maximization

# Multiple imputation

#-------------------------------------------------------------------------------
# 3C -- MNAR in an outcome variable (lower52_MNAR)
#-------------------------------------------------------------------------------

# Case deletion
summary(lm(lower52_MNAR ~ age + sexcd + splvl + lower01 + ais1, data = sygen_analysis_subset))

# Mean imputation

# Regression imputation

# Last observation carried forward

# Maximum likelihood

# Expectation-Maximization

# Multiple imputation

