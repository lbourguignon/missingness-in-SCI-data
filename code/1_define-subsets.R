################################################################################
# SCI - Handling missing data project 
# L. Bourguignon & L.P. Lukas 
# First version : 06.07.2021
# Last update : 13.07.2022
# ------------------------------------------------------------------------------
# FILE 1 - DEFINE SUBSETS
################################################################################

################################################################################
# Data and argument loading
################################################################################

data_path = '/cluster/scratch/blucie/Data/SCI/Sygen/'
sygen_raw <- read.csv(paste(data_path, 'JohnKramersProject_DATA_2019-10-07_0111.csv', sep = ''))

args = commandArgs(trailingOnly=TRUE)
outcome <- args[1]

set.seed(1)

################################################################################
# Sourcing additional scripts
################################################################################

source("./0_functions.R")

################################################################################
# Prepare Sygen data - step 1 (format the raw data)
################################################################################

# Select variables of interest for the simulation study
sygen_analysis <- subset_col(c('ptid', 'age', 'sexcd', 'splvl', 'ais1', 'lower01', outcome), sygen_raw)

# Transform level of injury into categories "cervical" and "thoracic"
sygen_analysis <- sygen_analysis %>%
  dplyr::mutate(level = case_when(
    grepl('C', splvl, fixed = TRUE) ~ "cervical",
    grepl('T', splvl, fixed = TRUE) ~ "thoracic"
  ))

dim(sygen_analysis) # n = 797
sygen_analysis[sygen_analysis == ''] <- NA # replace empty entries by NA

## Numeric variables
cols.num <- c("age", "lower01", outcome)
sygen_analysis[cols.num] <- sapply(sygen_analysis[cols.num], as.numeric)

## Factor variables
cols.fac <- c("ais1", "splvl", "sexcd", "level")
sygen_analysis[cols.fac] <- lapply(sygen_analysis[cols.fac], factor)

## Statistical test for MCAR, H_0: The data is missing completely at random
mcar_test(sygen_analysis) # H_0 is rejected with a pvalue of 0.00178, 8 missing patterns

################################################################################
# Prepare Sygen data - step 2 (selecting only complete cases for analysis)
################################################################################

sygen_analysis_subset <- sygen_analysis[complete.cases(sygen_analysis), ]
dim(sygen_analysis_subset) # n = 546 if outcome == 'lower52', n = 571 if outcome == 'lower26'

row.names(sygen_analysis_subset) <- NULL

# # sanity checks
# sygen_analysis_subset %>%
#  missing_plot()

################################################################################
# Selecting 500 patients at random
# Proportion of each AIS grade based on proportions observed in other cohorts
################################################################################

# proportion based on the entire Sygen cohort
prop_Sygen <- round_preserve_sum(table(sygen_analysis$ais1)/sum(table(sygen_analysis$ais1)), digits=2)
# proportions based on the subset of complete cases defined for the simulation scenario
prop_completesubset_Sygen <- round_preserve_sum(table(sygen_analysis_subset$ais1)/sum(table(sygen_analysis_subset$ais1)), digits=2)
print(prop_completesubset_Sygen)
# proportion based on the distribution in the EMSCI as reported in https://doi.org/10.1186/s12916-022-02395-0
prop_EMSCI <- c(0.39, 0.12, 0.19, 0.30)
# artificial scenario where all AIS severity grades are equally represented
prop_balanced <- rep(0.25,4)

## additional scenarii
#prop_SCIRehab <- c(0.5, 0.15, 0.19, 0.16)
#prop_Murnau <- c(0.34, 0.09, 0.11, 0.46)

list_proportions <- list(prop_Sygen, prop_completesubset_Sygen, prop_EMSCI, prop_balanced)
names_subsets <- c('SygenAll', 'SygenCompleteNAanalysis', 'EMSCI', 'balanced')

sygen_analysis_subset_aisA <- sygen_analysis_subset[which(sygen_analysis_subset$ais1=='AIS A'), ]
sygen_analysis_subset_aisB <- sygen_analysis_subset[which(sygen_analysis_subset$ais1=='AIS B'), ]
sygen_analysis_subset_aisC <- sygen_analysis_subset[which(sygen_analysis_subset$ais1=='AIS C'), ]
sygen_analysis_subset_aisD <- sygen_analysis_subset[which(sygen_analysis_subset$ais1=='AIS D'), ]

# Data to be merged in order to perform last observation carried forward
if (outcome == 'lower52'){
    sub_lower16_26 <- subset(sygen_raw[c('ptid', 'lower26')])
} else if (outcome == 'lower26'){
    sub_lower16_26 <- subset(sygen_raw[c('ptid', 'lower16')])
}

for (scenario in c(1:length(list_proportions))){
  prop_pop = list_proportions[scenario]
  name = names_subsets[scenario]
  for (i in c(1:500)){
    sub_A <- sample_n(sygen_analysis_subset_aisA, replace=T, size=prop_pop[[1]][1]*500)
    sub_B <- sample_n(sygen_analysis_subset_aisB, replace=T, size=prop_pop[[1]][2]*500)
    sub_C <- sample_n(sygen_analysis_subset_aisC, replace=T, size=prop_pop[[1]][3]*500)
    sub_D <- sample_n(sygen_analysis_subset_aisD, replace=T, size=prop_pop[[1]][4]*500)
    sub_all <- rbind(sub_A, sub_B, sub_C, sub_D)
    df_temp <- introduce_missingness(data = sub_all,
                                     cols = c('ais1', 'lower01', outcome),
                                     patterns = c('MCAR', 'MAR', 'MNAR'),
                                     prop = 0.3)
    df_temp$ais1_MCAR <- aisgrade_levelfactor(df_temp$ais1_MCAR)
    df_temp$ais1_MAR <- aisgrade_levelfactor(df_temp$ais1_MAR)

    df_temp <- merge(sub_lower16_26, df_temp, by='ptid')
    
    if (dim(df_temp)[1]<=500){
      write.csv(df_temp,
                paste0("/cluster/scratch/blucie/NA_SCI/subsets_withNA/", name, "_outcome", outcome, "/subset_", as.character(i), '_n500.csv'),
                row.names = FALSE)
    } else {
      print('problem')
      print(name)
      print(dim(df_temp)[1])
    }
  }
}
