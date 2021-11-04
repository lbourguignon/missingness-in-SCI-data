################################################################################
# SCI - Handling missing data project
# L. Bourguignon
# First version : 06.07.2021
# Last update : 12.10.2021
# ------------------------------------------------------------------------------
# MAIN FILE
################################################################################

################################################################################
# Packages and seed
################################################################################

library(tidyverse)
library(cowplot)
library(finalfit)
library(mice)
library(nnet)

set.seed(1)

# which data set?
data.set <- "sygen" # sygen or emsci

# path to remote storage (if mounted under MacOS)
path.to.data = "/Volumes/borgwardt/Data/SCI/"

################################################################################
# Data loading
################################################################################

if (data.set == "sygen") {
    injury_levels  <-  c(paste(rep("C0", 8), 1:8, sep = ""),
                      paste(rep("T0", 9), 1:9, sep = ""),
                      paste(rep("T", 3), 10:12, sep = ""))
    AIS_grades <- c("AIS A", "AIS B", "AIS C", "AIS D")
    data <- read_csv(paste(path.to.data,
                           "Sygen/JohnKramersProject_DATA_2019-10-07_0111.csv",
                           sep = ""),
                     col_types = list(ptid =  col_character(),
                                      age = col_integer(),
                                      sexcd = col_factor(levels = c("1", # male
                                                                    "2") # female
                                      ),
                                      splvl = col_factor(levels = injury_levels),
                                      ais1 = col_factor(levels = AIS_grades),
                                      lower01 = col_integer(),
                                      lower52 = col_integer()),
                     col_select = c("ptid",
                                    "age",
                                    "sexcd",
                                    "splvl",
                                    "ais1",
                                    "lower01",
                                    "lower52"),
                     na = c("", "NA")) %>%
        mutate(
            across(starts_with("ais"),
                   ~recode(.,
                           "AIS A" = "A",
                           "AIS B" = "B",
                           "AIS C" = "C",
                           "AIS D" = "D")
            )
        )
} else if (data.set == "emsci") {
    injury_levels <- c(paste(rep("C", 8), 1:8, sep = ""),
                      paste(rep("T", 12), 1:12, sep = ""),
                      paste(rep("L", 5), 1:5, sep = ""),
                      paste(rep("S", 5), 1:5, sep = ""))
    AIS_grades <- c("A", "B", "C", "D", "E")
    data <- read_csv(paste(path.to.data,
                           "EMSCI/emsci_data_sygen_format.csv",
                           # "Sygen/JohnKramersProject_DATA_2019-10-07_0111.csv",
                           sep = ""),
                     col_types = list(ptid =  col_integer(),
                                      age = col_integer(),
                                      sexcd = col_factor(levels = c("1", # male
                                                                    "2") # female
                                      ),
                                      splvl = col_factor(levels = injury_levels),
                                      ais1 = col_factor(levels = AIS_grades),
                                      lower01 = col_integer(),
                                      lower52 = col_integer()),
                     na = c("", "NA", "INT") # INT - intermediate; cannot be
                     # resolved to NLI with information contained in EMSCI?
    )
} else {
    data <- NA
    print("Invalid data set identifier! Must be 'sygen' or 'emsci'.")
}

# remove patients with AIS E at baseline (as Sygen does not contain any
# patients with AIS E)
dim(data)
data <- data %>%
    filter(ais1 %in% c("A", "B", "C", "D", NA))
dim(data)

################################################################################
# Sourcing additional scripts
################################################################################

source("functions.R") # use relative path

################################################################################
# Prepare data - step 1 (visual inspection of the raw data)
################################################################################
data <- data %>%
    mutate(level = case_when(
        grepl('C', splvl, fixed = TRUE) ~ "cervical",
        grepl('T', splvl, fixed = TRUE) ~ "thoracic",
        grepl('L', splvl, fixed = TRUE) ~ "lumbar")
        ) %>%
    mutate(level = factor(level,
                          levels = c("cervical", "thoracic", "lumbar"))
           )

# remove patients with lumbar injury at baseline (as Sygen does not contain any
# patients with AIS E)
dim(data)
data <- data %>%
    filter(level %in% c("cervical", "thoracic", NA))
dim(data) # Sygen: n = 797 | EMSCI: 4944 (5220 if AIS E and lumbar injury
# ... at baseline are included)
head(data)

################################################################################
# Visualise raw data
################################################################################

## Visualise original missingness patterns
# ref for interpretation of the following plot:
# https://cran.r-project.org/web/packages/finalfit/vignettes/missing.html

data %>% # missing_pairs function part of finalfit library
  missing_pairs("lower52", c("age", "sexcd", "level", "lower01", "ais1"))

data %>%
  missing_pairs("lower52", c("age", "sexcd", "level", "lower01", "ais1"),
                position = "fill", )

# x-axis represent patients, light blue = NA
data %>%
  missing_plot()

# ------------------------------------------------------------------------------
## Visualise the distributions of the different variables

custom_theme <- theme(plot.title = element_text(hjust = 0.5))

# AIS grade, i.e. severity
severity <- ggplot(data, aes(ais1)) +
    custom_theme +
    geom_bar() +
    ggtitle("Injury severity")
severity

# Age
age <- ggplot(data, aes(x=age)) +
    custom_theme +
    geom_histogram(aes(y=..density..), binwidth = 5, # explicitly use 5yr bins
                   colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666") +
    ggtitle("Age")
age

# Sex
sex <- ggplot(data, aes(factor(sexcd))) +
    custom_theme +
    geom_bar() +
    scale_x_discrete(labels=c("1" = "Female", "2" = "Male"), name = 'Sex') +
    ggtitle("Sex")
sex

# Distribution of LEMS at baseline and week 52
lems01 <- ggplot(data, aes(x=lower01)) +
    custom_theme +
    geom_density(alpha=.2, fill="#FF6666") +
    xlab("") +
    xlim(0,50) +
    ylim(0, 0.075) +
    ggtitle("Lower extremity motor score (LEMS) at baseline")
lems52 <- ggplot(data, aes(x=lower52)) +
    custom_theme +
    geom_density(alpha=.2, fill="#FF6666") +
    xlab("LEMS") +
    xlim(0,50) +
    ylim(0, 0.075) +
    ggtitle("Lower extremity motor score (LEMS) at week 52")
lems <- ggarrange(lems01, lems52, ncol = 1, nrow = 2)
lems

# Level of injury
nli <- ggplot(data, aes(splvl)) +
    custom_theme +
    geom_bar() +
    scale_x_discrete(name = 'level') +
    ylab("") +
    ggtitle("Neurological level of injury")
nli

# Variables stratified by AIS grade
#ggplot(sygen_analysis, aes(x=lower01, fill=ais1)) + geom_density(alpha=.3)+ xlim(0,50)
#ggplot(sygen_analysis, aes(x=lower52, fill=ais1)) + geom_density(alpha=.3)+ xlim(0,50)
#ggplot(sygen_analysis, aes(x=age, fill=ais1)) + geom_density(alpha=.3)
#ggplot(sygen_analysis, aes(x=factor(sexcd), fill=ais1)) + geom_density(alpha=.3)
#ggplot(sygen_analysis, aes(x=splvl, fill=ais1)) + geom_histogram(stat="count")

# Relationship between LEMS at week 01 and week 52, coloured by AIS grade
lems_by_ais_scatter <- ggplot(data, aes(lower01, lower52, colour=ais1)) +
  geom_jitter(width = 2, height = 2) +
  xlim(-2,50) +
  ylim(-2,50)
lems_by_ais_scatter

# Change in LEMS distribution over time, by AIS grade
lems01_by_AIS <- ggplot(data, aes(x=lower01)) +
    geom_density(alpha=.2, fill="#FF6666") +
    xlim(0,50) +
    facet_grid(rows = vars(ais1), scales = 'fixed')
lems52_by_AIS <- ggplot(data, aes(x=lower52)) +
    geom_density(alpha=.2, fill="#FF6666") +
    xlim(0,50) +
    ylim(0.0, 0.2) +
    ylab("") +
    facet_grid(rows = vars(ais1), scales = 'fixed')
LEMS_by_AIS_distribution <- ggarrange(lems01_by_AIS, lems52_by_AIS,
                                      ncol = 2, nrow = 1)
LEMS_by_AIS_distribution

################################################################################
# Prepare data - step 2 (selecting only complete cases for simulation)
################################################################################

complete_cases <- data[complete.cases(data), ]
dim(complete_cases) # Sygen: n = 546 | EMSCI: n = 1171 (if only AIS E is
# excluded, o/w n = 1043 if also lumbar injuries are excluded)

## confirm that only complete cases are included
# complete_cases %>%
#  missing_plot()

################################################################################
# Introduce missingness in column of your choice with pattern of your choice
################################################################################

data_missing <- introduce_missingness(data = complete_cases,
                                      cols = c('ais1', 'lower01', 'lower52'),
                                      patterns = c('MCAR', 'MAR', 'MNAR'),
                                      prop = 0.3)

## integer-valued variables
cols.int <- c("age", "lower01", "lower01_MCAR", "lower01_MAR", "lower01_MNAR",
              "lower52", "lower52_MCAR", "lower52_MAR", "lower52_MNAR")
data_missing[cols.int] <- sapply(data_missing[cols.int],
                                 as.integer)

## Factor variables
cols.fac <- c("ais1", "splvl", "sexcd",
              "ais1_MCAR", "ais1_MAR", "ais1_MNAR", "level")
data_missing[cols.fac] <- sapply(data_missing[cols.fac],
                                 as.factor)

# ------------------------------------------------------------------------------
## Sanity checks
head(data_missing)
# Print column names
names(data_missing)
# Proportion of missingness introduced
sapply(data_missing,
       function(y) sum(length(which(is.na(y)))))
sapply(data_missing,
       function(col) sum(is.na(col))/length(col))
# Type of each column
sapply(data_missing, class)

################################################################################
# Evaluate the effect of different imputation strategies
################################################################################
if (data.set == "sygen") {
    # save_path <- paste(path.to.data, "Sygen/", sep = "")
    save_path <- "~/Desktop/"
} else if (data.set == "emsci") {
    # save_path <- paste(path.to.data, "EMSCI/", sep = "")
    save_path <- "~/Desktop/"
}

result_imputation_ais <- result_imputations(data = data_missing,
                                            var = 'ais1',
                                            patterns = c('MCAR', 'MAR', 'MNAR'),
                                            imp = c('case_deletion', 'majority', 'regression'))

result_imputation_lems01 <- result_imputations(data = data_missing,
                                               var = 'lower01',
                                               patterns = c('MCAR', 'MAR', 'MNAR'),
                                               imp = c('case_deletion', 'mean', 'regression'))

result_imputation_lems52 <- result_imputations(data = data_missing,
                                               var = 'lower52',
                                               patterns = c('MCAR', 'MAR', 'MNAR'),
                                               imp = c('case_deletion', 'mean', 'regression'))

plots_ais <- plot_imputation_results(results_data = result_imputation_ais[1][[1]],
                                     dist_data = result_imputation_ais[2][[1]],
                                     var = 'ais1',
                                     patterns = c('MCAR', 'MAR', 'MNAR'),
                                     imp = c('case_deletion', 'majority', 'regression'))

plots_ais[[1]]
add_joint_title(plots_ais[[1]],
                paste(toupper(data.set), ": missingness in AIS, week 1", sep = ""))
ggsave(paste(save_path, data.set, "_AIS-week-1_coefficients.pdf", sep = ""),
       height = 20, width = 20, units = "cm",
       device = "pdf")
plots_ais[[2]]
add_joint_title(plots_ais[[2]],
                paste(toupper(data.set), ": missingness in AIS, week 1", sep = ""))
ggsave(paste(save_path, data.set, "_AIS-week-1_pvalues.pdf", sep = ""),
       height = 20, width = 20, units = "cm",
       device = "pdf")

plots_lems01 <- plot_imputation_results(results_data = result_imputation_lems01[1][[1]],
                                        dist_data = result_imputation_lems01[2][[1]],
                                        var = 'lower01',
                                        patterns = c('MCAR', 'MAR', 'MNAR'),
                                        imp = c('case_deletion', 'mean', 'regression'))
plots_lems01[[1]]
add_joint_title(plots_lems01[[1]],
                paste(toupper(data.set), ": missingness in LEMS, week 1", sep = ""))
ggsave(paste(save_path, data.set, "_LEMS-week-1_coefficients.pdf", sep = ""),
       height = 20, width = 20, units = "cm",
       device = "pdf")
plots_lems01[[2]]
add_joint_title(plots_lems01[[2]],
                paste(toupper(data.set), ": missingness in LEMS, week 1", sep = ""))
ggsave(paste(save_path, data.set, "_LEMS-week-1_pvalues.pdf", sep = ""),
       height = 20, width = 20, units = "cm",
       device = "pdf")

plots_lems52 <- plot_imputation_results(results_data = result_imputation_lems52[1][[1]],
                                        dist_data = result_imputation_lems52[2][[1]],
                                        var = 'lower52',
                                        patterns = c('MCAR', 'MAR', 'MNAR'),
                                        imp = c('case_deletion', 'mean', 'regression'))
plots_lems52[[1]]
add_joint_title(plots_lems52[[1]],
                paste(toupper(data.set), ": missingness in LEMS, week 52", sep = ""))
ggsave(paste(save_path, data.set, "_LEMS-week-52_coefficients.pdf", sep = ""),
       height = 20, width = 20, units = "cm",
       device = "pdf")
plots_lems52[[2]]
add_joint_title(plots_lems52[[2]],
                paste(toupper(data.set), ": missingness in LEMS, week 52", sep = ""))
ggsave(paste(save_path, data.set, "_LEMS-week-52_pvalues.pdf", sep = ""),
       height = 20, width = 20, units = "cm",
       device = "pdf")
