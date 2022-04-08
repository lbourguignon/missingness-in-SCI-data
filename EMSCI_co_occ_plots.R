################################################################################
# SCI - Handling missing data project - EMSCI
# L. Bourguignon & L.P. Lukas
# First version : 08.11.2021
# Last update : 22.11.2021
################################################################################

################################################################################
# Packages and seed
################################################################################

#install.packages("finalfit")
library(finalfit)
library(tidyr)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(reshape2)
library(networkD3)

set.seed(1)

################################################################################
# Data loading
################################################################################

data_path = '/Volumes/borgwardt/Data/SCI/'
emsci_raw <- read.csv(paste(data_path, 'EMSCI/emsci_data_2020.csv', sep = ''))

################################################################################
# Functions
################################################################################

plot_miss_upset <- function(data, cols){
    df_temp <- subset_col(cols, data)
    # requires library(naniar)
    plot <- gg_miss_upset(df_temp, nsets = n_var_miss(df_temp))

    return (plot)
}

# Function to subset columns in vec from dataframe df
# Output is a dataframe
subset_col <- function(vec, df){
  return(df[vec])
}

# Function to add string week to all elements of vec as pre- or suffix
# based on order (take values 'before' or 'after')
# Output is a vector
vector_var <- function(vec, week, order){
  if (order == 'before'){
    return(paste(week, vec, sep=""))
  }
  if (order == 'after'){
    return(paste(vec, week, sep=""))
  }
}

################################################################################
# Initial visualisation of the data
################################################################################

# Create a dataframe with the variables used in the imputation simulation
emsci_analysis <- data.frame(matrix(ncol = 7, nrow = 0))
columns <- c('ptid', 'age', 'sexcd', 'splvl', 'ais1', 'lower01', 'lower52')
colnames(emsci_analysis) <- columns
for (pat in levels(factor(emsci_raw$Patientennummer))){
  age <- with(emsci_raw, AgeAtDOI[Patientennummer %in% pat & ExamStage %in% 'very acute'])
  sex <- with(emsci_raw, Sexcd[Patientennummer %in% pat & ExamStage %in% 'very acute'])
  lvl <- with(emsci_raw, NLI[Patientennummer %in% pat & ExamStage %in% 'very acute'])
  ais1 <- with(emsci_raw, AIS[Patientennummer %in% pat & ExamStage %in% 'very acute'])
  lower1 <- with(emsci_raw, LEMS[Patientennummer %in% pat & ExamStage %in% 'very acute'])
  lower52 <- with(emsci_raw, LEMS[Patientennummer %in% pat & ExamStage %in% 'chronic'])
  vec_pat <- c(pat, age, sex, lvl, ais1, lower1, lower52)
  emsci_analysis[nrow(emsci_analysis) + 1, ] = vec_pat
}
# Replace missing entries by NA
emsci_analysis[emsci_analysis == ''] <- NA

# Harmonise the type of variables per column
## Numeric variables
cols.num <- c("age", "lower01", "lower52")
emsci_analysis[cols.num] <- sapply(emsci_analysis[cols.num], as.numeric)
## Factor variables
cols.fac <- c("ais1", "splvl", "sexcd")
emsci_analysis[cols.fac] <- lapply(emsci_analysis[cols.fac], factor)

# Initial plots describing the raw data

# AIS grade distribution at baseline
ggplot(emsci_analysis, aes(ais1)) + geom_bar()

# Age distribution at baseline
ggplot(emsci_analysis, aes(x=age)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")
# Age distribution at basline, stratified by AIS grade at baseline
ggplot(emsci_analysis, aes(x=age, fill=ais1)) + geom_density(alpha=.3)

# Sex distribution at baseline
ggplot(emsci_analysis, aes(factor(sexcd))) + geom_bar() +
  scale_x_discrete(labels=c("1" = "Female", "0" = "Male"), name = 'Sex')
# Sex distribution at baseline, stratified by AIS grade at baseline
ggplot(emsci_analysis, aes(x=factor(sexcd), fill=ais1)) + geom_density(alpha=.3)

# Level of injury distribution at baseline
ggplot(emsci_analysis, aes(splvl)) + geom_bar() +
  scale_x_discrete(name = 'Level of injury')
# Level of injury distribution at baseline, stratified by AIS grade at baseline
ggplot(emsci_analysis, aes(x=splvl, fill=ais1)) + geom_histogram(stat="count")

# LEMS distribution at baseline VS at recovery, per AIS grade at baseline
# baseline (lower01)
p1 <- ggplot(emsci_analysis, aes(x=lower01)) + geom_density(alpha=.2, fill="#FF6666") +
  xlim(0,50) +
  facet_grid(rows = vars(ais1), scales = 'free')
# recovery (lower52)
p2 <- ggplot(emsci_analysis, aes(x=lower52)) + geom_density(alpha=.2, fill="#FF6666") +
  xlim(0,50) +
  facet_grid(rows = vars(ais1), scales = 'free')
# combined plot with baseline and recovery LEMS distributions per AIS grade
figure <- ggarrange(p1, p2, ncol = 2, nrow = 1)
figure

# LEMS distribution at baseline per AIS grade
ggplot(emsci_analysis, aes(x=lower01, fill=ais1)) + geom_density(alpha=.3)+ xlim(0,50)
# LEMS distribution at recovery (week52) per AIS grade
ggplot(emsci_analysis, aes(x=lower52, fill=ais1)) + geom_density(alpha=.3)+ xlim(0,50)

# LEMS at baseline VS LEMS at recovery, colored per AIS grade
ggplot(emsci_analysis, aes(lower01, lower52, colour=ais1)) +
  geom_jitter(width = 2, height = 2) +
  xlim(-2,50) +
  ylim(-2,50)

# Visualisation of missingness in the raw data
# ref for interpretation of the folloming plot:
# https://cran.r-project.org/web/packages/finalfit/vignettes/missing.html

explanatory = c("age", "sexcd", "splvl", "lower01", "ais1")
dependent = "lower52"

emsci_analysis %>%
  missing_pairs(dependent, explanatory)
emsci_analysis %>%
  missing_pairs(dependent, explanatory, position = "fill", )

# Heatmap visualising missingness per patient across variables
# x-axis represent patients
# light blue = NA
emsci_analysis %>%
  missing_plot()

# Heatmap representing missingness per patient across baseline variables
# x-axis: ID, age, sex, NLI, AIS grade at baseline and LEMS at baseline and recovery
# y-axis: one line is one patient (in total: n>5000)
# light gray: value is present; dark gray: value is missing
emsci_analysis_plot <- emsci_analysis %>%
  rename(sex = sexcd,
         ID = ptid,
         AIS_baseline = ais1,
         level_of_injury = splvl,
         LEMS_baseline = lower01,
         LEMS_52weeks = lower52)
#tiff("baseline_plot.tiff", units="in", width=8.5, height=5, res=300)
vis_miss(emsci_analysis_plot) # requires library(visdat)
#dev.off()


################################################################################
# Visualise the co-occurrence of missingness in raw data
################################################################################

# Create a dataframe with the AIS grades and LEMS evaluated over time
# Additionally: VAC and DAP are included
emsci_lems_ais_overtime <- data.frame(matrix(ncol = 13, nrow = 0))
columns <- c('ptid', 'ais2', 'ais4', 'ais12', 'ais26', 'ais52',
             'lower02', 'lower04', 'lower12', 'lower26', 'lower52', 'VAC', 'DAP')
colnames(emsci_lems_ais_overtime) <- columns
for (pat in levels(factor(emsci_raw$Patientennummer))){
  ais2 <- with(emsci_raw, AIS[Patientennummer %in% pat & ExamStage %in% 'very acute'])
  ais4 <- with(emsci_raw, AIS[Patientennummer %in% pat & ExamStage %in% 'acute I'])
  ais12 <- with(emsci_raw, AIS[Patientennummer %in% pat & ExamStage %in% 'acute II'])
  ais26 <- with(emsci_raw, AIS[Patientennummer %in% pat & ExamStage %in% 'acute III'])
  ais52 <- with(emsci_raw, AIS[Patientennummer %in% pat & ExamStage %in% 'chronic'])
  lower02 <- with(emsci_raw, LEMS[Patientennummer %in% pat & ExamStage %in% 'very acute'])
  lower04 <- with(emsci_raw, LEMS[Patientennummer %in% pat & ExamStage %in% 'acute I'])
  lower12 <- with(emsci_raw, LEMS[Patientennummer %in% pat & ExamStage %in% 'acute II'])
  lower26 <- with(emsci_raw, LEMS[Patientennummer %in% pat & ExamStage %in% 'acute III'])
  lower52 <- with(emsci_raw, LEMS[Patientennummer %in% pat & ExamStage %in% 'chronic'])
  VAC <- with(emsci_raw, VAC[Patientennummer %in% pat & ExamStage %in% 'very acute'])
  DAP <- with(emsci_raw, DAP[Patientennummer %in% pat & ExamStage %in% 'very acute'])
  vec_pat <- c(pat, ais2, ais4, ais12, ais26, ais52,
               lower02, lower04, lower12, lower26, lower52, VAC, DAP)
  emsci_lems_ais_overtime[nrow(emsci_lems_ais_overtime) + 1, ] = vec_pat
}
# Replace all the empty entries by NA
# LL: what about NT and INT?
emsci_lems_ais_overtime[emsci_lems_ais_overtime == ''] <- NA

# -------------------------

# Define vectors with all LEMS and AIS grades variable names
vec_lems <- c('lower02', 'lower04', 'lower12', 'lower26', 'lower52') # all LEMS
vec_ais <- c('ais2', 'ais4', 'ais12', 'ais26', 'ais52') # all AIS grades

# -------------------------

# Heatmap representing missingness per patient across variables
# x-axis: AIS grade and LEMS evaluated overtime + VAP and DAP
# y-axis: one line is one patient (in total: n>5000)
# light gray: value is present; dark gray: value is missing
vis_miss(emsci_lems_ais_overtime)

# -------------------------
# Co-occurrence of missingness in AIS grade and LEMS
# -------------------------

# LEMS dataframe and overview heatmap
emsci_lems_overtime <- subset_col(c('lower02', 'lower04', 'lower12', 'lower26', 'lower52'),
                                  emsci_lems_ais_overtime)
emsci_lems_overtime %>%
  missing_plot()
vis_miss(emsci_lems_overtime)

# AIS grade dataframe and overview heatmap
emsci_ais_overtime <- subset_col(c('ais2', 'ais4', 'ais12', 'ais26', 'ais52'),
                                 emsci_lems_ais_overtime)
emsci_ais_overtime %>%
  missing_plot()
vis_miss(emsci_ais_overtime)

# co-occurring NAs among AIS measurements over time
miss_upset_ais_overtime <- plot_miss_upset(emsci_lems_ais_overtime, vec_ais)
miss_upset_ais_overtime
# co-occurring NAs among AIS measurements over time, for patients with AIS A at baseline
miss_upset_ais_overtime_A <- plot_miss_upset(emsci_lems_ais_overtime[emsci_lems_ais_overtime$ais2 == 'A', ],
                                             vec_ais)
miss_upset_ais_overtime_A
# co-occurring NAs among LEMS measurements over time
miss_upset_lems_overtime <- plot_miss_upset(emsci_lems_ais_overtime, vec_lems)
miss_upset_lems_overtime
# co-occurring NAs among LEMS and AIS measurements over time
#tiff("ais_lems_upset.tiff", units="in", width=8, height=5, res=300)
miss_upset_lems_ais_overtime <- plot_miss_upset(emsci_lems_ais_overtime,
                                                c(vec_lems, vec_ais))
miss_upset_lems_ais_overtime
#dev.off()

# -------------------------
# Co-occurrence of missingness at myotome and dermatome levels
# -------------------------

# Extract all the rows containing information at very acute stage
emsci_very_acute <- emsci_raw[emsci_raw$ExamStage == 'very acute',]
emsci_very_acute[emsci_very_acute == ''] <- NA
# Extract all the rows containing information at chronic stage
emsci_chronic <- emsci_raw[emsci_raw$ExamStage == 'chronic',]
emsci_chronic[emsci_chronic == ''] <- NA

# Define the levels evaluated for motor and sensory scores
levels_mot_emsci <- c('C5', 'C6', 'C7', 'C8', 'T1', 'L2', 'L3', 'L4', 'L5', 'S1')
levels_sens_emsci <- c('C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8',
                       'T1', 'T2', 'T3', 'T4', 'T5', 'T6', 'T7', 'T8', 'T9', 'T10', 'T11', 'T12',
                       'L1', 'L2', 'L3', 'L4', 'L5',
                       'S1', 'S2', 'S3', 'S45')
# Generate vectors matching the variable names of myotome and dermatome level scores
# Vector with all variable names for motor score assessment
vec_mot_emsci <- c(vector_var('RMS_', levels_mot_emsci, 'after'), vector_var('LMS_', levels_mot_emsci, 'after'))
# Vector with all variable names for pinprick assessment
vec_pp_emsci <- c(vector_var('RPP_', levels_mot_emsci, 'after'), vector_var('LPP_', levels_mot_emsci, 'after'))
# Vector with all variable names for light touch assessment
vec_lt_emsci <- c(vector_var('RLT_', levels_mot_emsci, 'after'), vector_var('LLT_', levels_mot_emsci, 'after'))

# co-occurring NAs among myotomes at week 00
miss_upset_myotomes_00 <- plot_miss_upset(emsci_very_acute, vec_mot_emsci)
miss_upset_myotomes_00
# co-occurring NAs among myotomes at week 52
miss_upset_myotomes_52 <- plot_miss_upset(emsci_chronic, vec_mot_emsci)
miss_upset_myotomes_52
# co-occurring NAs among pin prink dermatomes at week 01
miss_upset_pinprick_01 <- plot_miss_upset(emsci_very_acute, vec_pp_emsci) # always all missing together
miss_upset_pinprick_01
# co-occurring NAs among pin prink dermatomes at week 52
miss_upset_pinprick_52 <- plot_miss_upset(emsci_chronic, vec_pp_emsci)
miss_upset_pinprick_52
# co-occurring NAs among light touch dermatomes at week 00
miss_upset_lighttouch_01 <- plot_miss_upset(emsci_very_acute, vec_lt_emsci)
miss_upset_lighttouch_01
# co-occurring NAs among light touch dermatomes at week 52
miss_upset_lighttouch_52 <- plot_miss_upset(emsci_chronic, vec_lt_emsci)
miss_upset_lighttouch_52

# -------------------------
# Co-occurrence of missingness in functional scores
# -------------------------

# co-occurring NAs among functional scores at week 02 (baseline/very acute)
miss_upset_funct_00 <- plot_miss_upset(emsci_very_acute,
                                       c('SCIM23_TotalScore',
                                         'SCIM3_TotalScore',
                                         'WISCI', 'X6min', 'X10m', 'TUG'))
miss_upset_funct_00
# co-occurring NAs among functional scores at week 52 (chronic)
miss_upset_funct_52 <- plot_miss_upset(emsci_chronic,
                                       c('SCIM23_TotalScore',
                                         'SCIM3_TotalScore',
                                         'WISCI', 'X6min', 'X10m', 'TUG'))
miss_upset_funct_52

# Closer look at missingness in SCIM scores overtime
## Define the dataframe containing SCIM scores overtime
emsci_scim_overtime <- data.frame(matrix(ncol = 6, nrow = 0))
columns <- c('ptid', 'SCIM2', 'SCIM4', 'SCIM12', 'SCIM26', 'SCIM52')
colnames(emsci_scim_overtime) <- columns
for (pat in levels(factor(emsci_raw$Patientennummer))){
  SCIM2 <- with(emsci_raw, SCIM23_TotalScore[Patientennummer %in% pat & ExamStage %in% 'very acute'])
  SCIM4 <- with(emsci_raw, SCIM23_TotalScore[Patientennummer %in% pat & ExamStage %in% 'acute I'])
  SCIM12 <- with(emsci_raw, SCIM23_TotalScore[Patientennummer %in% pat & ExamStage %in% 'acute II'])
  SCIM26 <- with(emsci_raw, SCIM23_TotalScore[Patientennummer %in% pat & ExamStage %in% 'acute III'])
  SCIM52 <- with(emsci_raw, SCIM23_TotalScore[Patientennummer %in% pat & ExamStage %in% 'chronic'])
  vec_pat <- c(pat, SCIM2, SCIM4, SCIM12, SCIM26, SCIM52)
  emsci_scim_overtime[nrow(emsci_scim_overtime) + 1, ] = vec_pat
}
# co-occurring NAs in SCIM score overtime
#tiff("test.tiff", units="in", width=8, height=5, res=300)
miss_upset_SCIM <- plot_miss_upset(emsci_scim_overtime,
                                   c('SCIM2', 'SCIM4', 'SCIM12', 'SCIM26', 'SCIM52'))
miss_upset_SCIM
#dev.off()

# -------------------------
# Sankey plot : transition in AIS grades from week 01 to week 26
# -------------------------

# Renaming the variable values such that they include information on the week when it was collected
emsci_ais_overtime$ais1 <- paste(emsci_ais_overtime$ais1, "_1", sep="")
emsci_ais_overtime$ais4 <- paste(emsci_ais_overtime$ais4, "_4", sep="")
emsci_ais_overtime$ais8 <- paste(emsci_ais_overtime$ais8, "_8", sep="")
emsci_ais_overtime$ais26 <- paste(emsci_ais_overtime$ais26, "_26", sep="")
emsci_ais_overtime$ais52 <- paste(emsci_ais_overtime$ais52, "_52", sep="")

# Create a table with :
# source (i.e. AIS grade at week 1),
# target (i.e. AIS grade at week 52)
# value (i.e. number of patients go from one category to another from week 1 to 52)
transition1_52 <- emsci_ais_overtime %>%
  group_by(ais1) %>%
  count(ais52)
transition1_52 <- as.data.frame(transition1_52)
names(transition1_52) <- c('source', 'target', 'value')

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(transition1_52$source),
         as.character(transition1_52$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
transition1_52$IDsource <- match(transition1_52$source, nodes$name)-1
transition1_52$IDtarget <- match(transition1_52$target, nodes$name)-1

# Make the Network
sankey_transition_1_52 <- sankeyNetwork(Links = transition1_52, Nodes = nodes,
                                        Source = "IDsource", Target = "IDtarget",
                                        Value = "value", NodeID = "name",
                                        sinksRight=T, iterations = 0)
sankey_transition_1_52

# -------------------------
# LEMS missingness pattern per patient
# -------------------------

# Recode the data: 1 is NA, 0 is value available
emsci_lems_overtime_binary <- emsci_lems_overtime
emsci_lems_overtime_binary[!(is.na(emsci_lems_overtime_binary))] <- -1 # temporary
emsci_lems_overtime_binary[(is.na(emsci_lems_overtime_binary))] <- 1
emsci_lems_overtime_binary[emsci_lems_overtime_binary==-1] <- 0

# Add information about severity grade at week 01 to the binarised lems dataframe
emsci_lems_overtime_binary_ais1 <- emsci_lems_overtime_binary
emsci_lems_overtime_binary_ais1$ais1 <- emsci_lems_ais_overtime$ais1

emsci_lems_overtime_binary_ais1[(is.na(emsci_lems_overtime_binary_ais1))] <- 'NA'

# Transform lems info overtime to patterns representing sequence of LEMS measurements
# e.g. 1-0-0-0-0 means LEMS is missing at week 02 but present for all following time points
lems_na_pattern_ais1 <- emsci_lems_overtime_binary_ais1 %>%
  group_by(lower02, lower04, lower12, lower26, lower52, ais1) %>%
  count()
# Remove AIS E category
lems_na_pattern_ais1 <- lems_na_pattern_ais1[lems_na_pattern_ais1$ais1 != 'E',]
# Introduce column summarising the pattern of missingness overtime
lems_na_pattern_ais1$pattern <- apply( lems_na_pattern_ais1[ , c('lower02', 'lower04', 'lower12', 'lower26', 'lower52') ] , 1 , paste , collapse = "-" )
# Introduce column with percentage of each pattern, stratified per AIS grade
lems_na_pattern_ais1_percent <- lems_na_pattern_ais1 %>% group_by(ais1) %>% mutate(Percentage = prop.table(n)*100)

# Plot heatmap: number of patients per missing pattern per AIS grade
#tiff("heatmap.tiff", units="in", width=8, height=5, res=400)
ggplot(lems_na_pattern_ais1_percent, aes(x=ais1, y=pattern, fill=Percentage)) +
  geom_tile() +
  geom_text(aes(label = round(Percentage, digits=3), color='white'))
#dev.off()

