library(tidyverse)
library(finalfit)
library(visdat)
library(naniar)
library(UpSetR)
library(networkD3)

set.seed(17)
AIS_grades <- c("A", "B", "C", "D", "E")
injury_levels <- c(paste(rep("C", 8), 1:8, sep = ""),
                   paste(rep("T", 12), 1:12, sep = ""),
                   paste(rep("L", 5), 1:5, sep = ""),
                   paste(rep("S", 5), 1:5, sep = ""))
################################################################################
data.path = '/Volumes/borgwardt/Data/SCI/EMSCI/'
# data.path = "~/data/SCI/EMSCI/"
data.raw <- read_csv(paste(data.path,
                           "emsci_data_2020.csv",
                           sep = ""),
                     col_types = list(
                         Patientennummer = col_integer(),
                         Center = col_factor(),
                         Country = col_factor(),
                         ExamStage = col_factor(levels = c("very acute",
                                                           "acute I",
                                                           "acute II",
                                                           "acute III",
                                                           "chronic")
                         ),
                         ExamStage_weeks = col_integer(),
                         DOI = col_character(),
                         YEARDOI = col_integer(),
                         Sex = col_factor(),
                         Sexcd = col_factor(),
                         AgeAtDOI = col_integer(),
                         AIS = col_factor(levels = AIS_grades),
                         NLI = col_factor(levels = injury_levels),
                         NLI_level = col_factor(),
                         VAC = col_factor(),
                         DAP = col_factor()),
                     na = c("", "NA", "NT", "INT")
)

# convert data to wide
data.subset <- data.raw %>%
    select(Patientennummer,
           AgeAtDOI,
           Sexcd,
           ExamStage_weeks,
           NLI,
           AIS,
           LEMS,
           SCIM23_TotalScore)
data.subset.wide <- data.subset %>%
    pivot_wider(names_from = ExamStage_weeks,
                values_from = c(NLI, AIS, LEMS, SCIM23_TotalScore)) %>%
    relocate(AIS_26, .after = AIS_12) %>%
    relocate(LEMS_26, .after = LEMS_12) %>%
    select(Patientennummer, AgeAtDOI, Sexcd, NLI_2, starts_with("AIS"),
           starts_with("LEMS"), starts_with("SCIM23"))
################################################################################
ggplot(data.subset.wide, aes(AIS_2)) +
    geom_bar()
ggplot(data.subset.wide, aes(x=AgeAtDOI)) +
    geom_histogram(aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666")
ggplot(data.subset.wide, aes(x=AgeAtDOI, fill=AIS_2)) +
    geom_density(alpha=.3)
ggplot(data.subset.wide, aes(factor(Sexcd))) +
    geom_bar() +
    scale_x_discrete(labels=c("1" = "Female", "0" = "Male"), name = 'Sex')
################################################################################
explanatory = c("AgeAtDOI", "Sexcd", "NLI_2", "LEMS_2", "AIS_2")
dependent = "LEMS_52"
data.subset.wide %>%
    missing_pairs(dependent, explanatory)
data.subset.wide %>%
    missing_pairs(dependent, explanatory, position = "fill")
data.subset.wide %>%
    missing_plot()
# Heatmap representing missingness per patient across baseline variables
# x-axis: ID, age, sex, NLI, AIS grade at baseline and LEMS at baseline and recovery
# y-axis: one line is one patient (in total: n > 5000)
# light gray: value is present; dark gray: value is missing
data.subset.wide %>%
    rename(sex = Sexcd,
           ID = Patientennummer,
           AIS_baseline = AIS_2,
           level_of_injury = NLI_2,
           LEMS_baseline = LEMS_2,
           LEMS_52weeks = LEMS_52) %>%
    vis_miss() +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0))
# LEMS over time
data.subset.wide %>%
    select(starts_with("LEMS_")) %>%
    vis_miss() +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0))
# AIS over time
data.subset.wide %>%
    select(starts_with("AIS_")) %>%
    vis_miss() +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0))
########################### Upset plots ########################################
# ... to visualise co-occurence of missingness
# start w/ AIS grade and LEMS over time
data.subset.wide %>%
    select(Patientennummer, starts_with("AIS_")) %>%
    gg_miss_upset(sets = paste(c("AIS_52", "AIS_26", "AIS_12", "AIS_4", "AIS_2"),
                               rep("NA", 5),
                               sep = "_"),
                  keep.order = TRUE)
data.subset.wide %>%
    select(Patientennummer, starts_with("LEMS_")) %>%
    gg_miss_upset(sets = paste(c("LEMS_52", "LEMS_26", "LEMS_12", "LEMS_4", "LEMS_2"),
                               rep("NA", 5),
                               sep = "_"),
                  keep.order = TRUE
                  )
######## check myotome and dermatome level scores ########
motor.levels <- c(paste(rep("C", 4), 5:8, sep = ""),
                  "T1",
                  paste(rep("L", 4), 2:5, sep = ""),
                  "S1")
sensory.levels <- c(paste(rep("C", 7), 2:8, sep = ""),
                    paste(rep("T", 12), 1:12, sep = ""),
                    paste(rep("L", 5), 1:5, sep = ""),
                    paste(rep("S", 3), 1:3, sep = ""), "S45")
motor.level.assessments <- c(paste(rep("RMS", length(motor.levels)), motor.levels, sep="_"),
                             paste(rep("LMS", length(motor.levels)), motor.levels, sep="_"))
pp.level.assessments <- c(paste(rep("RPP", length(motor.levels)), motor.levels, sep="_"),
                          paste(rep("LPP", length(motor.levels)), motor.levels, sep="_"))
lt.level.assessments <- c(paste(rep("RLT", length(motor.levels)), motor.levels, sep="_"),
                          paste(rep("LLT", length(motor.levels)), motor.levels, sep="_"))
#### ... at baseline ####
# myotomes (motor scores)
data.raw %>%
    filter(ExamStage_weeks == 2) %>%
    select(all_of(motor.level.assessments)) %>%
    as_shadow_upset() %>%
    upset(sets = rev(paste(motor.level.assessments, "NA", sep = "_")),
          keep.order = TRUE,
          order.by = "freq"
    )
# dermatomes
# -> pin prick
data.raw %>%
    filter(ExamStage_weeks == 2) %>%
    select(all_of(pp.level.assessments)) %>%
    as_shadow_upset() %>%
    upset(sets = rev(paste(pp.level.assessments, "NA", sep = "_")),
          keep.order = TRUE,
          order.by = "freq")
# -> light touch
data.raw %>%
    filter(ExamStage_weeks == 2) %>%
    select(all_of(lt.level.assessments)) %>%
    as_shadow_upset() %>%
    upset(sets = rev(paste(lt.level.assessments, "NA", sep = "_")),
          keep.order = TRUE,
          order.by = "freq")
#### ... and at week 52 ####
# myotomes (motor scores)
data.raw %>%
    filter(ExamStage_weeks == 52) %>%
    select(all_of(motor.level.assessments)) %>%
    as_shadow_upset() %>%
    upset(sets = rev(paste(motor.level.assessments, "NA", sep = "_")),
          keep.order = TRUE,
          order.by = "freq"
    )
# dermatomes
# -> pin prick
data.raw %>%
    filter(ExamStage_weeks == 52) %>%
    select(all_of(pp.level.assessments)) %>%
    as_shadow_upset() %>%
    upset(sets = rev(paste(pp.level.assessments, "NA", sep = "_")),
          keep.order = TRUE,
          order.by = "freq")
# -> light touch
data.raw %>%
    filter(ExamStage_weeks == 52) %>%
    select(all_of(lt.level.assessments)) %>%
    as_shadow_upset() %>%
    upset(sets = rev(paste(lt.level.assessments, "NA", sep = "_")),
          keep.order = TRUE,
          order.by = "freq")

######## check functional scores ########
functional.scores <- c('SCIM23_TotalScore', 'SCIM3_TotalScore', 'WISCI', '6min',
                       '10m', 'TUG')
# ... at baseline
data.raw %>%
    filter(ExamStage_weeks == 2) %>%
    select(all_of(functional.scores)) %>%
    as_shadow_upset() %>%
    upset(sets = rev(paste(functional.scores, "NA", sep = "_")),
          keep.order = TRUE,
          order.by = "freq")
# ... at week 52
data.raw %>%
    filter(ExamStage_weeks == 52) %>%
    select(all_of(functional.scores)) %>%
    as_shadow_upset() %>%
    upset(sets = rev(paste(functional.scores, "NA", sep = "_")),
          keep.order = TRUE,
          order.by = "freq")
# SCIM scores over time
scim.weeks <- paste(rep("SCIM23_TotalScore", 5), c(2, 4, 12, 26, 52), sep = "_")
data.subset.wide %>%
    select(Patientennummer, starts_with("SCIM23")) %>%
    gg_miss_upset(sets = rev(paste(scim.weeks, rep("NA", 5), sep = "_")),
                  keep.order = TRUE)

########################### Sankey plot ########################################
source.values <- data.subset %>%
    filter(ExamStage_weeks == 2) %>%
    distinct(AIS) %>%
    rename(AIS_source = AIS)
target.values <- data.subset %>%
    filter(ExamStage_weeks == 52) %>%
    distinct(AIS) %>%
    rename(AIS_target = AIS)
transitions <- data.subset.wide %>%
    nest_by(AIS_2, AIS_52) %>%
    mutate(n = nrow(data)) %>%
    select(-data) %>%
    rename(source = AIS_2, target = AIS_52) %>%
    mutate(source = paste(source, "2", sep = "_")) %>%
    mutate(target = paste(target, "52", sep = "_")) %>%
    as.data.frame()
nodes <- transitions %>%
    select(source) %>%
    distinct() %>%
    rename(name = source) %>%
    add_row(transitions %>%
                select(target) %>%
                distinct() %>%
                rename(name = target)
            ) %>%
    mutate(id = 0:(n() - 1))
transitions <- transitions  %>%
    left_join(nodes, by = c("source" = "name")) %>%
    rename(source.id = id) %>%
    left_join(nodes, by = c("target" = "name")) %>%
    rename(target.id = id)
sankeyNetwork(Links = transitions, Nodes = nodes,
              Source = "source.id", Target = "target.id",
              Value = "n", NodeID = "name",
              sinksRight = TRUE, iterations = 0)
