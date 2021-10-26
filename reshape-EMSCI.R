################################################################################
# Louis Lukas
# created: 25 October 2021
################################################################################
# read and appropriately format EMSCI data
# Note: NT (not tested) for now treated as NA; in the future, this could be
#       changed as it might carry some information
################################################################################

library(tidyverse)
data_path <- "/Volumes/borgwardt/Data/SCI/EMSCI/"

emsci_raw <- read_csv(paste(data_path,
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
                          Cause = col_factor(),
                          ISNCSCI_TestDate = col_date(format = "%d.%m.%y"),
                          TestDate_Mot = col_date(format = "%d.%m.%y"),
                          TestDate_Sens = col_date(format = "%d.%m.%y"),
                          AIS = col_factor(),
                          AIScd = col_factor(),
                          NLI = col_factor(),
                          NLI_level = col_factor(),
                          plegia = col_factor(),
                          RSL = col_factor(),
                          LSL = col_factor(),
                          RML = col_factor(),
                          LML = col_factor(),
                          COMPL = col_factor(),
                          RSZPP = col_factor(),
                          LSZPP = col_factor(),
                          RMZPP = col_factor(),
                          LMZPP = col_factor(),
                          VAC = col_factor(),DAP = col_factor(),
                          SCIM23_TestDate = col_date(format = "%d.%m.%y"),
                          WISCI_TestDate = col_date(format = "%d.%m.%y"),
                          `6min_TestDate` = col_date(format = "%d.%m.%y"),
                          `10m_TestDate` = col_date(format = "%d.%m.%y"),
                          TUG_TestDate = col_date(format = "%d.%m.%y"),
                          SCIM2_TestDate = col_date(format = "%d.%m.%y"),
                          SCIM3_TestDate = col_date(format = "%d.%m.%y"),
                          .default = col_integer() # most score columns are not
                          # continuous so an integer value is more appropriate
                        ),
                      col_select = 1:235, # ignore columns 236 to 239 for now
                      # (YEARDOI_new2, 5_year_bins, 2_year_bins, 2_4_year_bins)
                      na = c("", "NA", "NT")
                      )

# convert DOI from 'weekday, month dd, yyyy' to a standard representation
emsci_raw <- emsci_raw %>%
    extract(DOI, c("weekday", "date"), "(.{3,7}day), (.+)") %>%
    select(-weekday) %>%
    mutate(date = as.Date(date, format = "%B %d, %Y"))

emsci_raw <- emsci_raw %>%
    mutate(across(where(is.factor), ~na_if(., "NT")))


# example patient
emsci_raw %>%
    filter(Patientennummer == 522034) %>%
    arrange(ExamStage, desc()) %>%
    View()

# map to format of Sygen data
# note that Sygen data is originally in wide form while EMSCI is already in long
# form; reshaped version of Sygen exists, see tidy-data branch)
# expected columns:
#   $ ptid   : chr
#   $ age    : num
#   $ sexcd  : Factor w/ 2 levels "1","2" (1 - female, 2 - male)
#   $ splvl  : Factor w/ 20 levels "C01","C02","C03",..
#   $ ais1   : Factor w/ 4 levels "AIS A","AIS B",..
#   $ lower01: num
#   $ lower52: num
#   $ level  : Factor w/ 2 levels "cervical","thoracic"
emsci_reduced <- emsci_raw %>%
    select(Patientennummer,
           AgeAtDOI,
           Sexcd, # 0 - male, 1 - female
           ExamStage_weeks,
           NLI,
           AIS, # only coded as: A, B, C, D
           LEMS
           ) %>%
    filter(ExamStage_weeks %in% c(2, 52)) %>%
    mutate(Sexcd = case_when(Sexcd == 0 ~ 2,
                             Sexcd == 1 ~ 1
                             )
           ) %>%
    rename(ptid = Patientennummer,
           age = AgeAtDOI,
           sexcd = Sexcd,
           splvl = NLI
           )

emsci_lems_wide <- emsci_reduced %>%
    select(ptid,
           ExamStage_weeks,
           AIS,
           LEMS) %>%
    pivot_wider(names_from = ExamStage_weeks,
                values_from = c(AIS, LEMS))

emsci_reformatted <- emsci_reduced %>%
    filter(ExamStage_weeks == 2) %>%
    select(ptid, age, sexcd, splvl) %>%
    inner_join(emsci_lems_wide, by = "ptid") %>%
    select(ptid, age, sexcd, splvl, AIS_2, LEMS_2, LEMS_52) %>%
    rename(ais1 = AIS_2, # note the mismatch introduced between the labels
           lower01 = LEMS_2, # originating from Sygen (ais1, lower01) where
           lower52 = LEMS_52) %>% # baseline assessments were presumably
    arrange(ptid, desc()) # completed within the first week while for EMSCI
    # this assessment needs to be completed within the first two weeks

# fill empty values w/ NAs
emsci_reformatted <- emsci_reformatted %>%
    mutate(across(c(splvl, ais1)), na_if(., ""))

# save
write_csv(emsci_reformatted,
          paste(data_path,
                "emsci_data_sygen_format.csv",
                sep = ""))
