################################################################################
# Louis Lukas
# created: 25 October 2021
################################################################################
# read and appropriately format EMSCI data
# Note: NT (not tested) for now treated as NA; in the future, this could be
#       changed as it might carry some information
################################################################################

library(tidyverse)
data_path <- "../../data/SCI/"

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
                                                          "chronic")),
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
                        VAC = col_factor(),
                        DAP = col_factor(),
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
view(head(emsci_raw))

# example patient
emsci_raw %>%
    filter(Patientennummer == 522034) %>%
    arrange(ExamStage, desc())
