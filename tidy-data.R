# ##############################################################################
# Louis Lukas
# created: 02 August 2021
# ##############################################################################
#
# ##############################################################################
# "Convert" Sygen data to adhere to the tidy definition of data (long form).
# Mostly requires converting attributes w/ time component into separate rows.
# ##############################################################################
library(tidyverse)
data_path <- "../data/sygen/"
ais_codes <- c("AIS A", "AIS B", "AIS C", "AIS D")

#### read data ####
# ... and recode variables
sygen_extended <- read_csv(paste(data_path,
                                 "JohnKramersProject_DATA_2019-10-07_0111.csv",
                                 sep=""),
                           col_types = cols_only(
                             ptid = col_character(),
                             sexcd = col_factor(c("1", "2")),
                             age = col_integer(),
                             ais1 = col_factor(ais_codes),
                             ais4 = col_factor(ais_codes),
                             ais8 = col_factor(ais_codes), 
                             ais16 = col_factor(ais_codes),
                             ais26 = col_factor(ais_codes),
                             ais52 = col_factor(ais_codes),
                             splvl = col_character(),
                             lower01 = col_integer(),
                             lower04 = col_integer(),
                             lower08 = col_integer(),
                             lower16 = col_integer(),
                             lower26 = col_integer(),
                             lower52 = col_integer(),
                             death = col_factor(c("0", "1"))) # coding: 0 - no, 1 - yes
                           )
sygen_extended$sexcd <- recode(sygen_extended$sexcd, 
                               "1" = "female", 
                               "2" = "male")
# ais1_NAs <- sygen_extended %>%
#   filter(is.na(ais1)) %>%
#   select(ptid)
sygen_extended <- sygen_extended %>% 
  mutate(
    across(starts_with("ais"),
           ~recode(., 
                   "AIS A" = "A", 
                   "AIS B" = "B", 
                   "AIS C" = "C", 
                   "AIS D" = "D")
           )
    )
# ais1_recoded_NAs <- sygen_extended %>%
#   filter(is.na(ais1)) %>%
#   select(ptid)
# all_equal(ais1_NAs, ais1_recoded_NAs)
sygen_extended <- sygen_extended %>% 
  mutate(
    level = case_when(
      str_detect(splvl, "C") ~ "C",
      str_detect(splvl, "T") ~ "T"),
    level = factor(level, levels = c("C", "T"))  
  )

#### tidy data ####
# get data into a tidy format (one row per observation):
#   i. split ais1, ais4, ais8, ... into ASIA_scale and week columns
#      (similarly for lower01, lower04, ...)
sygen_tidy <- sygen_extended %>%
  select(-starts_with("lower")) %>%
  pivot_longer(cols = c(ais1, ais4, ais8, ais16, ais26, ais52),
               names_to = "week",
               values_to = "ASIA_scale")
sygen_tidy$week <- as.factor(recode(sygen_tidy$week,
                                    "ais1" = 1,
                                    "ais4" = 4,
                                    "ais8" = 8,
                                    "ais16" = 16,
                                    "ais26" = 26,
                                    "ais52" = 52))
sygen_lower_pivot <- sygen_extended %>% 
  select(ptid, starts_with("lower")) %>%
  pivot_longer(cols = c(lower01, lower04, lower08, lower16, lower26, lower52),
               names_to = "week",
               values_to = "LEMS")
sygen_lower_pivot$week <- as.factor(recode(sygen_lower_pivot$week,
                                           "lower01" = 1,
                                           "lower04" = 4,
                                           "lower08" = 8,
                                           "lower16" = 16,
                                           "lower26" = 26,
                                           "lower52" = 52))
sygen_tidy <- left_join(sygen_tidy, sygen_lower_pivot,
                        by = c("ptid" = "ptid",
                               "week" = "week"))
sygen_tidy %>%
  write_csv(file = paste(data_path, "sygen-tidy.csv", sep = ""))

#### check for complete cases ####
# get only complete cases, i.e. those for which all time points (weeks) have 
# information for the ASIA grade and the LEMS
sygen_tidy %>% 
  group_by(ptid) %>%
  filter(!any(is.na(ASIA_scale) | is.na(LEMS))) %>%
  distinct(ptid) %>%
  n_distinct()
# use a slightly looser definition of complete case: weeks 1 and 52 have 
# information for ASIA grade and LEM
sygen_tidy %>%
  filter(week %in% c(1, 52)) %>%
  group_by(ptid) %>% 
  filter(!any(is.na(ASIA_scale) | is.na(LEMS))) %>%
  distinct(ptid) %>%
  n_distinct()
