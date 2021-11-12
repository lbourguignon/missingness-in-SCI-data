################################################################################
# SCI - Handling missing data project
# L. Bourguignon & L.P. Lukas
# First version : 31.10.2021
# Last update : 12.11.2021
# ------------------------------------------------------------------------------
# VISUAL EXPLORATION
################################################################################

library(tidyverse)
library(cowplot)
library(finalfit)
library(naniar)
# custom functions
source("functions.R")

data.set <- "emsci" # one of: sygen, emsci
path.to.data = "/Volumes/borgwardt/Data/SCI/"
save.plots <- FALSE

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
                     na = c("", "NA", "ND")) %>%
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
                     na = c("", "NA", "INT", "NT") # INT - intermediate; cannot be
                     # resolved to NLI with information contained in EMSCI?
    )
    data <- data %>%
        filter(ais1 %in% c("A", "B", "C", "D", NA))
} else {
    data <- NA
    print("Invalid data set identifier! Must be 'sygen' or 'emsci'.")
}
# add the aggregated level attribute
data <- data %>%
    mutate(level = case_when(
        grepl('C', splvl, fixed = TRUE) ~ "cervical",
        grepl('T', splvl, fixed = TRUE) ~ "thoracic",
        grepl('L', splvl, fixed = TRUE) ~ "lumbar")
    ) %>%
    mutate(level = factor(level,
                          levels = c("cervical", "thoracic", "lumbar"))
    )

################################################################################
# Visualise raw data
################################################################################
# Visualise the distributions of the different variables at baseline
# AIS grade
ais1.dist <- plot.bar(data, "ais1", "count",
                      title = paste("AIS grades at week 1 (",
                                    toupper(data.set), ")", sep = ""))
ais1.dist.prop <- plot.bar(data, "ais1", "prop",
                           title = paste("AIS grades at week 1 (",
                                         toupper(data.set), ")", sep = ""))
if (save.plots) {
    ggsave(paste("~/Desktop/", toupper(data.set), "-AIS-wk1.pdf", sep = ""),
           plot = ais1.dist,
           width = 15, height = 20, units = "cm",
           device = "pdf")
}
ais1.dist.no.NA <- plot.bar(data, "ais1", "count", include.NA = FALSE,
                            title = paste("AIS grades at week 1 (",
                                          toupper(data.set), ")", sep = ""))
ais.1.dist.no.NA.prop <- plot.bar(data, "ais1", "count", include.NA = FALSE,
                                  title = paste("AIS grades at week 1 (",
                                                toupper(data.set), ")", sep = ""))
if (save.plots) {
    ggsave(paste("~/Desktop/", toupper(data.set), "-AIS-wk1-no-NA.pdf", sep = ""),
           plot = ais1.dist.no.NA,
           width = 15, height = 20, units = "cm",
           device = "pdf")
}
# Age
age.dist <- ggplot(data, aes(x = age)) +
    geom_histogram(aes(y=..density..), bins = 100,
                   colour = "black", fill = "white") +
    geom_density(alpha = .2, fill = "#FF6666") +
    xlim(0, 100) +
    labs(title = paste("Age at injury (", toupper(data.set), ")", sep = ""))
# Sex
sex.dist <- plot.bar(data, "sexcd", "count",
                     title = paste("Sex (", toupper(data.set), ")", sep = "")) +
    scale_x_discrete(labels = c("1" = "Female", "2" = "Male"), name = 'Sex')
sex.dist.prop <- plot.bar(data, "sexcd", "prop",
                          title = paste("Sex (", toupper(data.set), ")", sep = "")) +
    scale_x_discrete(labels = c("1" = "Female", "2" = "Male"), name = 'Sex')
# LEMS at week 01
lems01.dist <- ggplot(data) +
    geom_density(aes(x = lower01, y = ..scaled..),
                 alpha = .2, fill = "#FF6666") +
    xlim(0, 50) +
    labs(title = paste("LEMS at week 1 (", toupper(data.set), ")", sep = "")) +
    ylab("Density (scaled)")
# LEMS at week 52
lems52.dist <- ggplot(data) +
    geom_density(aes(x = lower52, y = ..scaled..),
                 alpha = .2, fill = "#FF6666") +
    xlim(0,50) +
    labs(title = paste("LEMS at week 52 (", toupper(data.set), ")", sep = "")) +
    ylab("Density (scaled)")
# Level of injury
nli.dist <- plot.bar(data, "splvl", "count",
                     title = paste("Neurological level of injury (",
                                   toupper(data.set), ")", sep = ""))
if (save.plots) {
    ggsave(paste("~/Desktop/", toupper(data.set), "-NLI.pdf", sep = ""),
           plot = nli.dist,
           width = 15, height = 20, units = "cm",
           device = "pdf")
}
nli.dist.no.NA <- plot.bar(data, "splvl", "count", include.NA = FALSE,
                           title = paste("Neurological level of injury (",
                                         toupper(data.set), ")", sep = ""))
if (save.plots) {
    ggsave(paste("~/Desktop/", toupper(data.set), "-NLI-no-NA.pdf", sep = ""),
           plot = nli.dist.no.NA,
           width = 15, height = 20, units = "cm",
           device = "pdf")
}

# Variables stratified by AIS grade
#ggplot(sygen_analysis, aes(x=lower01, fill=ais1)) + geom_density(alpha=.3)+ xlim(0,50)
#ggplot(sygen_analysis, aes(x=lower52, fill=ais1)) + geom_density(alpha=.3)+ xlim(0,50)
#ggplot(sygen_analysis, aes(x=age, fill=ais1)) + geom_density(alpha=.3)
#ggplot(sygen_analysis, aes(x=factor(sexcd), fill=ais1)) + geom_density(alpha=.3)
#ggplot(sygen_analysis, aes(x=splvl, fill=ais1)) + geom_histogram(stat="count")

# Relationship between LEMS at week 01 and week 52, coloured by AIS grade
lems.by.ais <- ggplot(data, aes(lower01, lower52, colour = ais1)) +
    geom_jitter(width = 2, height = 2,
                alpha = .5, stroke = 1) +
    xlim(-2,50) + ylim(-2,50) +
    xlab("LEMS (week 1)") + ylab("LEMS (week 52)") +
    labs(title = paste("Change in LEMS (", toupper(data.set), ")", sep = ""),
         color = "AIS (week 1)") +
    guides(col = guide_legend(ncol = 2)) +
    theme(legend.position = c(.9, .25),
          legend.justification = "center", # c(5, 1)
    )

# Change in LEMS distribution over time, by AIS grade
ais.counts <- data %>%
    group_by(ais1) %>%
    count() %>%
    mutate(n = paste("n = ", n, sep = ""))
lems01.by.AIS <- ggplot(data) +
    geom_density(aes(x=lower01, y=..scaled..), # use ..scaled.. for better
                 alpha=.2, fill="#FF6666") + # comparability between AIS grades
    xlim(-1, 50) +
    ylim(0, 1.1) +
    xlab("LEMS (week 1)") +
    ylab("Density (scaled)") +
    facet_grid(rows = vars(ais1), scales = 'fixed') +
    geom_text(data = ais.counts,
              mapping = aes(x = 25, y = .975, label = n),
    )
lems52.by.AIS <- ggplot(data) +
    geom_density(aes(x=lower52, y=..scaled..),
                 alpha=.2, fill="#FF6666") +
    xlim(-1, 50) +
    ylim(0, 1.1) +
    xlab("LEMS (week 52)") +
    facet_grid(rows = vars(ais1), scales = 'fixed') +
    theme(axis.title.y = element_blank()) # remove y-axis label
lems.by.AIS.dist <- plot_grid(lems01.by.AIS,
                              lems52.by.AIS,
                              ncol = 2, nrow = 1)
# lems.by.AIS.dist
if (save.plots) {
    ggsave(paste("~/Desktop/", toupper(data.set), "-LEMS-1-52-by-AIS.pdf", sep = ""),
           plot = lems.by.AIS.dist,
           width = 40, height = 20, units = "cm",
           device = "pdf")
}

################################################################################
# Visualise missingness in data
################################################################################
## Visualise original missingness patterns
# reference for interpretation of the following plot:
# https://cran.r-project.org/web/packages/finalfit/vignettes/missing.html
missing_dist <- data %>%
    missing_pairs(dependent = "lower52",
                  explanatory = c("age", "sexcd", "level", "lower01", "ais1"),
                  title = paste(toupper(data.set), " - missing data", sep = ""))
missing_dist_fill <- data %>%
    missing_pairs(dependent = "lower52",
                  explanatory = c("age", "sexcd", "level", "lower01", "ais1"),
                  position = "fill",
                  title = paste(toupper(data.set), " - missing data", sep = ""))
# x-axis represent patients, light blue = NA
missing_heatmap <- data %>%
    missing_plot(title = paste(toupper(data.set), " - missing data", sep = ""))
## Visualise the co-occurrence of missingness (ONLY Sygen)
# previous visualisations worked w/ smaller subset of variables; EMSCI only
# adapted on this subset (as of 04 November)
sygen <- read.csv(paste(path.to.data,
                        'Sygen/JohnKramersProject_DATA_2019-10-07_0111.csv',
                        sep = ''))
sygen[sygen == ''] <- NA
sygen[sygen == 'ND'] <- NA

# Define vectors with column names of the different category of variables
vec_lems <- c('lower01', 'lower04', 'lower08', 'lower16', 'lower26', 'lower52') # all LEMS
vec_ais <- c('ais1', 'ais4', 'ais8', 'ais16', 'ais26', 'ais52') # all AIS grades
vec_mot <- c('ankdol', 'ankdor', 'ankpll', 'ankplr',
             'elbexl', 'elbexr', 'elbfll', 'elbflr',
             'finabl', 'finabr','finfll', 'finflr',
             'gretol', 'gretor','hipfll', 'hipflr',
             'kneetr', 'kneexl', 'wrextl', 'wrextr') # all myotomes
sygen_levels <- c('c2', 'c3', 'c4', 'c4', 'c6', 'c7', 'c8',
                  't1', 't2', 't3', 't4', 't5', 't6', 't7', 't8', 't9', 't10', 't11', 't12',
                  'l1', 'l2', 'l3', 'l4', 'l5',
                  's1', 's2', 's3', 's45') # all levels evaluated in sensory scores
vec_pp <- append(vector_var(sygen_levels, 'ppl', 'after'),
                 vector_var(sygen_levels, 'ppr', 'after')) # all dermatomes for pin prick
vec_lt <- append(vector_var(sygen_levels, 'ltl', 'after'),
                 vector_var(sygen_levels, 'ltr', 'after')) # all dermatomes for light touch

# ------------------------------------------------------------------------------
# WILL BE INCLUDED IN FUNCTION FILE
plot_miss_upset <- function(data, cols){

  df_temp <- subset_col(cols, data)

  plot <- gg_miss_upset(df_temp, nsets = n_var_miss(df_temp))

  return (plot)
}
# ------------------------------------------------------------------------------

# co-occurring NAs among AIS measurements over time
miss_upset_ais_overtime <- plot_miss_upset(sygen, vec_ais)
miss_upset_ais_overtime_A <- plot_miss_upset(sygen[sygen$ais1 == 'AIS A', ],
                                             vec_ais)

# co-occurring NAs among LEMS measurements over time
miss_upset_lems_overtime <- plot_miss_upset(sygen, vec_lems)
# co-occurring NAs among LEMS and AIS measurements over time
miss_upset_lems_ais_overtime <- plot_miss_upset(sygen,
                                                c(vec_lems, vec_ais,
                                                  'vaccd01', 'anyana01'))
# co-occurring NAs among myotomes at week 00
miss_upset_myotomes_00 <- plot_miss_upset(sygen, paste0(vec_mot, '00'))
# co-occurring NAs among myotomes at week 52
miss_upset_myotomes_52 <- plot_miss_upset(sygen, paste0(vec_mot, '52'))
# co-occurring NAs among pin prink dermatomes at week 01
miss_upset_pinprick_01 <- plot_miss_upset(sygen, paste0(vec_pp, '01')) # always all missing together
# co-occurring NAs among pin prink dermatomes at week 52
miss_upset_pinprick_52 <- plot_miss_upset(sygen, paste0(vec_pp, '52'))
# co-occurring NAs among light touch dermatomes at week 00
miss_upset_lighttouch_01 <- plot_miss_upset(sygen, paste0(vec_lt, '01'))
# co-occurring NAs among light touch dermatomes at week 52
miss_upset_lighttouch_52 <- plot_miss_upset(sygen, paste0(vec_lt, '52'))

# LEMS dataframe and overview heatmap
sygen_lems_overtime <- subset_col(vec_lems, sygen)
sygen_lems_overtime %>%
  missing_plot()
vis_miss(sygen_lems_overtime)

# AIS grade dataframe and overview heatmap
sygen_ais_overtime <- subset_col(vec_ais, sygen)
sygen_ais_overtime %>%
  missing_plot()
vis_miss(sygen_ais_overtime)

# LEMS and AIS grade dataframe + VAC and DAP
sygen_ais_lems_overtime <- subset_col(c(vec_lems, vec_ais,
                                        'vaccd01', 'anyana01'), sygen)

# -------------------------

# Sankey plot : transition in AIS grades from week 01 to week 26

# Remove "AIS" from the values of the AIS grades columns
sygen_ais_overtime <- sygen_ais_overtime %>%
  mutate_all(funs(str_replace(., "AIS ", "")))

# renaming the variable values such that they include information on the week when it was collected
sygen_ais_overtime$ais1 <- paste(sygen_ais_overtime$ais1, "_1", sep="")
sygen_ais_overtime$ais4 <- paste(sygen_ais_overtime$ais4, "_4", sep="")
sygen_ais_overtime$ais8 <- paste(sygen_ais_overtime$ais8, "_8", sep="")
sygen_ais_overtime$ais16 <- paste(sygen_ais_overtime$ais16, "_16", sep="")
sygen_ais_overtime$ais26 <- paste(sygen_ais_overtime$ais26, "_26", sep="")
sygen_ais_overtime$ais52 <- paste(sygen_ais_overtime$ais52, "_52", sep="")

# Create a table with :
# source (i.e. AIS grade at week 1),
# target (i.e. AIS grade at week 26)
# value (i.e. number of patients go from one category to another from week 1 to 26)
transition1_26 <- sygen_ais_overtime %>%
  group_by(ais1) %>%
  count(ais26)
transition1_26 <- as.data.frame(transition1_26)
names(transition1_26) <- c('source', 'target', 'value')

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(transition1_26$source),
         as.character(transition1_26$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
transition1_26$IDsource <- match(transition1_26$source, nodes$name)-1
transition1_26$IDtarget <- match(transition1_26$target, nodes$name)-1

# Make the Network
sankey_transition_1_26 <- sankeyNetwork(Links = transition1_26, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name",
                   sinksRight=T, iterations = 0)
sankey_transition_1_26

# -------------------------

# Recode the data: 0 is NA, 1 is value available
sygen_lems_overtime_binary <- sygen_lems_overtime
sygen_lems_overtime_binary[!(is.na(sygen_lems_overtime_binary))] <- 1
sygen_lems_overtime_binary[(is.na(sygen_lems_overtime_binary))] <- 0

# Similar plot as before looking at relationship between lems at week 01 and week 52
# Here are also plotted points:
# below 0 on x-axis : lems at week 01 known, NA for lems at week 52
# below 0 on y-axis : NA for lems at week 01, lems at week 52 known
ggplot(sygen_ais_lems_overtime, aes(lower01, lower52)) +
  #geom_jitter(width = 2, height = 2) +
  geom_miss_point() +
  facet_wrap(~ais1)

# Add information about severity grade at week 01 to the binarised lems dataframe
sygen_lems_overtime_binary_ais1 <- sygen_lems_overtime_binary
sygen_lems_overtime_binary_ais1$ais1 <- sygen$ais1

# Transform lems info overtime to patterns representing sequence of LEMS measurements
# e.g. 0-1-1-1-1-1 means LEMS is missing at week 01 but present for all following time points
lems_na_pattern_ais1 <- sygen_lems_overtime_binary_ais1 %>%
  group_by(lower01, lower04, lower08, lower16, lower26, lower52, ais1) %>%
  count()
lems_na_pattern_ais1$pattern <- apply( lems_na_pattern_ais1[ , vec_lems ] , 1 , paste , collapse = "-" )
# Remove pattern with all measurements available since we are interested in patterns with NAs
lems_na_pattern_ais1_withna <- lems_na_pattern_ais1[!(lems_na_pattern_ais1$pattern=="1-1-1-1-1-1"),]

# Harmonise the NAs in the AIS grade column
#lems_na_pattern_ais1_withna[lems_na_pattern_ais1_withna == ""] <- NA
#lems_na_pattern_ais1_withna[lems_na_pattern_ais1_withna == 'ND'] <- NA

# Subset only patterns with known AIS grade
lems_na_pattern_ais1_withna_aisnonena <- lems_na_pattern_ais1_withna[!(is.na(lems_na_pattern_ais1_withna$ais1)),]

# Plot heatmap: number of patients per missing pattern per AIS grade
ggplot(lems_na_pattern_ais1_withna, aes(x=ais1, y=pattern, fill=n)) +
  geom_tile() +
  geom_text(aes(label = n, color='white'))

# Decision tree that best determine the proportion of missingness in patients
sygen_lems_ais_overtime[sygen_lems_ais_overtime == ""] <- NA
#sygen_lems_ais_overtime[sygen_lems_ais_overtime == 'ND'] <- NA
sygen_lems_ais_overtime %>%
  add_prop_miss() %>%
  rpart(prop_miss_all ~ ., data = .) %>%
  prp(type = 4, extra = 101, prefix = "Prop. Miss = ")
