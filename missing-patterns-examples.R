library(tidyverse)
save.plots <- TRUE

n <- 10000
ais.fractions <- c("A" = 0.4, "B" = 0.17, "C" = 0.13, "D" = 0.3)
sex.fractions <- c("female" = 0.35, "male" = 0.65)

ais.grades <- c(rep("A", floor(n * ais.fractions[["A"]])),
                rep("B", floor(n * ais.fractions[["B"]])),
                rep("C", floor(n * ais.fractions[["C"]])),
                rep("D", floor(n * ais.fractions[["D"]])))
sex <- c(rep("female", floor(n * sex.fractions[["female"]])),
         rep("male", floor(n * sex.fractions[["male"]])))
ais.grades <- sample(ais.grades)
sex <- sample(sex)

data.simulated <- tibble(AIS = factor(ais.grades,
                                      levels = c("A", "B", "C", "D"),
                                      ordered = TRUE),
                         sex = factor(sex),
                         MCAR = rep(NA, n),
                         MAR = rep(NA, n),
                         MNAR = rep(NA, n))
ggplot(data = data.simulated, aes(sex)) +
    geom_bar()
ggplot(data = data.simulated, aes(AIS)) +
    geom_bar()

# MCAR
fraction.missing <- 0.15
for (grade in names(ais.fractions)) {
    n.by.grade <- floor(n * ais.fractions[[grade]])
    missing <- sample(as.logical(c(rep(1, floor(n.by.grade * fraction.missing)),
                                   rep(0, ceiling(n.by.grade * (1 - fraction.missing))))
                                 )
                      )
    data.simulated <- data.simulated %>%
        mutate(MCAR = ifelse(AIS == grade, missing, MCAR))
}
mcar.plot <- ggplot(data.simulated, aes(fill = MCAR, x = AIS)) +
    geom_bar(position = "fill") +
    geom_hline(yintercept = fraction.missing,
               linetype = "dashed") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_fill_manual("Missing?",
                      labels = c(`FALSE` = "No", `TRUE` = "Yes"),
                      values = c(`FALSE` = "#C5E8B7", `TRUE` = "#2EB62C")) +
    theme_classic() +
    theme(axis.title.y = element_blank())
if (save.plots) {
    ggsave("~/Desktop/mcar.pdf",
           plot = mcar.plot,
           width = 20, height = 15, units = "cm",
           device = "pdf")
}

# MAR
fraction.missing <- c("female" = 0.15,
                      "male" = 0.35)
for (grade in names(ais.fractions)) {
    n.females <- data.simulated %>%
        filter(AIS == grade & sex == "female") %>%
        count()
    n.males <- data.simulated %>%
        filter(AIS == grade & sex == "male") %>%
        count()
    missing.females <- sample(as.logical(c(rep(1, floor(n.females$n * fraction.missing[["female"]])),
                                           rep(0, ceiling(n.females$n * (1 - fraction.missing[["female"]]))))
                                         )
                              )
    missing.males <- sample(as.logical(c(rep(1, floor(n.males$n * fraction.missing[["male"]])),
                                         rep(0, ceiling(n.males$n * (1 - fraction.missing[["male"]]))))
                                       )
                            )
    data.simulated <- data.simulated %>%
        mutate(MAR = ifelse(AIS == grade, ifelse(sex == "female",
                                                 missing.females,
                                                 missing.males),
                            MAR))
}
mar.plot <- ggplot(data.simulated, aes(fill = MAR, x = AIS)) +
    geom_bar(position = "fill") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_fill_manual("Missing?",
                      labels = c(`FALSE` = "No", `TRUE` = "Yes"),
                      values = c(`FALSE` = "#C5E8B7", `TRUE` = "#2EB62C")) +
    theme_classic() +
    theme(axis.title.y = element_blank())
if (save.plots) {
    ggsave("~/Desktop/mar.pdf",
           plot = mar.plot,
           width = 20, height = 15, units = "cm",
           device = "pdf")
}
mar.annotated.plot <- ggplot(data.simulated, aes(fill = MAR, x = AIS)) +
    geom_bar(position = "fill") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_fill_manual("Missing?",
                      labels = c(`FALSE` = "No", `TRUE` = "Yes"),
                      values = c(`FALSE` = "#C5E8B7", `TRUE` = "#2EB62C")) +
    geom_hline(yintercept = c(fraction.missing,
                              weighted.mean(fraction.missing, sex.fractions)),
               linetype = "dashed") +
    annotate("text",
             x = rep(4.25, 3), y = c(fraction.missing,
                                     weighted.mean(fraction.missing, sex.fractions)),
             label = c("female", "male", "mean"),
             vjust = -1) +
    theme_classic() +
    theme(axis.title.y = element_blank())
if (save.plots) {
    ggsave("~/Desktop/mar-annotated.pdf",
           plot = mar.annotated.plot,
           width = 20, height = 15, units = "cm",
           device = "pdf")
}
mar.resolved.plot <- ggplot(data.simulated, aes(x = sex, fill = MAR)) +
    geom_bar(position = "fill") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    facet_wrap(. ~ AIS, nrow = 1) +
    # annotate("text", c(1, 3, 5, 7), y = -5, label = c("A", "B", "C", "D")) +
    scale_fill_manual("Missing?",
                      labels = c(`FALSE` = "No", `TRUE` = "Yes"),
                      values = c(`FALSE` = "#C5E8B7", `TRUE` = "#2EB62C")) +
    theme_classic() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 12, face = "bold"))
if (save.plots) {
    ggsave("~/Desktop/mar-resolved.pdf",
           plot = mar.resolved.plot,
           width = 20, height = 15, units = "cm",
           device = "pdf")
}
# MNAR
fraction.missing <- c("A" = .15,
                      "B" = .35,
                      "C" = .3,
                      "D" = .25)
for (grade in names(ais.fractions)) {
    n.grade <- data.simulated %>%
        filter(AIS == grade) %>%
        count()
    missing.grade <- sample(as.logical(c(rep(1, floor(n.grade$n * fraction.missing[[grade]])),
                                         rep(0, ceiling(n.grade$n * (1 - fraction.missing[[grade]]))))
    )
    )
    data.simulated <- data.simulated %>%
        mutate(MNAR = ifelse(AIS == grade,
                             missing.grade,
                             MNAR))
}
mnar.plot <- ggplot(data.simulated, aes(x = AIS, fill = MNAR)) +
    geom_bar(position = "fill") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    # annotate("text", c(1, 3, 5, 7), y = -5, label = c("A", "B", "C", "D")) +
    scale_fill_manual("Missing?",
                      labels = c(`FALSE` = "No", `TRUE` = "Yes"),
                      values = c(`FALSE` = "#C5E8B7", `TRUE` = "#2EB62C")) +
    theme_classic() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())
if (save.plots) {
    ggsave("~/Desktop/mnar-plot.pdf",
           plot = mnar.plot ,
           width = 20, height = 15, units = "cm",
           device = "pdf")
}
mnar.complete <- data.simulated %>%
    filter(!MNAR) %>%
    select(AIS)
mnar.only.present.plot <- data.simulated %>%
    filter(!MNAR) %>%
    select(AIS) %>%
    ggplot(aes(AIS)) +
    geom_bar(fill = "#2EB62C") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 3500)) +
    theme_classic()
if (save.plots) {
    ggsave("~/Desktop/mnar-present-plot.pdf",
           plot = mnar.only.present.plot,
           width = 20, height = 15, units = "cm",
           device = "pdf")
}
