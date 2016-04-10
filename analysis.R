#------------------------------------------------------------------#
# R-Projekt: Flexstat Data
# Authors: Hannes Riebl, Stanislaus Stadlmann
#------------------------------------------------------------------#

# ------ PRELIMINARIES ------ #

# Delete everything
rm(list = ls())

# Packages
library(dplyr)
library(ggplot2)

# Set WD as the location from Document
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)
rm(path)

# Load data
load(file = "data/records.RData")

# ------ ANALYSIS ------ #

# All people who gave exams
pruefer <- unique(records$pruefer)

# Container
results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(results) <- c("pruefer", "anzahl", "schnitt")

# Some formatting
records <- records %>%
  mutate_each(funs(as.numeric), anzahl:schnittBestanden) %>%
  mutate_each(funs(as.factor), pruefer, modul, semester) %>%
  mutate_each(funs(as.Date(., "%d.%m.%Y")), termin)
  
# Mean and Sum per examiner
meansum <- records %>%
  group_by(pruefer) %>%
  summarise_each(funs(mean(., na.rm = T), sum(., na.rm = T)), schnitt, anzahl) %>%
  select(pruefer, schnitt_mean, anzahl_sum) %>%
  distinct() %>%
  na.omit()

# Graph Examiners with highest avg and more than 50 exams
meansum %>%
  arrange(desc(schnitt_mean)) %>%
  filter(pruefer != "0PA") %>%
  filter(anzahl_sum > 50) %>%
  slice(1:15) %>%
  ggplot(., aes(x = reorder(pruefer, -schnitt_mean), y = schnitt_mean)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Prüfer", y = "Schnitt") +
  coord_cartesian(ylim = c(1, 4))
  


### HANNES
for (i in pruefer) {

  thisAnzahl <- sum(records[records$pruefer == i, "anzahl"])
  thisSchnitt <- weighted.mean(records[records$pruefer == i, "schnitt"],
                               records[records$pruefer == i, "anzahl"],
                               na.rm = TRUE)

  if (thisAnzahl >= 100) {
    results[nrow(results) + 1,] <- c(i, thisAnzahl, thisSchnitt)
  }

}

results[, 2:3] <- sapply(results[, 2:3], as.numeric)

meistePruefungen <- results[order(results$anzahl, decreasing = TRUE),]
besteSchnitte <- results[order(results$schnitt),]

head(meistePruefungen)
head(besteSchnitte)
tail(besteSchnitte)

# write.csv(meistePruefungen, "csv/pruefer-meiste-pruefungen.csv", row.names = FALSE)
# write.csv(besteSchnitte, "csv/pruefer-beste-schnitte.csv", row.names = FALSE)
