setwd("/home/hannes/Desktop/stat-devel/flexstat-goe")

records <- read.csv("csv/records.csv")
pruefer <- levels(records$pruefer)

results <- data.frame(matrix(nrow = 0, ncol = 3))
names(results) <- c("pruefer", "anzahl", "schnitt")

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
