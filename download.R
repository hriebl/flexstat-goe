setwd("/home/hannes/Desktop/stat-devel/flexstat-goe")

# install.packages("jsonlite")
# install.packages("httr")

library(jsonlite)
library(httr)

resultsURL <- "https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/queryexecution/results"
requestJSON <- readChar("json/request.json", file.info("json/request.json")$size)
modulesJSON <- readChar("json/modules.json", file.info("json/modules.json")$size)
modulesDataFrame <- fromJSON(modulesJSON)

# module 112 is B.WIWI-BWL.0001 Unternehmenssteuern I,
# module 113 is B.WIWI-BWL.0002 Interne Unternehmensrechnung,
# for more, see head(modulesDataFrame) etc.

records <- data.frame(matrix(nrow = 0, ncol = 21))

for (module in modulesDataFrame$value) {
  for (semester in 52:62) {

    # semester 52 is winter semester 2010/11,
    # semester 53 is summer semester 2011
    # and so on...

    moduleString <- paste0('"lastValue":"', module, '"')
    thisRequestJSON <- sub('"lastValue":"112"', moduleString, requestJSON)

    semesterString <- paste0('"lastValue":"', semester, '"')
    thisRequestJSON <- sub('"lastValue":"60"', semesterString, thisRequestJSON)

    bodyList <- list(data = thisRequestJSON)
    request <- POST(resultsURL, body = bodyList, encode = "form")
    stop_for_status(request)

    responseJSON <- content(request, encoding = "UTF-8", type = "text")
    responseDataFrame <- fromJSON(responseJSON)$data$records

    if (class(responseDataFrame) == "data.frame") {

      thisRecords <- cbind(
        modul            = module,
        semester         = semester,
        termin           = responseDataFrame[, "Klausurtermin"],
        pruefer          = responseDataFrame[, "Prüfer"],
        anzahl           = responseDataFrame[, "Anzahl"],
        bestanden        = responseDataFrame[, "Bestanden"],
        nichtBestanden   = responseDataFrame[, "Nicht bestanden"],
        schnitt          = responseDataFrame[, "Notenschnitt"],
        schnittBestanden = responseDataFrame[, "Notenschnitt (nur Bestanden)"],
        "X1_0"           = responseDataFrame[, "1_0"],
        "X1_3"           = responseDataFrame[, "1_3"],
        "X1_7"           = responseDataFrame[, "1_7"],
        "X2_0"           = responseDataFrame[, "2_0"],
        "X2_3"           = responseDataFrame[, "2_3"],
        "X2_7"           = responseDataFrame[, "2_7"],
        "X3_0"           = responseDataFrame[, "3_0"],
        "X3_3"           = responseDataFrame[, "3_3"],
        "X3_7"           = responseDataFrame[, "3_7"],
        "X4_0"           = responseDataFrame[, "4_0"],
        "X5_0"           = responseDataFrame[, "5_0"],
        ohneNote         = responseDataFrame[, "Ohne Note"]
      )

      firstNewRow <- nrow(records) + 1
      lastNewRow <- nrow(records) + nrow(thisRecords)
      records[firstNewRow:lastNewRow,] <- thisRecords

    }
  }
}

names(records) <- colnames(thisRecords)
records[records == "-"] <- NA
records[records == ""] <- NA

records[, c(1:2, 5:21)] <- sapply(records[, c(1:2, 5:21)], as.numeric)

# write.csv(records, "csv/records.csv", row.names = FALSE)
