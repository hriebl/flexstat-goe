#------------------------------------------------------------------#
# R-Projekt: Flexstat Data
# Authors: Hannes Riebl, Stanislaus Stadlmann
#------------------------------------------------------------------#

# ------ PRELIMINARIES ------ #

# Set Working Directory
setwd("~/GitHub/flexstat-goe")

# Delete everything
rm(list = ls())

# Packages
library(jsonlite)
library(httr)

# ------ DATA DOWNLOAD ------ #

# Get Link
resultsURL <- "https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/queryexecution/results"

# Read both JSON files
requestJSON <- readChar("json/request.json", file.info("json/request.json")$size)
modulesJSON <- readChar("json/modules.json", file.info("json/modules.json")$size)

# Store Name of all courses and their code
modulesDataFrame <- fromJSON(modulesJSON)

# Container for records
records <- data.frame(matrix(nrow = 0, ncol = 21))

for (module in modulesDataFrame$value) {
  for (semester in 52:62) { # 52 is Winter-semester 2010/11

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
  
  # Prozessleiste
  cat(paste("Modul", 
            which(modulesDataFrame$value == module), 
            "von", 
            length(modulesDataFrame$value), 
            "\n"))
}

# Delete NA's and assign right column names
names(records) <- colnames(thisRecords)
records[records == "-"] <- NA
records[records == ""] <- NA

# Transform columns to be numeric
records[, c(1:2, 5:21)] <- sapply(records[, c(1:2, 5:21)], as.numeric)

# Export data in .RData object
save(list = "records", file = "data/records.RData")
