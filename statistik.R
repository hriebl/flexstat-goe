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
library(jsonlite)
library(httr)

# Set WD as the location from Document
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)
rm(path)

# Load data
load(file = "data/records.RData")

# ------ DOWNLOAD ------ #

# Get Link
resultsURL <- "https://pruefungsverwaltung.uni-goettingen.de/statistikportal/api/queryexecution/results"

# Read both JSON files
requestJSON <- readChar("json/request.json", file.info("json/request.json")$size)
modulesJSON <- readChar("json/modules.json", file.info("json/modules.json")$size)

# Store Name of all courses and their code
modulesDataFrame <- fromJSON(modulesJSON)

# Container for records
records <- data.frame(matrix(nrow = 0, ncol = 21))

for (module in modulesDataFrame$value[c(91, 87)]) { # just statistics, mathematics
  for (semester in 35:65) { # 52 is Winter-semester 2010/11
    
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

# ------ WRANGLING ------ #

# Delete NA's and assign right column names
names(records) <- colnames(thisRecords)
records[records == "-"] <- NA
records[records == ""] <- NA

# Make numeric, character and Date
records <- records %>%
  mutate_each(funs(as.numeric), anzahl:ohneNote) %>%
  mutate_each(funs(as.factor), pruefer, modul, semester) %>%
  mutate_each(funs(as.Date(., "%d.%m.%Y")), termin)

# Specific colums
mathstat <- records %>%
  select(modul:schnittBestanden) %>%
  na.omit()

# Math, Stat
levels(mathstat$modul) <- c("Mathematik", "Statistik")

# Mathe, Statistik - Plot pro Prüfer
mathstat %>%
  filter(modul == "Statistik") %>%
  ggplot(aes(x = termin, y = schnitt, col = pruefer)) +
  geom_line() +
  labs(x = "Termin", y = "Schnitt (inkl. n. b.)") +
  ggtitle("Schnitt der Statistik-Klausuren im Zeitverlauf")

mathstat %>%
  filter(modul == "Mathematik") %>%
  ggplot(aes(x = termin, y = schnitt, col = pruefer)) +
  geom_line() +
  labs(x = "Termin", y = "Schnitt (inkl. n. b.)") +
  ggtitle("Schnitt der Mathematik-Klausuren im Zeitverlauf")

# Plot generell
ggplot(statistics, aes(x = termin, y = schnitt)) +
  geom_point() +
  geom_smooth()