#Lae Peleti
#Senior Statistician
#Central Statistics Division
#Ministry of Finance and Economic Development
#Government of Tuvalu

#Dynamic directory path mapping
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

source("function/setup.R")

#Establish connection to SQLite database
mydb <- dbConnect(RSQLite::SQLite(), "data/vital.db")

#--------------------------------------------------------------------------------------
#               BIRTHS
#--------------------------------------------------------------------------------------
births <- read_excel("data/births.xlsx")

#Generating sequential id
births$id <- row_number(births$DOB)
births <- births[!is.na(births$DOB), ]

#Date of birth, year of birth, and month of birth
colnames(births)[colnames(births) == "DOB"] <- "dob"
births$date <- convertToDateTime(births$dob, origin = "1900-01-01")
births$yearBirth <- year(births$date)
births$monthBirth <- month(births$date)
births <- births |>
  mutate(quarter = case_when(
    monthBirth <= 3 ~ 1,
    monthBirth <= 6 ~ 2,
    monthBirth <= 9 ~ 3,
    monthBirth <= 12 ~ 4
  ))

#Cleaning labels for sex
colnames(births)[colnames(births) == "Gender"] <- "sex"
births$sexLower <- gsub(" ","", tolower(births$sex))
births$sexNormal <- paste0(toupper(substr(births$sex, 1, 1)), tolower(substr(births$sex, 2, nchar(births$sex))))

#Cleaning labels for islands
colnames(births)[colnames(births) == "Island where Birth"] <- "island"
births$islandLower <- gsub(" ","", tolower(births$island))
births$islandLower <- ifelse(grepl("una|futi", births$islandLower), "Funafuti", births$islandLower)
births$islandNormal <- paste0(toupper(substr(births$islandLower, 1, 1)), tolower(substr(births$islandLower, 2, nchar(births$islandLower))))

#age and age group of mothers
colnames(births)[colnames(births) == "DOB Mother"] <- "motherDOB"
births$motherDate <- convertToDateTime(births$motherDOB, origin = "1900-01-01")
births$motherDOBM = month(births$motherDate)
births$motherDOBY = year(births$motherDate)
births$motherAge = births$yearBirth - births$motherDOBY
births$motherAge <- ifelse(births$motherAge < 15, "ERROR", births$motherAge)
births$motherAge[is.na(births$motherAge)] <- "NS"
births <- births |>
  mutate(myageGroup = case_when(
    motherAge == "NS" ~ "NS",
    motherAge == "ERROR" ~ "ERROR",
    motherAge < 15 ~ "<15",
    motherAge <= 19 ~ "15-19",
    motherAge <= 24 ~ "20-24",
    motherAge <= 29 ~ "25-29",
    motherAge <= 34 ~ "30-34",
    motherAge <= 39 ~ "35-39",
    motherAge <= 45 ~ "40-44",
    motherAge > 45 ~ ">45"
  ))

#Place of birth
colnames(births)[colnames(births) == "Place of birth"] <- "placeBirth"
births$lowerPlaceBirth <- tolower(births$placeBirth)
births$normalPlaceBirth <- paste0(toupper(substr(births$lowerPlaceBirth, 1, 1)), tolower(substr(births$lowerPlaceBirth, 2, nchar(births$lowerPlaceBirth))))

#Marital status of mother
colnames(births)[colnames(births) == "Marital Status"] <- "marriedStat"
births$lowerMarriedStat <- tolower(births$marriedStat)
births$normalMarriedStat <- paste0(toupper(substr(births$lowerMarriedStat, 1, 1)), tolower(substr(births$lowerMarriedStat, 2, nchar(births$lowerMarriedStat))))
births$normalMarriedStat[births$normalMarriedStat=="Maarried"] <- "Married"
births$normalMarriedStat[births$normalMarriedStat=="Maried"] <- "Married"
births$normalMarriedStat[births$normalMarriedStat=="Singlle"] <- "Single"
#Count variable
births$N <- 1
dbWriteTable(mydb, "births", births, overwrite = TRUE)
#--------------------------------------------------------------------------------------
#               DEATHS
#--------------------------------------------------------------------------------------
deaths <- read_excel("data/deaths.xlsx")

deaths$id <- row_number(deaths$Quarter)
deaths <- deaths[!is.na(deaths$Quarter), ]

#Date of death, year of death, and month of death
colnames(deaths)[colnames(deaths) == "Date of Death"] <- "DOD"
deaths$date <- convertToDateTime(deaths$DOD, origin = "1900-01-01")
deaths$yearDeath <- year(deaths$date)
deaths$monthDeath <- month(deaths$date)
deaths$monthDeath[is.na(deaths$monthDeath)] <- "NS"

#Sex
#Manually changing unknown sex for Iona Tinapa
colnames(deaths)[colnames(deaths) == "Name of Deceased"] <- "name"
deaths$Sex <- ifelse(deaths$name == "Iona" & deaths$Surname == "Tinapa",1,deaths$Sex)
deaths$Sex[deaths$Sex==1] <- "Male"
deaths$Sex[deaths$Sex==2] <- "Female"

#Age and age group
colnames(deaths)[colnames(deaths) == "Date of Birth"] <- "DOB"
deathsDOB <- dmy(deaths$DOB)
deaths$DOB <- convertToDateTime(deaths$DOB, origin = "1900-01-01")
deaths$yearBirth <- year(deaths$DOB)
deaths$Age <- deaths$yearDeath - deaths$yearBirth
deaths <- deaths |>
  mutate(myageGroup = case_when(
    Age <= 4 ~ "0-4",
    Age <= 9 ~ "5-9",
    Age <= 14 ~ "10-14",
    Age <= 19 ~ "15-19",
    Age <= 24 ~ "20-24",
    Age <= 29 ~ "25-29",
    Age <= 34 ~ "30-34",
    Age <= 39 ~ "35-39",
    Age <= 44 ~ "40-44",
    Age <= 49 ~ "45-49",
    Age <= 54 ~ "50-54",
    Age <= 59 ~ "55-59",
    Age <= 64 ~ "60-64",
    Age <= 69 ~ "65-69",
    Age <= 74 ~ "70-74",
    Age <= 79 ~ "75-79",
    Age <= 84 ~ "70-84",
    Age >= 85 ~ "85+"
  ))
deaths$myageGroup <- ifelse(is.na(deaths$Age),"NA", deaths$myageGroup)
deaths$myageGroup <- ifelse(deaths$Age < 0 & deaths$Age > 100,"ERROR", deaths$myageGroup)

#island
colnames(deaths)[colnames(deaths) == "Island of occurrence"] <- "island"
deaths$islandLower <- gsub(" ","", tolower(deaths$island))
deaths$islandNormal <- paste0(toupper(substr(deaths$islandLower, 1, 1)), tolower(substr(deaths$islandLower, 2, nchar(deaths$islandLower))))
deaths$islandNormal[deaths$islandNormal=="Nmaga"] <- "Nanumaga"
deaths$islandNormal[deaths$islandNormal=="Fuanfuti"] <- "Funafuti"

#place of death
colnames(deaths)[colnames(deaths) == "Place of death"] <- "place"
deaths$placeLower <- gsub(" ","", tolower(deaths$place))
deaths$placeNormal <- paste0(toupper(substr(deaths$placeLower, 1, 1)), tolower(substr(deaths$placeLower, 2, nchar(deaths$placeLower))))
deaths$placeNormal[deaths$place=="Hh"] <- "Home"
deaths$placeNormal[deaths$place=="Hopsital"] <- "Hospital"

#Quarters
colnames(deaths)[colnames(deaths) == "Quarter"] <- "quarter"
deaths <- deaths |>
  mutate(quarter = case_when(
    quarter == "Q1" ~ 1,
    quarter == "Q2" ~ 2,
    quarter == "Q3" ~ 3,
    quarter == "Q4" ~ 4
    ))

deaths$N <- 1
dbWriteTable(mydb, "deaths", deaths, overwrite = TRUE)