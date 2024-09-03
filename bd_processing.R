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
births$yearQuarter <- paste0(births$yearBirth,"-",births$quarter)

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

#Correcting or replacing mother's age using the mean age for quarter
meanAge <- births
meanAge <- meanAge[!is.na(meanAge$motherAge), ]
meanAge <- meanAge%>%
  group_by(yearQuarter)%>%
  summarise(meanAge = round(mean(motherAge),0))

births <- merge(births, meanAge, by = "yearQuarter", ALL = TRUE)

births$motherAgeCorr <- ifelse(births$motherAge < 15 | is.na(births$motherAge), births$meanAge, births$motherAge)
births <- births |>
  mutate(myageGroup = case_when(
    motherAgeCorr < 15 ~ "<15",
    motherAgeCorr <= 19 ~ "15-19",
    motherAgeCorr <= 24 ~ "20-24",
    motherAgeCorr <= 29 ~ "25-29",
    motherAgeCorr <= 34 ~ "30-34",
    motherAgeCorr <= 39 ~ "35-39",
    motherAgeCorr <= 45 ~ "40-44",
    motherAgeCorr > 45 ~ ">45"
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
#Quarters
colnames(deaths)[colnames(deaths) == "Quarter"] <- "quarter"
deaths <- deaths |>
  mutate(quarter = case_when(
    quarter == "Q1" ~ 1,
    quarter == "Q2" ~ 2,
    quarter == "Q3" ~ 3,
    quarter == "Q4" ~ 4
  ))

#Date of death, year of death, and month of death
colnames(deaths)[colnames(deaths) == "Date of Death"] <- "DOD"
deaths$date <- convertToDateTime(deaths$DOD, origin = "1900-01-01")
deaths$yearDeath <- year(deaths$date)
deaths$monthDeath <- month(deaths$date)
deaths$monthDeath[is.na(deaths$monthDeath)] <- "NS"
deaths$yearQuarter <- paste0(deaths$yearDeath,"-",deaths$quarter)
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

meanAge <- deaths
meanAge <- meanAge[!is.na(meanAge$Age), ]
meanAge <- meanAge%>%
  group_by(yearQuarter)%>%
  summarise(meanAge = round(mean(Age),0))

deaths <- merge(deaths, meanAge, by = "yearQuarter", ALL = TRUE)

deaths$ageCorr <- ifelse(deaths$Age < 15 | is.na(deaths$Age), deaths$meanAge, deaths$Age)

deaths <- deaths |>
  mutate(myageGroup = case_when(
    ageCorr <= 4 ~ "0-4",
    ageCorr <= 9 ~ "5-9",
    ageCorr <= 14 ~ "10-14",
    ageCorr <= 19 ~ "15-19",
    ageCorr <= 24 ~ "20-24",
    ageCorr <= 29 ~ "25-29",
    ageCorr <= 34 ~ "30-34",
    ageCorr <= 39 ~ "35-39",
    ageCorr <= 44 ~ "40-44",
    ageCorr <= 49 ~ "45-49",
    ageCorr <= 54 ~ "50-54",
    ageCorr <= 59 ~ "55-59",
    ageCorr <= 64 ~ "60-64",
    ageCorr <= 69 ~ "65-69",
    ageCorr <= 74 ~ "70-74",
    ageCorr <= 79 ~ "75-79",
    ageCorr <= 84 ~ "70-84",
    ageCorr >= 85 ~ "85+"
  ))
deaths$myageGroup <- ifelse(is.na(deaths$ageCorr),"NA", deaths$myageGroup)
deaths$myageGroup <- ifelse(deaths$ageCorr < 0 & deaths$ageCorr > 100,"ERROR", deaths$myageGroup)

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

deaths$N <- 1
dbWriteTable(mydb, "deaths", deaths, overwrite = TRUE)