#Lae Peleti
#Senior Statistician
#Central Statistics Division
#Ministry of Finance and Economic Development
#Government of Tuvalu

##processing arrival and departure

#load libraries
library(readxl) #used to import excel files
library(tidyverse)
library(dplyr)
library(RSQLite)
library(openxlsx)

#Dynamic directory path mapping
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

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
births$quarter <- ifelse(births$monthBirth>=1 & births$monthBirth <=3,1,
                         ifelse(births$monthBirth>=4 & births$monthBirth<=6,2,
                                ifelse(births$monthBirth>=7 & births$monthBirth<=9,3,
                                       ifelse(births$monthBirth>=10 & births$monthBirth<=12,4,"NS"))))

#sex of child
colnames(births)[colnames(births) == "Gender"] <- "sex"
births$sex[births$sex=="MALE"] <- "Male"
births$sex[births$sex=="male"] <- "Male"
births$sex[births$sex=="MAle"] <- "Male"
births$sex[births$sex=="FEMALE"] <- "Female"
births$sex[births$sex=="female"] <- "Female"

#island of births - this may not be required in the future as it is very common that more than 95% of births occur in Funafuti
colnames(births)[colnames(births) == "Island where Birth"] <- "island"
births$island[births$island=="FUNAFUTI"] <- "Funafuti"
births$island[births$island=="funafuti"] <- "Funafuti"
births$island[births$island=="FUNFUTI"] <- "Funafuti"
births$island[births$island=="Funfuti"] <- "Funafuti"
births$island[births$island=="F unafuti"] <- "Funafuti"
births$island[is.na(births$island)] <- "NS"

#age and age group of mothers
colnames(births)[colnames(births) == "DOB Mother"] <- "motherDOB"
births$motherDate <- convertToDateTime(births$motherDOB, origin = "1900-01-01")
births$motherDOBM = month(births$motherDate)
births$motherDOBY = year(births$motherDate)
births$motherAge = births$yearBirth - births$motherDOBY
births$motherAge[is.na(births$motherAge)] <- "NS"

births$ageGroup <- ifelse(births$motherAge == "NS","NS",
                          ifelse(births$motherAge<15,"<15",
                            ifelse(births$motherAge>=15 & births$motherAge<=19,"15-19",
                                 ifelse(births$motherAge>=20 & births$motherAge<=24,"20-24",
                                        ifelse(births$motherAge>=25 & births$motherAge<=29,"25-29",
                                               ifelse(births$motherAge>=30 & births$motherAge<=34,"30-34",
                                                      ifelse(births$motherAge>=35 & births$motherAge<=39, "35-39",
                                                             ifelse(births$motherAge>=40 & births$motherAge<=44,"40-44",
                                                                    ifelse(births$motherAge>=45,">45","NS")))))))))
#Place of birth
colnames(births)[colnames(births) == "Place of birth"] <- "placeBirth"
#No further cleaning required, labels appear to be okay

#Marital status of mother
colnames(births)[colnames(births) == "Marital Status"] <- "marriedStat"
births$marriedStat[births$marriedStat=="MARRIED"] <- "Married"
births$marriedStat[births$marriedStat=="Maarried"] <- "Married"
births$marriedStat[births$marriedStat=="Maried"] <- "Married"
births$marriedStat[births$marriedStat=="married"] <- "Married"
births$marriedStat[births$marriedStat=="Singlle"] <- "Single"
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
deaths$ageGroup <- ifelse(deaths$Age <5, "0-4",
                          ifelse(deaths$Age >= 5 & deaths$Age <=9,"5-9",
                                 ifelse(deaths$Age >= 10 & deaths$Age <=14,"10-14",
                                        ifelse(deaths$Age >= 15 & deaths$Age <=19,"15-19",
                                               ifelse(deaths$Age >= 20 & deaths$Age <=24,"20-24",
                                                      ifelse(deaths$Age >= 25 & deaths$Age <=29,"25-29",
                                                             ifelse(deaths$Age >= 30 & deaths$Age <=34,"30-34",
                                                                    ifelse(deaths$Age >= 35 & deaths$Age <=39,"35-39",
                                                                           ifelse(deaths$Age >= 40 & deaths$Age <=44,"40-44",
                                                                                  ifelse(deaths$Age >= 45 & deaths$Age <=49,"45-49",
                                                                                         ifelse(deaths$Age >= 50 & deaths$Age <=54,"50-54",
                                                                                                ifelse(deaths$Age >= 55 & deaths$Age <=59,"55-59",
                                                                                                       ifelse(deaths$Age >= 60 & deaths$Age <=64,"60-64",
                                                                                                              ifelse(deaths$Age >= 65 & deaths$Age <=69,"65-69",
                                                                                                                     ifelse(deaths$Age >= 70 & deaths$Age <=74,"70-74",
                                                                                                                            ifelse(deaths$Age >= 75 & deaths$Age <=79,"75-79",
                                                                                                                                   ifelse(deaths$Age >= 80 & deaths$Age <=84,"80-84",
                                                                                                                                          ifelse(deaths$Age >= 85 & deaths$Age <=89,"85-89",
                                                                                                                                                 ifelse(deaths$Age >= 90,"90+","NS")))))))))))))))))))
deaths$ageGroup[is.na(deaths$ageGroup)] <- "NS"

#island
colnames(deaths)[colnames(deaths) == "Island of occurrence"] <- "island"
deaths$island[deaths$island=="FUNAFUTI"] <- "Funafuti"
deaths$island[deaths$island=="Fuanfuti"] <- "Funafuti"
deaths$island[deaths$island=="NIUTAO"] <- "Niutao"
deaths$island[deaths$island=="NMAGA"] <- "Nanumaga"

#place of death
colnames(deaths)[colnames(deaths) == "Place of death"] <- "place"
deaths$place[deaths$place=="HOME"] <- "Home"
deaths$place[deaths$place=="Hh"] <- "Home"
deaths$place[deaths$place=="Hopsital"] <- "Hospital"
deaths$place[deaths$place=="HOSPITAL"] <- "Hospital"
deaths$place[deaths$place=="Out Island"] <- "Outer island"
deaths$place[deaths$place=="Out island"] <- "Outer island"
deaths$place[deaths$place=="Out Island "] <- "Outer island"
deaths$place[deaths$place==" Out Island"] <- "Outer island"

deaths$N <- 1
dbWriteTable(mydb, "deaths", deaths, overwrite = TRUE)