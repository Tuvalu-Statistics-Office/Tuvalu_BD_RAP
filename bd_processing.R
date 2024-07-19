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

#Dynamic directory path mapping
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

mydb <- dbConnect(RSQLite::SQLite(), "data/vital.db")

#--------------------------------------------------------------------------------------
#               BIRTHS
#--------------------------------------------------------------------------------------
births <- read_excel("data/births.xlsx")

#Date of birth, year of birth, and month of birth
colnames(births)[colnames(births) == "DOB"] <- "dob"
births$yearBirth <- year(births$dob)
births$monthBirth <- month(births$dob)
births$quarter <- ifelse(births$monthBirth>=1 & births$monthBirth <=3,1,
                         ifelse(births$monthBirth>=4 & births$monthBirth<=6,2,
                                ifelse(births$monthBirth>=7 & births$monthBirth<=9,3,4)))

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
births$island[is.na(births$island)] <- "NS"

#age and age group of mothers
colnames(births)[colnames(births) == "DOB Mother"] <- "motherDOB"
births$motherDOBM = month(births$motherDOB)
births$motherDOBY = year(births$motherDOB)
births$motherAge = births$yearBirth - births$motherDOBY
births$motherAge[is.na(births$motherAge)] <- "NS"

births$ageGroup <- ifelse(births$motherAge<15,"<15",
                          ifelse(births$motherAge>=15 & births$motherAge<=19,"15-19",
                                 ifelse(births$motherAge>=20 & births$motherAge<=24,"20-24",
                                        ifelse(births$motherAge>=25 & births$motherAge<=29,"25-29",
                                               ifelse(births$motherAge>=30 & births$motherAge<=34,"30-34",
                                                      ifelse(births$motherAge>=35 & births$motherAge<=39, "35-39",
                                                             ifelse(births$motherAge>=40 & births$motherAge<=44,"40-44",">45")))))))

#Place of birth


#Marital status of mother

#Count variable
births$N <- 1

dbWriteTable(mydb, "births", births, overwrite = TRUE)