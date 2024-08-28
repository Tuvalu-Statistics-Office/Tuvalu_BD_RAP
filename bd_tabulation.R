#Lae Peleti
#Senior Statistician
#Central Statistics Division
#Ministry of Finance and Economic Development
#Government of Tuvalu

#Load required libraries
library(dplyr)
library(readxl)
library(openxlsx)
library(tidyr)
library(RSQLite)
library(lubridate) #Date conversions and manipulations
library(officer)
library(tidyverse)
library(ggplot2)
library(pivottabler)

#Dynamic directory path mapping
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#Declare variables to pass year and month for processing
#Staff to specify the year and month for tables
startYear = 2024
endYear = 2024

startMonth = 1
endMonth = 6

#Staff to specify the year and quarter for population estimate tables
pop_est_year = 2024
pop_est_qtr = 2

#Connect to db
mydb <- dbConnect(RSQLite::SQLite(), "data/vital.db")
wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
t1 <- dbGetQuery(mydb, "SELECT * FROM births")
t1 <- t1 |>
  filter((yearBirth >= startYear & yearBirth <= endYear) & (monthBirth >= startMonth & monthBirth <= endMonth))
#-----------------------------------------------------
#Table B1 Births by month and sex
#-----------------------------------------------------
pt <- PivotTable$new()
pt$addData(t1)
pt$addColumnDataGroups("sexNormal")
pt$addRowDataGroups("monthBirth")
pt$defineCalculation(calculationName="totalBirths", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableB1")
pt$writeToExcelWorksheet(wb=wb, wsName="tableB1", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)
#-----------------------------------------------------
#Table B2 Births by island and sex
#-----------------------------------------------------
pt <- PivotTable$new()
pt$addData(t1)
pt$addColumnDataGroups("sexNormal")
pt$addRowDataGroups("islandNormal")
pt$defineCalculation(calculationName="totalBirths", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableB2")
pt$writeToExcelWorksheet(wb=wb, wsName="tableB2", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#-----------------------------------------------------
#Table B3 Births by age group and marital status
#-----------------------------------------------------
pt <- PivotTable$new()
pt$addData(t1)
pt$addColumnDataGroups("normalMarriedStat")
pt$addRowDataGroups("myageGroup")
pt$defineCalculation(calculationName="totalBirths", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableB3")
pt$writeToExcelWorksheet(wb=wb, wsName="tableB3", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#-----------------------------------------------------
#Table B4 Births by age month and place of birth
#-----------------------------------------------------
pt <- PivotTable$new()
pt$addData(t1)
pt$addColumnDataGroups("normalPlaceBirth")
pt$addRowDataGroups("monthBirth")
pt$defineCalculation(calculationName="totalBirths", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableB4")
pt$writeToExcelWorksheet(wb=wb, wsName="tableB4", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#-----------------------------------------------------
#Table B5 Births by age month and marital status
#-----------------------------------------------------
pt <- PivotTable$new()
pt$addData(t1)
pt$addColumnDataGroups("normalMarriedStat")
pt$addRowDataGroups("monthBirth")
pt$defineCalculation(calculationName="totalBirths", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableB5")
pt$writeToExcelWorksheet(wb=wb, wsName="tableB5", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#-----------------------------------------------------
#Table D1 Deaths by month and sex
#-----------------------------------------------------
t2 <- dbGetQuery(mydb, "SELECT * FROM deaths")
t2 <- t2 |>
  filter((yearDeath >= startYear & yearBirth <= endYear) & (monthDeath >= startMonth & monthDeath <= endMonth))
pt <- PivotTable$new()
pt$addData(t2)
pt$addColumnDataGroups("Sex")
pt$addRowDataGroups("monthDeath")
pt$defineCalculation(calculationName="totalDeaths", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableD1")
pt$writeToExcelWorksheet(wb=wb, wsName="tableD1", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#-----------------------------------------------------
#Table D2 Deaths by age group and sex
#-----------------------------------------------------
pt <- PivotTable$new()
pt$addData(t2)
pt$addColumnDataGroups("Sex")
pt$addRowDataGroups("myageGroup")
pt$defineCalculation(calculationName="totalDeaths", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableD2")
pt$writeToExcelWorksheet(wb=wb, wsName="tableD2", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#-----------------------------------------------------
#Table D3 Deaths by island of occurrence and sex
#-----------------------------------------------------
pt <- PivotTable$new()
pt$addData(t2)
pt$addColumnDataGroups("Sex")
pt$addRowDataGroups("islandNormal")
pt$defineCalculation(calculationName="totalDeaths", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableD3")
pt$writeToExcelWorksheet(wb=wb, wsName="tableD3", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#-----------------------------------------------------
#Table D4 Deaths by place of death and sex
#-----------------------------------------------------
pt <- PivotTable$new()
pt$addData(t2)
pt$addColumnDataGroups("Sex")
pt$addRowDataGroups("placeNormal")
pt$defineCalculation(calculationName="totalDeaths", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableD4")
pt$writeToExcelWorksheet(wb=wb, wsName="tableD4", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

saveWorkbook(wb, file="output/Vital Tables.xlsx", overwrite = TRUE)
#-----------------------------------------------------
#Table for population estimates
#-----------------------------------------------------
wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
t1 <- t1 |>
  filter(yearBirth == pop_est_year & quarter == pop_est_qtr)
pt <- PivotTable$new()
pt$addData(t1)
pt$addColumnDataGroups("sexNormal")
pt$defineCalculation(calculationName="totalBirths", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "births")
pt$writeToExcelWorksheet(wb=wb, wsName="births", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

t2 <- t2 |>
  filter(yearDeath == pop_est_year & quarter == pop_est_qtr)
pt <- PivotTable$new()
pt$addData(t2)
pt$addColumnDataGroups("Sex")
pt$addRowDataGroups("Age")
pt$defineCalculation(calculationName="totalDeaths", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "deaths")
pt$writeToExcelWorksheet(wb=wb, wsName="deaths", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)
saveWorkbook(wb, file="output/popest.xlsx", overwrite = TRUE)