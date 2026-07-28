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
startYear = 2018
endYear = 2025

startMonth = 1
endMonth = 12

#Staff to specify the year and quarter for population estimate tables
pop_est_year = 2025
pop_est_qtr = 1

#Connect to db
mydb <- dbConnect(RSQLite::SQLite(), "data/vital.db")
wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
t1 <- dbGetQuery(mydb, "SELECT * FROM births")
t1 <- t1 |>
  filter((yearBirth >= startYear & yearBirth <= endYear) & (monthBirth >= startMonth & monthBirth <= endMonth))
#-----------------------------------------------------
#Table B1 Births by month by year and sex
#-----------------------------------------------------
pt <- PivotTable$new()
pt$addData(t1)
pt$addColumnDataGroups("yearBirth")
pt$addColumnDataGroups("sexNormal")
pt$addRowDataGroups("monthBirth")
pt$defineCalculation(calculationName="totalBirths", summariseExpression="round(sum(N), 0)")
pt$renderPivot()
addWorksheet(wb, "tableB1")
pt$writeToExcelWorksheet(wb=wb, wsName="tableB1", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)
#-----------------------------------------------------
#Table B2 Births by age group by year and marital status of mother
#-----------------------------------------------------
pt <- PivotTable$new()
pt$addData(t1)
pt$addColumnDataGroups("yearBirth")
pt$addColumnDataGroups("marriedStat")
pt$addRowDataGroups("myageGroup")
pt$defineCalculation(calculationName="totalBirths", summariseExpression="round(sum(N), 0)")
pt$renderPivot()
addWorksheet(wb, "tableB2")
pt$writeToExcelWorksheet(wb=wb, wsName="tableB2", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#-----------------------------------------------------
#Table B3 Sex ratio by year
#-----------------------------------------------------
pt <- PivotTable$new()
pt$addData(t1)
pt$addColumnDataGroups("sexNormal")
pt$addRowDataGroups("yearBirth")
pt$defineCalculation(calculationName="totalBirths", summariseExpression="round(sum(N), 0)")
#Need code to calculate percentages and ratio
pt$renderPivot()
addWorksheet(wb, "tableB3")
pt$writeToExcelWorksheet(wb=wb, wsName="tableB3", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#-----------------------------------------------------
#Table B4 Births by weight group of child and year
#-----------------------------------------------------
pt <- PivotTable$new()
pt$addData(t1)
pt$addColumnDataGroups("yearBirth")
pt$addRowDataGroups("Birth Grp")
pt$defineCalculation(calculationName="totalBirths", summariseExpression="round(sum(N), 0)")
pt$renderPivot()
addWorksheet(wb, "tableB4")
pt$writeToExcelWorksheet(wb=wb, wsName="tableB4", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#-----------------------------------------------------
#Table B5 Home island of mother by year of birth
#-----------------------------------------------------
pt <- PivotTable$new()
pt$addData(t1)
pt$addColumnDataGroups("yearBirth")
pt$addRowDataGroups("HI")
pt$defineCalculation(calculationName="totalBirths", summariseExpression="round(sum(N), 0)")
pt$renderPivot()
addWorksheet(wb, "tableB5")
pt$writeToExcelWorksheet(wb=wb, wsName="tableB5", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#-----------------------------------------------------
#Table B6 island of birth by year of birth
#-----------------------------------------------------
pt <- PivotTable$new()
pt$addData(t1)
pt$addColumnDataGroups("yearBirth")
pt$addRowDataGroups("island")
pt$defineCalculation(calculationName="totalBirths", summariseExpression="round(sum(N), 0)")
pt$renderPivot()
addWorksheet(wb, "tableB6")
pt$writeToExcelWorksheet(wb=wb, wsName="tableB6", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#-----------------------------------------------------
#Table D1 Deaths by month by year and sex
#-----------------------------------------------------
t2 <- dbGetQuery(mydb, "SELECT * FROM deaths")
t2 <- t2 |>
  filter(yearDeath >= startYear & yearDeath <= endYear)
pt <- PivotTable$new()
pt$addData(t2)
pt$addColumnDataGroups("yearDeath")
pt$addColumnDataGroups("Sex")
pt$addRowDataGroups("monthDeath")
pt$defineCalculation(calculationName="totalDeaths", summariseExpression="round(sum(N), 0)")
pt$renderPivot()
addWorksheet(wb, "tableD1")
pt$writeToExcelWorksheet(wb=wb, wsName="tableD1", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#-----------------------------------------------------
#Table D2 Deaths by age group and sex
#-----------------------------------------------------
pt <- PivotTable$new()
pt$addData(t2)
pt$addColumnDataGroups("yearDeath")
pt$addColumnDataGroups("Sex")
pt$addRowDataGroups("myageGroup")
pt$defineCalculation(calculationName="totalDeaths", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableD2")
pt$writeToExcelWorksheet(wb=wb, wsName="tableD2", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#-----------------------------------------------------
#Table D3 Deaths by home island and sex
#-----------------------------------------------------
pt <- PivotTable$new()
pt$addData(t2)
pt$addColumnDataGroups("yearDeath")
pt$addColumnDataGroups("Sex")
pt$addRowDataGroups("HomeIsland")
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
pt$addColumnDataGroups("yearDeath")
pt$addColumnDataGroups("Sex")
pt$addRowDataGroups("placeNormal")
pt$defineCalculation(calculationName="totalDeaths", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableD4")
pt$writeToExcelWorksheet(wb=wb, wsName="tableD4", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)


#-----------------------------------------------------
#Table D5 Deaths by island of occurrence and sex
#-----------------------------------------------------
pt <- PivotTable$new()
pt$addData(t2)
pt$addColumnDataGroups("yearDeath")
pt$addColumnDataGroups("Sex")
pt$addRowDataGroups("island")
pt$defineCalculation(calculationName="totalDeaths", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableD5")
pt$writeToExcelWorksheet(wb=wb, wsName="tableD5", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#Writing tables to Excel file and save workbook
saveWorkbook(wb, file="output/Vital Tables.xlsx", overwrite = TRUE)
#-----------------------------------------------------
#Table for population estimates
#-----------------------------------------------------
wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
t1 <- t1 |>
  filter(yearBirth == pop_est_year & calc_quart == pop_est_qtr)
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
saveWorkbook(wb, file="output/pop_est_bd.xlsx", overwrite = TRUE)