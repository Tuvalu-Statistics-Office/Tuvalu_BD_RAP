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

#Connect to db
mydb <- dbConnect(RSQLite::SQLite(), "data/vital.db")
wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
t1 <- dbGetQuery(mydb, "SELECT * FROM births WHERE monthBirth <= 6")

#-----------------------------------------------------
#Table B1 Births by month and sex
#-----------------------------------------------------
pt <- PivotTable$new()
pt$addData(t1)
pt$addColumnDataGroups("sex")
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
pt$addColumnDataGroups("sex")
pt$addRowDataGroups("island")
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
pt$addColumnDataGroups("marriedStat")
pt$addRowDataGroups("ageGroup")
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
pt$addColumnDataGroups("placeBirth")
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
pt$addColumnDataGroups("marriedStat")
pt$addRowDataGroups("monthBirth")
pt$defineCalculation(calculationName="totalBirths", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableB5")
pt$writeToExcelWorksheet(wb=wb, wsName="tableB5", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#-----------------------------------------------------
#Table D1 Deaths by month and sex
#-----------------------------------------------------
t1 <- dbGetQuery(mydb, "SELECT * FROM deaths WHERE monthDeath <= 6")
pt <- PivotTable$new()
pt$addData(t1)
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
t1 <- dbGetQuery(mydb, "SELECT * FROM deaths WHERE monthDeath <= 6")
pt <- PivotTable$new()
pt$addData(t1)
pt$addColumnDataGroups("Sex")
pt$addRowDataGroups("ageGroup")
pt$defineCalculation(calculationName="totalDeaths", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableD2")
pt$writeToExcelWorksheet(wb=wb, wsName="tableD2", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#-----------------------------------------------------
#Table D3 Deaths by island of occurrence and sex
#-----------------------------------------------------
t1 <- dbGetQuery(mydb, "SELECT * FROM deaths WHERE monthDeath <= 6")
pt <- PivotTable$new()
pt$addData(t1)
pt$addColumnDataGroups("Sex")
pt$addRowDataGroups("island")
pt$defineCalculation(calculationName="totalDeaths", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableD3")
pt$writeToExcelWorksheet(wb=wb, wsName="tableD3", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#-----------------------------------------------------
#Table D4 Deaths by place of death and sex
#-----------------------------------------------------
t1 <- dbGetQuery(mydb, "SELECT * FROM deaths WHERE monthDeath <= 6")
pt <- PivotTable$new()
pt$addData(t1)
pt$addColumnDataGroups("Sex")
pt$addRowDataGroups("place")
pt$defineCalculation(calculationName="totalDeaths", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableD4")
pt$writeToExcelWorksheet(wb=wb, wsName="tableD4", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

saveWorkbook(wb, file="output/Vital Tables.xlsx", overwrite = TRUE)