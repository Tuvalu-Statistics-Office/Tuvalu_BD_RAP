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
t1 <- dbGetQuery(mydb, "SELECT * FROM births WHERE quarter = 1")

#-----------------------------------------------------
#Table 1 Births by month and sex
#-----------------------------------------------------
pt <- PivotTable$new()
pt$addData(t1)
pt$addColumnDataGroups("sex")
pt$addRowDataGroups("monthBirth")
pt$defineCalculation(calculationName="TotalVisitors", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableB1")
pt$writeToExcelWorksheet(wb=wb, wsName="tableB1", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)
#-----------------------------------------------------
#Table 2 Births by island and sex
#-----------------------------------------------------
pt <- PivotTable$new()
pt$addData(t1)
pt$addColumnDataGroups("sex")
pt$addRowDataGroups("island")
pt$defineCalculation(calculationName="TotalVisitors", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableB2")
pt$writeToExcelWorksheet(wb=wb, wsName="tableB2", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#-----------------------------------------------------
#Table 3 Births by age group and marital status
#-----------------------------------------------------
pt <- PivotTable$new()
pt$addData(t1)
pt$addColumnDataGroups("marriedStat")
pt$addRowDataGroups("ageGroup")
pt$defineCalculation(calculationName="TotalVisitors", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableB3")
pt$writeToExcelWorksheet(wb=wb, wsName="tableB3", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#-----------------------------------------------------
#Table 4 Births by age month and place of birth
#-----------------------------------------------------
pt <- PivotTable$new()
pt$addData(t1)
pt$addColumnDataGroups("placeBirth")
pt$addRowDataGroups("monthBirth")
pt$defineCalculation(calculationName="TotalVisitors", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableB4")
pt$writeToExcelWorksheet(wb=wb, wsName="tableB4", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

#-----------------------------------------------------
#Table 5 Births by age month and marital status
#-----------------------------------------------------
pt <- PivotTable$new()
pt$addData(t1)
pt$addColumnDataGroups("marriedStat")
pt$addRowDataGroups("monthBirth")
pt$defineCalculation(calculationName="TotalVisitors", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()
addWorksheet(wb, "tableB5")
pt$writeToExcelWorksheet(wb=wb, wsName="tableB5", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

saveWorkbook(wb, file="output/Births Tables.xlsx", overwrite = TRUE)