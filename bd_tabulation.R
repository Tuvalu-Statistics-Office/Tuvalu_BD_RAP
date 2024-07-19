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

t1 <- dbGetQuery(mydb, "SELECT * FROM births WHERE quarter = 1")
pt <- PivotTable$new()
pt$addData(t1)
pt$addColumnDataGroups("sex")
pt$addRowDataGroups("monthBirth")
pt$defineCalculation(calculationName="TotalVisitors", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()

pt <- PivotTable$new()
pt$addData(t1)
pt$addColumnDataGroups("sex")
pt$defineCalculation(calculationName="TotalVisitors", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()

pt <- PivotTable$new()
pt$addData(t1)
pt$addColumnDataGroups("sex")
pt$addRowDataGroups("ageGroup")
pt$defineCalculation(calculationName="TotalVisitors", summariseExpression="format(round(sum(N), 0), big.mark = ',')")
pt$renderPivot()