# clean msha data

library(gdata)
library(dplyr)
library(stringr)
library(openxlsx)

setwd("~/Documents/Pitt/Data/msha_mine_quarterly_employment_production/")

mine_qtrly <- openxlsx::read.xlsx(xlsxFile = "MineQuartelyDB_EmploymentProduction.xlsx", 
                                  sheet = 1, 
                                  startRow = 3, 
                                  colNames = TRUE,
                                  detectDates = TRUE)

mines <- read.csv(file = "mines.csv")
