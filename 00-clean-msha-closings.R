# create 'mine closings' data set

# packages
library(openxlsx)
library(dplyr)

# import data 
mine_qtrly <- openxlsx::read.xlsx(xlsxFile = "~/Documents/Pitt/Data/msha_mine_quarterly_employment_production/MineQuartelyDB.xlsx", 
                                  sheet = 1, 
                                  startRow = 3, 
                                  colNames = TRUE,
                                  detectDates = TRUE)
names(mine_qtrly) <- c("Prod.Year","Prod.Qtr","Mine.ID","SubunitNum","SubunitName","Quarterly.Hrs","Coal.Prod","Avg.Empl.Cnt")

mine_closings <- mine_qtrly %>% group_by(Mine.ID, SubunitNum) %>% arrange(Prod.Qtr) %>% slice(which.max(rleid(Avg.Empl.Cnt)))  

mine_closings$status <- ifelse(mine_closings$Avg.Empl.Cnt > 0, 0, 1)
mine_closings$status_name <- ifelse(mine_closings$Avg.Empl.Cnt > 0, "Open", "Closed")





