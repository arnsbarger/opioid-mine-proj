# create 'mine closings' data set

# packages
library(openxlsx)
library(dplyr)

# import data 
mine_qtrly <- openxlsx::read.xlsx(xlsxFile = "MineQuartelyDB_EmploymentProduction.xlsx", 
                                  sheet = 1, 
                                  startRow = 3, 
                                  colNames = TRUE,
                                  detectDates = TRUE)
names(mine_qtrly) <- c("Prod.Year","Prod.Qtr","Mine.ID","SubunitNum","SubunitName","Quarterly.Hrs","Coal.Prod","Avg.Empl.Cnt")

mine_closings <- mine_qtrly %>% group_by(Mine.ID, SubunitNum) %>% arrange(Prod.Qtr) %>% slice(which.max(rleid(Avg.Empl.Cnt)))  

###### PRACTICE
# 
# data <- mine_qtrly %>% filter(Mine.ID == "0103381" & SubunitNum == "03")
# 
# data$rleid <- rleid(data$Avg.Empl.Cnt)
# 
# data <- mine_qtrly %>% filter(Mine.ID == "0103381")
# 
# test <- data %>% group_by(Mine.ID, SubunitNum) %>% arrange(Prod.Qtr) %>% slice(which.max(rleid(Avg.Empl.Cnt))) 
# 
# test_mine_closings <- mine_qtrly %>% group_by(Mine.ID, SubunitNum) %>% arrange(Prod.Qtr) %>% slice(which.max(rleid(Avg.Empl.Cnt)))
# 
# test_mine_closings2 <- mine_qtrly %>% group_by(Mine.ID, SubunitNum) %>% arrange(Prod.Qtr) %>% slice(which.max(rleid(Avg.Empl.Cnt))) %>% select(-Avg.Empl.Cnt) 
# 
# 
# 
# 







