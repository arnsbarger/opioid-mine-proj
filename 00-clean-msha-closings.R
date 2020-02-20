# create 'mine closings' data set

# packages
library(openxlsx)
library(dplyr)
library(stringr)
library(data.table)
options(scipen=999)
# import data 
# mine_qtrly <- openxlsx::read.xlsx(xlsxFile = "~/Documents/Pitt/Data/msha_mine_quarterly_employment_production/MineQuartelyDB.xlsx", 
#                                   sheet = 1, 
#                                   startRow = 3, 
#                                   colNames = TRUE,
#                                   detectDates = TRUE)
# names(mine_qtrly) <- c("Prod.Year","Prod.Qtr","Mine.ID","SubunitNum","SubunitName","Quarterly.Hrs","Coal.Prod","Avg.Empl.Cnt")

mine_qtrly <- read.delim("~/Documents/Pitt/Data/msha_mine_quarterly_employment_production/MinesProdQuarterly.txt",
                         header = TRUE,
                         sep = "|")

mine_qtrly$CAL_PROD_QTR <- paste0(mine_qtrly$CAL_YR, mine_qtrly$CAL_QTR)
mine_qtrly$MINE_ID <- str_pad(mine_qtrly$MINE_ID, 7, pad = "0")

mine_closings <- mine_qtrly %>% group_by(MINE_ID, SUBUNIT_CD) %>% arrange(CAL_PROD_QTR) %>% slice(which.max(rleid(AVG_EMPLOYEE_CNT)))  
mine_closings$ever_closed <- ifelse(mine_closings$AVG_EMPLOYEE_CNT > 0, 0, 1)
mine_closings$ever_closed_name <- ifelse(mine_closings$AVG_EMPLOYEE_CNT > 0, "Open", "Closed")


# paste0(names(mine_closings[,6:9]), "_CLOSED")


