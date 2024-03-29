# load and merge mine data, other relevant data

# libraries
library(ggplot2)
library(usmap)
library(rgdal)
library(reshape2)
library(acs)
library(dplyr)
library(stargazer)
library(haven) # read_dta()
options(scipen=999)

# load data
setwd("~/git/opioid-mine-proj/")
source("00-clean-msha.R") # mines
setwd("~/git/opioid-mine-proj/")
source("00-clean-msha-closings.R") # mine_qtrly , mine_closings
setwd("~/git/opioid-mine-proj/")
source("00-clean-cdc.R") # cdc deaths data
acs <- read.csv("~/Documents/Pitt/Data/acs_output/acs_mine_sample.csv") # acs data
acs$fips <- str_pad(acs$fips, 5, pad = "0")
accidents <- read.delim("~/Documents/Pitt/Data/msha_mine_quarterly_employment_production/Accidents.txt", header = TRUE, sep = "|") # load mine accidents info (since 2000)
violations <- read.delim("~/Documents/Pitt/Data/msha_mine_quarterly_employment_production/Violations.txt", header = TRUE, sep = "|")# load mine violations info (since 2000)

mines$County.Code <- paste0(mines$BOM_STATE_CD, mines$FIPS_CNTY_CD)
mines <- mines %>% select(MINE_ID, CURRENT_MINE_NAME, County.Code)

accidents$MINE_ID <- str_pad(accidents$MINE_ID, 7, pad = "0")
violations$MINE_ID <- str_pad(violations$MINE_ID, 7, pad = "0")

acc_info <- data.frame(accidents %>% group_by(MINE_ID) %>% summarise(injuries_since_2000_cnt = sum(NO_INJURIES),
                                                                     accidents_since_2000_cnt = n()))

vio_info <- data.frame(violations %>% group_by(MINE_ID) %>% summarise(violations_since_2000_cnt = n(),
                                                                      total_cnt_affected_empl = sum(NO_AFFECTED, na.rm = TRUE)))
prod_info <- data.frame(mine_qtrly %>% group_by(MINE_ID) %>% 
                            filter(COAL_PRODUCTION > 0) %>% 
                            summarise(avg_coal_prod = mean(COAL_PRODUCTION, na.rm = TRUE),
                                      avg_employees = mean(AVG_EMPLOYEE_CNT, na.rm = TRUE),
                                      earliest_yr = min(CAL_YR)))

mine_closings <- mine_qtrly %>% group_by(MINE_ID) %>% arrange(CAL_PROD_QTR) %>% slice(which.max(rleid(AVG_EMPLOYEE_CNT)))  
mine_closings$ever_closed <- ifelse(mine_closings$AVG_EMPLOYEE_CNT > 0, 0, 1)
mine_closings$ever_closed_name <- ifelse(mine_closings$AVG_EMPLOYEE_CNT > 0, "Open", "Closed")
#mine_closings <- mine_closings %>% select(MINE_ID,CAL_YR,CAL_QTR,CAL_PROD_QTR, COAL_METAL_IND, ever_closed, ever_closed_name)

# add closing dates to mine_closings
crosswalk <- data.frame(qtr = 1:4, month = str_pad(c(03,06,09,12), 2, pad = "0"))
mine_closings <- merge(mine_closings, crosswalk, by.x = "CAL_QTR", by.y = "qtr", all.x = TRUE)

mine_closings$date_closed <- as.Date(as.character(paste0(mine_closings$CAL_YR,"-",mine_closings$month,"-","01")))
mine_closings$date_closed[mine_closings$ever_closed_name == "Open"] <- as.Date("2020-01-01")

mine_data <- merge(x = prod_info, y = mines, by = "MINE_ID", all.x = TRUE)
mine_data <- merge(x = mine_data, y = acc_info, by = "MINE_ID", all.x = TRUE)
mine_data <- merge(x = mine_data, y = vio_info, by = "MINE_ID", all.x = TRUE)
mine_data <- merge(x = mine_data, y = mine_closings, by = "MINE_ID", all.x = TRUE)

#write.csv(mine_data, "~/Documents/Pitt/Data/msha_output/mine_data.csv", row.names = FALSE)