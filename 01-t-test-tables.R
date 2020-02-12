# want to show that counties with closings are systematically different from counties without closings

# libraries
library(ggplot2)
library(usmap)
library(rgdal)
library(reshape2)
library(acs)
library(dplyr)
options(scipen=999)

# load mines data frame
setwd("~/git/opioid-mine-proj/")
source("00-clean-msha.R") # mines

mines$County.Code <- paste0(mines$BOM_STATE_CD, mines$FIPS_CNTY_CD)
mines <- mines %>% select(MINE_ID, CURRENT_MINE_NAME, County.Code)

# load mine data
setwd("~/git/opioid-mine-proj/")
source("00-clean-msha-closings.R") # mine_qtrly , mine_closings
accidents <- read.delim("~/Documents/Pitt/Data/msha_mine_quarterly_employment_production/Accidents.txt", header = TRUE, sep = "|") # load mine accidents info (since 2000)
violations <- read.delim("~/Documents/Pitt/Data/msha_mine_quarterly_employment_production/Violations.txt", header = TRUE, sep = "|")# load mine violations info (since 2000)

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
mine_closings <- mine_closings %>% select(MINE_ID, CAL_PROD_QTR, COAL_METAL_IND, ever_closed, ever_closed_name)

mine_data <- merge(x = prod_info, y = mines, by = "MINE_ID", all.x = TRUE)
mine_data <- merge(x = mine_data, y = acc_info, by = "MINE_ID", all.x = TRUE)
mine_data <- merge(x = mine_data, y = vio_info, by = "MINE_ID", all.x = TRUE)
mine_data <- merge(x = mine_data, y = mine_closings, by = "MINE_ID", all.x = TRUE)

mine_data$avg_coal_prod[is.nan(mine_data$avg_coal_prod)] <- NA

county_data <- mine_data %>% group_by(County.Code) %>% summarise(num_mines = length(unique(MINE_ID)),
                                                                 avg_coal_prod_cnty = mean(avg_coal_prod, na.rm = TRUE),
                                                                 avg_emp_cnty = mean(avg_employees, na.rm = TRUE),
                                                                 earliest_yr_cnty = min(earliest_yr),
                                                                 avg_earliest_yr = mean(earliest_yr, na.rm = TRUE),
                                                                 total_injuries_since2000 = sum(injuries_since_2000_cnt),
                                                                 total_accidents_since2000 = sum(accidents_since_2000_cnt),
                                                                 total_violations_since2000 = sum(violations_since_2000_cnt),
                                                                 total_empl_affected_viol2000 = sum(total_cnt_affected_empl),
                                                                 total_ever_closed = sum(ever_closed),
                                                                 cnt_metal_mines = sum(COAL_METAL_IND == "M"),
                                                                 cnt_coal_mines = sum(COAL_METAL_IND == "C")
                                                                 )

# cdc data
setwd("~/git/opioid-mine-proj/")
source("00-clean-cdc.R")

cdc_na %>% group_by(Year) %>% summarise(sum(!is.na(Crude.Rate))) # no clear jump; run t-test for all years

test2 <- cdc_na %>% select(County.Code, Year, Crude.Rate)
cdc_table_data <- reshape2::dcast(data = test2, formula = County.Code ~ Year, value.var = "Crude.Rate", fun.aggregate = mean)[,1:20]
names(cdc_table_data) <- c("County.Code",paste0("Crude.Rate.",names(cdc_table_data)[-1]))

# acs county characteristics data
acs <- read.csv("~/Documents/Pitt/Data/acs_output/acs_mine_sample.csv")[,-1]
acs$fips <- str_pad(acs$fips, 5, pad = "0")

### MERGE

cdc_acs <- merge(x = cdc_table_data, y = acs, by.x = "County.Code", by.y = "fips", all = TRUE) # merge cdc and acs
data <- merge(x = county_data, y = cdc_acs, by = "County.Code", all = TRUE) # merge county mine stats with county characteristics

data$per_coal_mines <- data$cnt_coal_mines / data$num_mines
data$per_metal_mines <- data$cnt_metal_mines / data$num_mines
data$any_mines_closed <- ifelse(test = data$total_ever_closed > 0, yes = 1, no = 0)


# t-tests: establish differences between counties that 
results <- data.frame(variable = names(data[,2:(ncol(data)-1)]), t_stat = NA, p_value = NA)

for (i in 2:(ncol(data)-1)) {
    result <- t.test(data[,i] ~ any_mines_closed, data)
    
    results$t_stat[i-1] <- result$statistic
    
    results$p_value[i-1] <- result$p.value
    
}



