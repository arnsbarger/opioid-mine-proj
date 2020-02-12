# combine data

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

# #mines <- mines %>% filter(CURRENT_MINE_STATUS %in% c("Abandoned","Abandoned and Sealed") & CURRENT_STATUS_DT >= as.Date("2010/01/01"))
# # mine data
# mines_tables <- mines %>% filter(CURRENT_MINE_STATUS %in% c("Active","Abandoned","Abandoned and Sealed"))
# mines_tables$EverAbandoned <- ifelse(test = mines_tables$CURRENT_MINE_STATUS %in% c("Abandoned","Abandoned and Sealed"), yes = 1, no = 0)
# mines_tables$County.Code <- paste0(mines_tables$BOM_STATE_CD, mines_tables$FIPS_CNTY_CD)
# 
# mines_tables_abandoned <- mines_tables %>% filter(CURRENT_MINE_STATUS %in% c("Abandoned","Abandoned and Sealed") & CURRENT_STATUS_DT >= as.Date("2010/01/01"))
# mines_tables_active <- mines_tables %>% filter(CURRENT_MINE_STATUS == "Active")
# mines_tables <- rbind(mines_tables_abandoned, mines_tables_active)
# 
# test <- mines_tables %>% group_by(County.Code) %>% summarise(sum_mines_abandoned = sum(EverAbandoned))
# test$County.Type <- ifelse(test = test$sum_mines_abandoned > 0, yes = "SomeMinesAbandoned", no = "NoMinesAbandoned")

mines$County.Code <- paste0(mines$BOM_STATE_CD, mines$FIPS_CNTY_CD)
mines <- mines %>% select(MINE_ID, CURRENT_MINE_NAME, COAL_METAL_IND,CURRENT_MINE_TYPE, CURRENT_MINE_STATUS, CURRENT_STATUS_DT, STATE,BOM_STATE_CD, FIPS_CNTY_CD, FIPS_CNTY_NM, LONGITUDE, LATITUDE, County.Code)

# load mine_qtrly and mine_closings data frames
setwd("~/git/opioid-mine-proj/")
source("00-clean-msha-closings.R") # mine_qtrly , mine_closings

prod_info <- data.frame(mine_qtrly %>% group_by(MINE_ID) %>% summarise(avg_coal_prod = mean(COAL_PRODUCTION, na.rm = TRUE),
                                                                       avg_employees = mean(AVG_EMPLOYEE_CNT, na.rm = TRUE),
                                                                       earliest_yr = min(CAL_YR)))

# load mine accidents and violations info (since 2000)
accidents <- read.delim("~/Documents/Pitt/Data/msha_mine_quarterly_employment_production/Accidents.txt", header = TRUE, sep = "|")
accidents$MINE_ID <- str_pad(accidents$MINE_ID, 7, pad = "0")
violations <- read.delim("~/Documents/Pitt/Data/msha_mine_quarterly_employment_production/Violations.txt", header = TRUE, sep = "|")
violations$MINE_ID <- str_pad(violations$MINE_ID, 7, pad = "0")

acc_info <- data.frame(accidents %>% group_by(MINE_ID) %>% summarise(injuries_since_2000_cnt = sum(NO_INJURIES),
                                                                     accidents_since_2000_cnt = n()))

vio_info <- data.frame(violations %>% group_by(MINE_ID) %>% summarise(violations_since_2000_cnt = n(),
                                                                      total_cnt_affected_empl = sum(NO_AFFECTED, na.rm = TRUE)))
mine_info <- merge(x = prod_info, y = acc_info, by = "MINE_ID", all = TRUE)
mine_info <- merge(x = mine_info, y = vio_info, by = "MINE_ID", all = TRUE)
mine_info[is.na(mine_info)] <- 0 # NA's from this merge mean this MINE_ID didn't appear in the accidents or violations dataset, so set NA values for these columns = 0 (zero accidents, zero violations)

# cdc data
setwd("~/git/opioid-mine-proj/")
source("00-clean-cdc.R")

cdc_na %>% group_by(Year) %>% summarise(sum(!is.na(Crude.Rate))) # no clear jump; run t-test for all years

test2 <- cdc_na %>% select(County.Code, Year, Crude.Rate)
cdc_table_data <- reshape2::dcast(data = test2, formula = County.Code ~ Year, value.var = "Crude.Rate", fun.aggregate = mean)[,1:20]
names(cdc_table_data) <- c("County.Code",paste0("Crude.Rate.",names(cdc_table_data)[-1]))

# county characteristics
acs <- read.csv("~/Documents/Pitt/Data/acs_output/acs_mine_sample.csv")[,-1]
acs$fips <- str_pad(acs$fips, 5, pad = "0")

### MERGE

data1 <- merge(x = cdc_table_data, y = acs, by.x = "County.Code", by.y = "fips", all.x = TRUE) # merge cdc and acs
data2 <- merge(x = mine_info, y = mines, by = "MINE_ID", all = TRUE) # merge mine_info and mines

data3 <- merge(x = data2, y = data1, by = "County.Code", all = TRUE)

mine_closings_NOTbysubunit <- mine_qtrly %>% group_by(MINE_ID) %>% arrange(CAL_PROD_QTR) %>% slice(which.max(rleid(AVG_EMPLOYEE_CNT)))  
mine_closings_NOTbysubunit$ever_closed <- ifelse(mine_closings_NOTbysubunit$AVG_EMPLOYEE_CNT > 0, 0, 1)
mine_closings_NOTbysubunit$ever_closed_name <- ifelse(mine_closings_NOTbysubunit$AVG_EMPLOYEE_CNT > 0, "Open", "Closed")

data4 <- merge(x = data3, y = mine_closings_NOTbysubunit, by = "MINE_ID", all = TRUE)

coal <- data4 %>% filter(COAL_METAL_IND.x == "C") 

test <- coal %>% group_by(County.Code) %>% summarise(num_closed_mines = sum(ever_closed))

# t-tests: establish differences between counties that 
results <- data.frame(variable = as.character(), t_stat = as.numeric(), p_value = as.numeric())
# names(coal[,c(3:9,21:61)]

for (i in c(3:9,21:61)) {
    result <- t.test(coal[,i] ~ ever_closed_name, coal)
    results <- rbind(results, c(NA, result$statistic, result$p.value))
}

results$NA_real_. <- names(coal[,c(3:9,21:61)])

data3 <- merge(x = mines_tables, y = data2, by = "County.Code", all.x = TRUE)
data3 <- data3 %>% filter(CURRENT_MINE_STATUS %in% c("Abandoned","Abandoned and Sealed"))
data3$distance_from_2010 <- data3$CURRENT_STATUS_DT - as.Date("2010-01-01")


data4 <- data3 %>% select(CURRENT_MINE_TYPE, STATE, Crude.Rate.2010, Crude.Rate.2012, Crude.Rate.2014, Crude.Rate.2016, poverty_rateB17001, median_incomeB19013, distance_from_2010)

fit <- lm(as.numeric(distance_from_2010) ~ ., data4)
summary(fit)


