# randomness tables: factors that predict timing of closing vs ever closing

# libraries
library(ggplot2)
library(usmap)
library(rgdal)
library(reshape2)
library(acs)
library(dplyr)
library(stargazer)
library(haven) # read_dta()
library(corrplot)
library(Hmisc)

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

write.csv(mine_data, "~/Documents/Pitt/Data/msha_output/mine_data.csv", row.names = FALSE)
#mine_data$avg_coal_prod[is.nan(mine_data$avg_coal_prod)] <- NA


# cdc data
setwd("~/git/opioid-mine-proj/")
source("00-clean-cdc.R")
test2 <- cdc_na %>% select(County.Code, Year, Crude.Rate)
cdc_table_data <- reshape2::dcast(data = test2, formula = County.Code ~ Year, value.var = "Crude.Rate", fun.aggregate = mean)[,1:20]
names(cdc_table_data) <- c("County.Code",paste0("Crude.Rate.",names(cdc_table_data)[-1]))

# acs county characteristics data
acs <- read.csv("~/Documents/Pitt/Data/acs_output/acs_mine_sample.csv")
acs$fips <- str_pad(acs$fips, 5, pad = "0")

### MERGE

cdc_acs <- merge(x = cdc_table_data, y = acs, by.x = "County.Code", by.y = "fips", all = TRUE) # merge cdc and acs
data <- merge(x = mine_data, y = cdc_acs, by = "County.Code", all.x = TRUE) # merge county mine stats with county characteristics
table(data$County.Code) # checking how many counties have more than 1 mine
#data <- mine_data
data$distance_from_2000 <- as.numeric(data$date_closed - as.Date("2000-01-01"))  # distance in days since 2000
data$years_since_2000 <- data$distance_from_2000 / 365

# add county characteristics
icpsr <- read_dta("~/Documents/Pitt/Projects/opioid_mine_closings/ICPSR_20660/DS0001/20660-0001-Data.dta")
icpsr <- icpsr %>% select(ID, State, County, FIPS, StateName, CountyName, Division, Region, SexRatio05, MedianAge05, Fmale_MdAge05, Male_MdAge05, White05, Black05, AIAN05, UnempRate05, HouseStrs04, LowEduc04, PerstPov04, RuralUrban03, IdxCrime04, Robbery04, CrimeRate04, BushVotes04)
icpsr$FIPS <- str_pad(icpsr$FIPS, 5, pad = "0")

data <- merge(x = data, y = icpsr, by.x = "County.Code", by.y = "FIPS", all.x = TRUE)
data[,7:10][is.na(data[,7:10])] <- 0 # NA values for injuries, accidents, etc. probably means zero accidents, etc...
data$injuries_since_2000_per_avg_capita <- data$injuries_since_2000_cnt / data$avg_employees
data$accidents_since_2000_per_avg_capita <- data$accidents_since_2000_cnt / data$avg_employees
data$violations_since_2000_per_avg_capita <- data$violations_since_2000_cnt / data$avg_employees

# Determine which of these socioeconomic controls are highly correlated (looking for a set of variables that make for a well-specified regression)
cor_data <- data[,c(3:4,28,46,52:55,57:61,64,69:75,77:78,95:106,24,80)]
lapply(cor_data, class)
res <- cor(cor_data, use = "complete.obs")
round(res, 2)
corrplot(res, type = "upper", tl.col = "black", tl.srt = 45, method = "color")

res2 <- rcorr(as.matrix(cor_data))
flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
        row = rownames(cormat)[row(cormat)[ut]],
        column = rownames(cormat)[col(cormat)[ut]],
        cor  =(cormat)[ut],
        p = pmat[ut]
    )
}
flattenCorrMatrix(res2$r, res2$P)
corrplot(res2$r, type="upper", p.mat = res2$P, sig.level = 0.01, insig = "blank", tl.col = "black", tl.cex = .5)

### RANDOMNESS REGRESSIONS
#variables <- c(2:5,7:10) # only mining variables
variables <- c(3:4,7:10,46:47,58,69,77:78,88:103) # mining and social variables
#variables <- c(39:54)

# TIMING OF CLOSING - ALL DATA (SINCE 2000)

data_timing_all <- data %>% filter(ever_closed == 1) %>% select(c(variables, "years_since_2000")) # only keep desired variables and CLOSED mines
fit_timing_all <- lm(formula = years_since_2000 ~ ., data = data_timing_all)
summary(fit_timing_all)

# EVER CLOSED - ALL DATA (SINCE 2000)
data_ever_all <- data[,c(variables, 24)]
fit_ever_all <- lm(ever_closed ~ ., data_ever_all)
summary(fit_ever_all)

stargazer(fit_timing_all, fit_ever_all, df = FALSE)

# TIMING OF CLOSING - SINCE 2010
data$distance_from_2010 <- as.numeric(data$date_closed - as.Date("2010-01-01"))
data$years_since_2010 <- data$distance_from_2010 / 365
data_timing2010 <- data %>% filter(date_closed >= as.Date("2010-01-01") & ever_closed == 1) %>% select(c(variables, "years_since_2010"))

#data_timing2010 <- data2010[,c(variables, ncol(data))] 
fit_timing2010 <- lm(years_since_2010 ~ ., data_timing2010)
summary(fit_timing2010)

# EVER CLOSED - SINCE 2010
data_ever2010 <- data %>% filter(date_closed >= as.Date("2010-01-01")) %>% select(c(variables, 24))
fit_ever2010 <- lm(ever_closed ~ ., data_ever2010[,-1])
summary(fit_ever2010)

stargazer(fit_ever_all, fit_ever2010, fit_timing_all, fit_timing2010, df = FALSE)


