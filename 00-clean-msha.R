# clean msha data

library(gdata)
library(dplyr)
library(stringr)
library(openxlsx)
library(tidyr)

setwd("~/Documents/Pitt/Data/msha_mine_quarterly_employment_production/")

fips <- read.csv("~/Documents/Pitt/Data/geography/ssa_fips_state_county2017.csv")
# abrv <- read.csv("~/Documents/Pitt/Data/geography/usps_state_abrv_crosswalk.csv")
# fips <- merge(x = fips, y = abrv, by.x = "state", by.y = "Abbreviation", all.x = TRUE)
# fips <- select(fips, c("state","county","fipscounty","fipsstate","State"))

# mine_qtrly <- openxlsx::read.xlsx(xlsxFile = "MineQuartelyDB_EmploymentProduction.xlsx", 
#                                   sheet = 1, 
#                                   startRow = 3, 
#                                   colNames = TRUE,
#                                   detectDates = TRUE)

# mines <- read.delim("Mines.txt", header = TRUE, sep = "|")
# valid_CMS <- c("Active","Abandoned","Abandoned and Sealed","NonProducing","Temporarily Idled","Intermittent","New Mine")
# dim(mines %>% filter(!CURRENT_MINE_STATUS %in% valid_CMS)) # 280  59 # 131  59\

mines <- read.delim("~/Downloads/Mines.txt")
var_names <- c("MINE_ID","CURRENT_MINE_NAME","COAL_METAL_IND","CURRENT_MINE_TYPE","CURRENT_MINE_STATUS","CURRENT_STATUS_DT","CURRENT_CONTROLLER_ID","CURRENT_CONTROLLER_NAME","CURRENT_OPERATOR_ID","CURRENT_OPERATOR_NAME","STATE","BOM_STATE_CD","FIPS_CNTY_CD","FIPS_CNTY_NM","CONG_DIST_CD","COMPANY_TYPE","CURRENT_CONTROLLER_BEGIN_DT","DISTRICT","OFFICE_CD","OFFICE_NAME","ASSESS_CTRL_NO","PRIMARY_SIC_CD","PRIMARY_SIC","PRIMARY_SIC_CD_1","PRIMARY_SIC_CD_SFX","SECONDARY_SIC_CD","SECONDARY_SIC","SECONDARY_SIC_CD_1","SECONDARY_SIC_CD_SFX","PRIMARY_CANVASS_CD","PRIMARY_CANVASS","SECONDARY_CANVASS_CD","SECONDARY_CANVASS","CURRENT_103I","CURRENT_103I_DT","PORTABLE_OPERATION","PORTABLE_FIPS_ST_CD","DAYS_PER_WEEK","HOURS_PER_SHIFT","PROD_SHIFTS_PER_DAY","MAINT_SHIFTS_PER_DAY","NO_EMPLOYEES","PART48_TRAINING","LONGITUDE","LATITUDE","AVG_MINE_HEIGHT","MINE_GAS_CATEGORY_CD","METHANE_LIBERATION","NO_PRODUCING_PITS","NO_NONPRODUCING_PITS","NO_TAILING_PONDS","PILLAR_RECOVERY_USED","HIGHWALL_MINER_USED","MULTIPLE_PITS","MINERS_REP_IND","SAFETY_COMMITTEE_IND","MILES_FROM_OFFICE","DIRECTIONS_TO_MINE","NEAREST_TOWN")

mines[,1] <- as.character(mines[,1])

data <- data.frame(mines[1:20,])

data <- data.frame(str_split_fixed(string = data$mines.1.20..., pattern = ",", n = 2))
data[,2:3] <- data.frame(str_split_fixed(string = data$X2, pattern = ",C,|,M,", n = 2)) # We lose this Coal/Metal variable here... may want to split a different way eventually

data <- separate(data, X2.1, into = paste0("X",3:9), sep = ",", extra = "merge", fill = "right")

state_matches <- str_c(unique(fips$state), collapse = "|")

data <- separate(data, X9, into = c("X9","X10"), sep = state_matches, extra = "merge", fill = "right")

data <- separate(data, X10, into = paste0("X",10:30), sep = ",", extra = "merge", fill = "right")
