# clean msha data

library(gdata)
library(dplyr)
library(stringr)
library(openxlsx)
library(tidyr)

# set working directory
setwd("~/Documents/Pitt/Data/msha_mine_quarterly_employment_production/")

# load data
fips <- read.csv("~/Documents/Pitt/Data/geography/ssa_fips_state_county2017.csv")
mines <- read.delim("~/Downloads/Mines.txt")

# save variable names
#var_names <- c("MINE_ID","CURRENT_MINE_NAME","COAL_METAL_IND","CURRENT_MINE_TYPE","CURRENT_MINE_STATUS","CURRENT_STATUS_DT","CURRENT_CONTROLLER_ID","CURRENT_CONTROLLER_NAME","CURRENT_OPERATOR_ID","CURRENT_OPERATOR_NAME","STATE","BOM_STATE_CD","FIPS_CNTY_CD","FIPS_CNTY_NM","CONG_DIST_CD","COMPANY_TYPE","CURRENT_CONTROLLER_BEGIN_DT","DISTRICT","OFFICE_CD","OFFICE_NAME","ASSESS_CTRL_NO","PRIMARY_SIC_CD","PRIMARY_SIC","PRIMARY_SIC_CD_1","PRIMARY_SIC_CD_SFX","SECONDARY_SIC_CD","SECONDARY_SIC","SECONDARY_SIC_CD_1","SECONDARY_SIC_CD_SFX","PRIMARY_CANVASS_CD","PRIMARY_CANVASS","SECONDARY_CANVASS_CD","SECONDARY_CANVASS","CURRENT_103I","CURRENT_103I_DT","PORTABLE_OPERATION","PORTABLE_FIPS_ST_CD","DAYS_PER_WEEK","HOURS_PER_SHIFT","PROD_SHIFTS_PER_DAY","MAINT_SHIFTS_PER_DAY","NO_EMPLOYEES","PART48_TRAINING","LONGITUDE","LATITUDE","AVG_MINE_HEIGHT","MINE_GAS_CATEGORY_CD","METHANE_LIBERATION","NO_PRODUCING_PITS","NO_NONPRODUCING_PITS","NO_TAILING_PONDS","PILLAR_RECOVERY_USED","HIGHWALL_MINER_USED","MULTIPLE_PITS","MINERS_REP_IND","SAFETY_COMMITTEE_IND","MILES_FROM_OFFICE","DIRECTIONS_TO_MINE","NEAREST_TOWN")

# factor to character class
mines[,1] <- as.character(mines[,1])
names(mines) <- "START"

# isolate Mine_ID
mines <- separate(mines, START, into = c("MINE_ID","X"), sep = ",", extra = "merge", fill = "right")

# split at COAL_METAL_IND to isolate CURRENT_MINE_NAME
mines <- separate(mines, X, into = c("CURRENT_MINE_NAME","X"), sep = "(?=,C,|,M,)", extra = "merge", fill = "right")

# split by "," up to CURRENT_STATUS_DT
mines <- separate(mines, X, into = c("X1","COAL_METAL_IND","CURRENT_MINE_TYPE","CURRENT_MINE_STATUS","CURRENT_STATUS_DT","X"), sep = ",", extra = "merge", fill = "right")

# split by CURRENT_OPERATOR_ID
#mines <- separate(mines, X, into = c("CURRENT_CONTROLLER_NAME","X"), sep = "(?=,[A-Z]*[0-9]{5},|,[A-Z]*[0-9]{6},|,[0-9]{7},)", extra = "merge", fill = "right")

# split on state (CURRENT_OPERATOR_ID and CURRENT_OPERATOR_NAME are too messy to deal with; priority is retaining FIPS info)
state_matches <- paste0("(?=",",",str_c(unique(fips$state), collapse = ",|,"),",",")")
mines <- separate(mines, X, into = c("MINE_CONTROLLER_OPERATOR_INFO","X"), sep = state_matches, extra = "merge", fill = "right")

# 
mines <- separate(mines, X, into = c("X2","STATE","BOM_STATE_CD","FIPS_CNTY_CD","FIPS_CNTY_NM","CONG_DIST_CD","COMPANY_TYPE","CURRENT_CONTROLLER_BEGIN_DT","DISTRICT","OFFICE_CD","X"), sep = ",", extra = "merge", fill = "right")

################# PRACTICE ################
current_control_nm_commas <- test %>% filter(str_detect(CURRENT_CONTROLLER_NAME, ","))
data <- mines
data2 <- mines[60:100,]

CURRENT_OPERATOR_ID <- gsub(str_extract(data2$X7, pattern = ",[A-Z]*[0-9]{5},|,[A-Z]*[0-9]{6},|,[0-9]{7},"), pattern = ",", replacement = "")
data2 <- separate(data2, X7, into = paste0("X",7:8), sep = "(?<=,[A-Z]*[0-9]{5},|,[A-Z]*[0-9]{6},|,[0-9]{7},", extra = "merge", fill = "right")

table(test$X11)


mines <- separate(mines, X9, into = c("X9","X10"), sep = state_matches, extra = "merge", fill = "right")

mines <- separate(mines, X10, into = paste0("X",10:30), sep = ",", extra = "merge", fill = "right")
