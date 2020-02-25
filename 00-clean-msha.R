# clean msha data

library(gdata)
library(dplyr)
library(stringr)
library(openxlsx)
library(tidyr)
options(scipen=999)
# set working directory
setwd("~/Documents/Pitt/Data/msha_mine_quarterly_employment_production/")

# load data
fips <- read.csv("~/Documents/Pitt/Data/geography/ssa_fips_state_county2017.csv")
mines <- read.delim("Mines.txt")

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

# split on state (CURRENT_OPERATOR_ID and CURRENT_OPERATOR_NAME are too messy to deal with; priority is retaining FIPS info)
state_matches <- paste0("(?=",",",str_c(unique(fips$state), collapse = ",|,"),",",")")
mines <- separate(mines, X, into = c("MINE_CONTROLLER_OPERATOR_INFO","X"), sep = state_matches, extra = "merge", fill = "right")

# split next 10 clean variables on ","
mines <- separate(mines, X, into = c("X2","STATE","BOM_STATE_CD","FIPS_CNTY_CD","FIPS_CNTY_NM","CONG_DIST_CD","COMPANY_TYPE","CURRENT_CONTROLLER_BEGIN_DT","DISTRICT","OFFICE_CD","X"), sep = ",", extra = "merge", fill = "right")

# get lat/longs
mines <- separate(mines, X, into = c("MORE_MINE_INFO","X"), sep = "(?=[0-9]{2}[.][0-9]*)", extra = "merge", fill = "right")
mines <- separate(mines, X, into = c("LONGITUDE","LATITUDE","X"), sep = ",", extra = "merge", fill = "right")

rm(list=c("fips","state_matches"))
mines <- mines[ , !names(mines) %in% c("X1","X2")]

mines$CURRENT_STATUS_DT <- as.Date(mines$CURRENT_STATUS_DT, "%m/%d/%Y")
mines$CURRENT_CONTROLLER_BEGIN_DT <- as.Date(mines$CURRENT_CONTROLLER_BEGIN_DT, "%m/%d/%Y")

mines$County.Code <- paste0(mines$BOM_STATE_CD, mines$FIPS_CNTY_CD)

# NEED TO FIX COORDINATE SYSTEM EVENTUALLY
# should remove "Facility" CURRENT_MINE_TYPE (5510 obs, 6% of data)
# coordinates(mines) <- ~ LONGITUDE + LATITUDE
