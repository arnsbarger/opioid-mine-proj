# combine data

# libraries
library(ggplot2)
library(usmap)
library(rgdal)
library(reshape2)
library(acs)
library(dplyr)

# load mines data frame
setwd("~/git/opioid-mine-proj/")
source("00-clean-cdc.R")
setwd("~/git/opioid-mine-proj/")
source("00-clean-msha.R") # mines

#mines <- mines %>% filter(CURRENT_MINE_STATUS %in% c("Abandoned","Abandoned and Sealed") & CURRENT_STATUS_DT >= as.Date("2010/01/01"))
# mine data
mines_tables <- mines %>% filter(CURRENT_MINE_STATUS %in% c("Active","Abandoned","Abandoned and Sealed"))
mines_tables$EverAbandoned <- ifelse(test = mines_tables$CURRENT_MINE_STATUS %in% c("Abandoned","Abandoned and Sealed"), yes = 1, no = 0)
mines_tables$County.Code <- paste0(mines_tables$BOM_STATE_CD, mines_tables$FIPS_CNTY_CD)

# cdc data
cdc_na %>% group_by(Year) %>% summarise(sum(!is.na(Crude.Rate))) # no clear jump; run t-test for all years

test <- cdc_na %>% select(County.Code, Year, Crude.Rate)
cdc_table_data <- reshape2::dcast(data = test, formula = County.Code ~ Year, value.var = "Crude.Rate", fun.aggregate = mean)
names(cdc_table_data) <- c("County.Code",paste0("Crude.Rate.",names(cdc_table_data)[-1]))

# county characteristics




