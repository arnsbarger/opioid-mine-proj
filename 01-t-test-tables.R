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

test <- mines_tables %>% group_by(County.Code) %>% summarise(sum_mines_abandoned = sum(EverAbandoned))
test$County.Type <- ifelse(test = test$sum_mines_abandoned > 0, yes = "SomeMinesAbandoned", no = "NoMinesAbandoned")

# cdc data
cdc_na %>% group_by(Year) %>% summarise(sum(!is.na(Crude.Rate))) # no clear jump; run t-test for all years

test <- cdc_na %>% select(County.Code, Year, Crude.Rate)
cdc_table_data <- reshape2::dcast(data = test, formula = County.Code ~ Year, value.var = "Crude.Rate", fun.aggregate = mean)[,1:20]
names(cdc_table_data) <- c("County.Code",paste0("Crude.Rate.",names(cdc_table_data)[-1]))

# county characteristics
acs <- read.csv("~/Documents/Pitt/Data/acs_output/acs_mine_sample.csv")
# acs <- acs[,2:ncol(acs)]
# acs$fips <- str_pad(acs$fips, 5, pad = "0")

### MERGE

data1 <- merge(x = cdc_table_data, y = acs, by.x = "County.Code", by.y = "fips", all.x = TRUE)
data2 <- merge(x = data1, y = test, by = "County.Code", all.y = TRUE)

population <- t.test(total_popB01001 ~ County.Type, data2)
population$statistic
population$p.value

results <- data.frame(variable = names(data2[,2:43]), t_stat = NA, p_value = NA)

for (i in 2:(ncol(data2)-1)) {
    
    result <- t.test(data2[,i] ~ County.Type, data2)
    results$t_stat[i-1] <- result$statistic
    results$p_value[i-1] <- result$p.value
    
}


