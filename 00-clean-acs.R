library(acs)
library(dplyr)
options(scipen=999)

mycensuskey <- "58d6c5063996d0aa971852db70f8b2f8e2c060e1"
api.key.install(mycensuskey)
acs.tables.install()
counties = geo.make(county = "*", state = "*")

# population; total and percent by race
B02001 <- acs.fetch(geography=counties, table.number="B02001", endyear='2010', span=5)
B02001pretty <- acs.fetch(geography=counties, table.number="B02001", endyear='2010', span=5, col.names = "pretty")

View(data.frame(pretty = B02001pretty@acs.colnames, colname = B02001@acs.colnames))

B02001 <- data.frame(cbind(data.frame(B02001@geography), data.frame(B02001@estimate))) %>% rowwise() %>% 
    summarize(
        fips=paste0(state, county),
        total_popB02001 = B02001_001,
        #percent_hispanicB02001 = B02001_012 / B02001_001,
        percent_whiteB02001 = B02001_002 / B02001_001,
        percent_blackB02001 = B02001_003 / B02001_001,
        percent_asianB02001 = B02001_005 / B02001_001,
        percent_nativeB02001 = B02001_004 / B02001_001,
        percent_otherB02001 = B02001_007 / B02001_001
    )

# total pop and race by hispanic/latino
B03002 <- acs.fetch(geography=counties, table.number="B03002", endyear='2010', span=5)
B03002pretty <- acs.fetch(geography=counties, table.number="B03002", endyear='2010', span=5, col.names = "pretty")

View(data.frame(pretty = B03002pretty@acs.colnames, colname = B03002@acs.colnames))

B03002 <- data.frame(cbind(data.frame(B03002@geography), data.frame(B03002@estimate))) %>% rowwise() %>% 
    summarize(
        fips=paste0(state, county),
        total_popB03002 = B03002_001,
        percent_hispanicB03002 = B03002_012 / B03002_001,
        percent_whiteB03002 = B03002_003 / B03002_001,
        percent_blackB03002 = B03002_004 / B03002_001
    )

# median income
B19013 <- acs.fetch(geography = counties, table.number = "B19013", endyear = '2010', span = 5)
B19013pretty <- acs.fetch(geography = counties, table.number ="B19013", endyear = '2010', span = 5, col.names = "pretty")

View(data.frame(pretty = B19013pretty@acs.colnames, colname = B19013@acs.colnames))

B19013 <- data.frame(cbind(data.frame(B19013@geography), data.frame(B19013@estimate))) %>% rowwise() %>% 
    summarize(
        fips=paste0(state, county),
        median_incomeB19013 = B19013_001,
    )


# poverty rate
temp <- acs.lookup(keyword = "poverty", endyear = 2010, span = 5)
View(temp@results)

B17001 <- acs.fetch(geography = counties, table.number = "B17001", endyear = '2010', span = 5)
B17001pretty <- acs.fetch(geography = counties, table.number ="B17001", endyear = '2010', span = 5, col.names = "pretty")

View(data.frame(pretty = gsub(x = B17001pretty@acs.colnames,pattern =  "Poverty Status in the past 12 Months by Sex by Age",replacement =  ""), colname = B17001@acs.colnames))

B17001 <- data.frame(cbind(data.frame(B17001@geography), data.frame(B17001@estimate))) %>% rowwise() %>% 
    summarize(
        fips=paste0(state, county),
        total_popB17001 = B17001_001,
        total_below_pov_levelB17001 = B17001_002,
        poverty_rateB17001 = B17001_002 / B17001_001
    )














rm(list = ls(pattern = "pretty"))
