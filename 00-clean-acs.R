library(acs)
library(dplyr)
options(scipen=999)

mycensuskey <- "58d6c5063996d0aa971852db70f8b2f8e2c060e1"
api.key.install(mycensuskey)
acs.tables.install()
counties = geo.make(county = "*", state = "*")

### total population and race by hispanic/latino
B03002 <- acs.fetch(geography=counties, table.number="B03002", endyear='2010', span=5)
B03002pretty <- acs.fetch(geography=counties, table.number="B03002", endyear='2010', span=5, col.names = "pretty")

# View(data.frame(pretty = B03002pretty@acs.colnames, colname = B03002@acs.colnames))

B03002 <- data.frame(cbind(data.frame(B03002@geography), data.frame(B03002@estimate))) %>% rowwise() %>% 
    summarize(
        fips=paste0(state, county),
        total_popB03002 = B03002_001,
        percent_hispanicB03002 = B03002_012 / B03002_001,
        percent_whiteB03002 = B03002_003 / B03002_001,
        percent_blackB03002 = B03002_004 / B03002_001,
        percent_nativeB03002 = B03002_005 / B03002_001,
        percent_asianB03002 = B03002_006 / B03002_001,
        percent_2plusracesB03002 = B03002_009 / B03002_001
    )

### median income
B19013 <- acs.fetch(geography = counties, table.number = "B19013", endyear = '2010', span = 5)
B19013pretty <- acs.fetch(geography = counties, table.number ="B19013", endyear = '2010', span = 5, col.names = "pretty")

# View(data.frame(pretty = B19013pretty@acs.colnames, colname = B19013@acs.colnames))

B19013 <- data.frame(cbind(data.frame(B19013@geography), data.frame(B19013@estimate))) %>% rowwise() %>% 
    summarize(
        fips=paste0(state, county),
        median_incomeB19013 = B19013_001,
    )


### poverty rate
B17001 <- acs.fetch(geography = counties, table.number = "B17001", endyear = '2010', span = 5)
B17001pretty <- acs.fetch(geography = counties, table.number ="B17001", endyear = '2010', span = 5, col.names = "pretty")

#View(data.frame(pretty = gsub(x = B17001pretty@acs.colnames,pattern =  "Poverty Status in the past 12 Months by Sex by Age",replacement =  ""), colname = B17001@acs.colnames))

B17001 <- data.frame(cbind(data.frame(B17001@geography), data.frame(B17001@estimate))) %>% rowwise() %>% 
    summarize(
        fips=paste0(state, county),
        total_popB17001 = B17001_001,
        total_below_pov_levelB17001 = B17001_002,
        poverty_rateB17001 = B17001_002 / B17001_001
    )


# sex
B01001 <- acs.fetch(geography = counties, table.number = "B01001", endyear = '2010', span = 5)
B01001pretty <- acs.fetch(geography = counties, table.number ="B01001", endyear = '2010', span = 5, col.names = "pretty")

#View(data.frame(pretty = gsub(x = B01001pretty@acs.colnames,pattern =  "Poverty Status in the past 12 Months by Sex by Age",replacement =  ""), colname = B01001@acs.colnames))

B01001 <- data.frame(cbind(data.frame(B01001@geography), data.frame(B01001@estimate))) %>% rowwise() %>% 
    summarize(
        fips=paste0(state, county),
        total_popB01001 = B01001_001,
        total_male = B01001_002,
        total_female = B01001_026,
        percent_male =  B01001_002 / B01001_001,
        percent_female =  B01001_026 / B01001_001
    )

# age
B01001 <- acs.fetch(geography = counties, table.number = "B01001", endyear = '2010', span = 5)
B01001pretty <- acs.fetch(geography = counties, table.number ="B01001", endyear = '2010', span = 5, col.names = "pretty")

#View(data.frame(pretty = gsub(x = B01001pretty@acs.colnames,pattern =  "Poverty Status in the past 12 Months by Sex by Age",replacement =  ""), colname = B01001@acs.colnames))

B01001 <- data.frame(cbind(data.frame(B01001@geography), data.frame(B01001@estimate))) %>% rowwise() %>% 
    summarize(
        fips=paste0(state, county),
        total_popB01001 = B01001_001,
        total_male = B01001_002,
        total_female = B01001_026,
        percent_male =  B01001_002 / B01001_001,
        percent_female =  B01001_026 / B01001_001,
        # percent_age0_19 =  (B01001_027+B01001_028+B01001_029+B01001_030+B01001_031) / B01001_001,
        # percent_age20_44 =   / B01001_001,
        # percent_age45_64 =   / B01001_001,
        # percent_age65_up =   / B01001_001
    )


data1 <- merge(B01001, B02001, by = "fips")
data2 <- merge(data1, B03002, by = "fips")
data3 <- merge(data2, B17001, by = "fips")
data4 <- merge(data3, B19013, by = "fips")

data4$fips <- str_pad(data4$fips, 5, pad = "0")
write.csv(data4, "~/Documents/Pitt/Data/acs_output/acs_mine_sample.csv", row.names = FALSE)







rm(list = ls(pattern = "pretty"))
