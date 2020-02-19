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


# sex and age
B01001 <- acs.fetch(geography = counties, table.number = "B01001", endyear = '2010', span = 5)
B01001pretty <- acs.fetch(geography = counties, table.number ="B01001", endyear = '2010', span = 5, col.names = "pretty")

data.frame(pretty = B01001pretty@acs.colnames, colname = B01001@acs.colnames)

B01001 <- data.frame(cbind(data.frame(B01001@geography), data.frame(B01001@estimate))) %>% rowwise() %>% 
    summarize(
        fips=paste0(state, county),
        total_popB01001 = B01001_001,
        total_male = B01001_002,
        total_female = B01001_026,
        percent_male =  B01001_002 / B01001_001,
        percent_female =  B01001_026 / B01001_001,
        percent_male_0_19 = (B01001_003 + B01001_004 + B01001_005 + B01001_006 + B01001_007) / B01001_001,
        percent_male_20_44 = (B01001_008 + B01001_009 + B01001_010 + B01001_011 + B01001_012 + B01001_013 + B01001_014) / B01001_001,
        percent_male_45_64 = (B01001_015 + B01001_016 + B01001_017 + B01001_018 + B01001_019) / B01001_001,
        percent_male_65_plus = (B01001_020 + B01001_021 + B01001_022 + B01001_023 + B01001_024 + B01001_025) / B01001_001
    )

# marital status by sex
B12001 <- acs.fetch(geography = counties, table.number = "B12001", endyear = '2010', span = 5)
B12001pretty <- acs.fetch(geography = counties, table.number ="B12001", endyear = '2010', span = 5, col.names = "pretty")

data.frame(pretty = gsub(x = B12001pretty@acs.colnames,pattern =  "Sex by Marital Status for the Population 15 Years and",replacement =  ""), colname = B12001@acs.colnames)

B12001 <- data.frame(cbind(data.frame(B12001@geography), data.frame(B12001@estimate))) %>% rowwise() %>% 
    summarize(
        fips=paste0(state, county),
        #total_popB12001 = B12001_001,
        total_maleB12001 = B12001_002,
        #total_female = B12001_011,
        percent_male_married = B12001_004 / B12001_002,
        percent_male_never_married =  B12001_003 / B12001_002,
        percent_male_divorced = B12001_010 / B12001_002,
        percent_male_widowed = B12001_009 / B12001_002
    )

# education by sex
B15002 <- acs.fetch(geography = counties, table.number = "B15002", endyear = '2010', span = 5)
B15002pretty <- acs.fetch(geography = counties, table.number ="B15002", endyear = '2010', span = 5, col.names = "pretty")

data.frame(pretty = B15002pretty@acs.colnames, colname = B15002@acs.colnames)

B15002 <- data.frame(cbind(data.frame(B15002@geography), data.frame(B15002@estimate))) %>% rowwise() %>% 
    summarize(
        fips=paste0(state, county),
        #total_popB12001 = B12001_001,
        total_maleB12001 = B15002_002,
        #total_female = B12001_011,
        percent_male_hs_dropout = (B15002_007 + B15002_008 + B15002_009 + B15002_010) / B15002_002,
        percent_male_hs =  B15002_011 / B15002_002,
        percent_male_some_college = (B15002_012 + B15002_013) / B15002_002,
        percent_male_college_grad = B15002_009 / B15002_002,
        percent_male_associates = B15002_014 / B15002_002,
        percent_male_bachelors = B15002_015 / B15002_002,
        percent_male_grad = (B15002_016 + B15002_017 + B15002_018) / B15002_002
        # percent_age0_19 =  (B01001_027+B01001_028+B01001_029+B01001_030+B01001_031) / B01001_001,
        # percent_age20_44 =   / B01001_001,
        # percent_age45_64 =   / B01001_001,
        # percent_age65_up =   / B01001_001
    )




data1 <- merge(B01001, B03002, by = "fips")
data2 <- merge(data1, B12001, by = "fips")
data3 <- merge(data2, B15002, by = "fips")
data4 <- merge(data3, B17001, by = "fips")
data5 <- merge(data4, B19013, by = "fips")

data5$fips <- str_pad(data5$fips, 5, pad = "0")
write.csv(data4, "~/Documents/Pitt/Data/acs_output/acs_mine_sample.csv", row.names = FALSE)







rm(list = ls(pattern = "pretty"))
