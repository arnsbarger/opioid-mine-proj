library(acs)
library(dplyr)
options(scipen=999)

mycensuskey <- "58d6c5063996d0aa971852db70f8b2f8e2c060e1"
api.key.install(mycensuskey)
acs.tables.install()
counties = geo.make(county = "*", state = "*")

# population
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



















rm(list = ls(pattern = "pretty"))
