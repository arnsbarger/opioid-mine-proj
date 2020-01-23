# cdc wonder

library(gdata)
library(dplyr)
library(stringr)

setwd("~/Documents/Pitt/Data/cdc_multiple_cause_of_death/")

# cdc multiple cause data
cdc_deaths1999 <- read.delim("Query1999.txt")
cdc_deaths2000 <- read.delim("Query2000.txt")
cdc_deaths01_02 <- read.delim("Query2001-2002.txt")
cdc_deaths03_05 <- read.delim("Query2003-2005.txt")
cdc_deaths06_09 <- read.delim("Query2006-2009.txt")
cdc_deaths10_17 <- read.delim("Query2010-2017.txt")

cdc <- rbind(cdc_deaths1999,cdc_deaths2000,cdc_deaths01_02,cdc_deaths03_05,cdc_deaths06_09,cdc_deaths10_17)
rm(cdc_deaths1999,cdc_deaths2000,cdc_deaths01_02,cdc_deaths03_05,cdc_deaths06_09,cdc_deaths10_17)

#missing <- c("Suppressed","Missing","Unreliable")

table(cdc$Deaths) # 46243 suppressed, 116 missing
plot(density(as.numeric(cdc$Deaths), na.rm = TRUE)) # why is there such a spike around 100?

cdc$State <- str_sub(cdc$County, -2,-1)
cdc$State.Code <- str_sub(as.character(cdc$County.Code), 1, 2)

table(cdc$Year[cdc$Deaths=="Suppressed"], cdc$Year[cdc$Deaths=="Suppressed"])

cdc_deaths[cdc_deaths == "Suppressed" | cdc_deaths == "Missing" | cdc_deaths == "Unreliable"] <- NA
cdc_deaths <- cdc_deaths %>% filter(UCD...Drug.Alcohol.Induced.Code == "D")

sum(!is.na(cdc_deaths$Crude.Rate))/nrow(cdc_deaths) # only 12.6% of data is complete :(

for (i in 1:ncol(cdc_deaths)) {
    print(class(cdc_deaths[,i]))
}

cdc_deaths$Crude.Rate <- as.numeric(as.character(cdc_deaths$Crude.Rate))
cdc_deaths$Year <- as.numeric(as.character(cdc_deaths$Year))
cdc_deaths$Crude.Rate.Standard.Error <- as.numeric(as.character(cdc_deaths$Crude.Rate.Standard.Error))
cdc_deaths$Crude.Rate.Lower.95..Confidence.Interval <- as.numeric(as.character(cdc_deaths$Crude.Rate.Lower.95..Confidence.Interval))


sum(cdc_deaths$Crude.Rate>0, na.rm = TRUE) # only 7573 observations across 18 years

years <- data.frame(year = unique(cdc_deaths$Year), count = NA, percent_country = NA)

for (i in years$year) {
    years$count[i-1998] <- sum(cdc_deaths$Crude.Rate[cdc_deaths$Year == i] > 0, na.rm = TRUE)
    years$percent_country[i-1998] <- years$count[i-1998]/length(unique(cdc_deaths$County[cdc_deaths$Year == i]))
}

# between 5-20% of the country is represented in each year

min(cdc_deaths$Crude.Rate, na.rm = TRUE)
plot(density(cdc_deaths$Crude.Rate.Standard.Error[is.na(cdc_deaths$Crude.Rate)], na.rm = TRUE))
plot(density(cdc_deaths$Crude.Rate.Lower.95..Confidence.Interval[is.na(cdc_deaths$Crude.Rate)], na.rm = TRUE)) # not all rates are suppressed because they're too low... 


# next steps:
#   1) learn when CDC suppresses data (only values < a certain number? by population?)
#   2) can i "back out" the suppressed data from the confidence intervals, given CDC's suppression rules?
#       - run analysis without filled in values as robustness check if you take this route
#   3) 

