# cleaning cdc wonder  overdose data

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

cdc$Crude.Rate <- as.numeric(as.character(cdc$Crude.Rate))
cdc$Year <- as.numeric(as.character(cdc$Year))
cdc$Crude.Rate.Standard.Error <- as.numeric(as.character(cdc$Crude.Rate.Standard.Error))
cdc$Crude.Rate.Lower.95..Confidence.Interval <- as.numeric(as.character(cdc$Crude.Rate.Lower.95..Confidence.Interval))

cdc$County.Code <- str_pad(cdc$County.Code, width = 5, side = "left", pad = "0")

cdc_na <- cdc
cdc_na[cdc_na == "Suppressed" | cdc_na == "Missing" | cdc_na == "Unreliable"] <- NA


# next steps:
#   1) learn when CDC suppresses data (only values < a certain number? by population?)
#   2) can i "back out" the suppressed data from the confidence intervals, given CDC's suppression rules?
#       - run analysis without filled in values as robustness check if you take this route
#   3) 

