# randomness tables: factors that predict timing of closing vs ever closing

data3 <- merge(x = mines_tables, y = data2, by = "County.Code", all.x = TRUE)
data3 <- data3 %>% filter(CURRENT_MINE_STATUS %in% c("Abandoned","Abandoned and Sealed"))
data3$distance_from_2010 <- data3$CURRENT_STATUS_DT - as.Date("2010-01-01")


data4 <- data3 %>% select(CURRENT_MINE_TYPE, STATE, Crude.Rate.2010, Crude.Rate.2012, Crude.Rate.2014, Crude.Rate.2016, poverty_rateB17001, median_incomeB19013, distance_from_2010)

fit <- lm(as.numeric(distance_from_2010) ~ ., data4)
summary(fit)

