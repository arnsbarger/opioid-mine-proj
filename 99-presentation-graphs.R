library(ggplot2)
library(ggthemes)
library(tigris)
library(dplyr)

x = seq(2000,2020,1)

run = x -2000

preA = 10*run + 100
postA = 30*run + 100
A = c(preA[1:12], postA[13:21] - preA[13])

A = data.frame(x = x, y = c(preA[1:12], postA[13:21] - preA[13]), mine = "A", which = "Actual")
A_counter = data.frame(x = x, y = c(rep(NA,11), preA[12:21]), mine = "A", which = "Counterfactual")

bad_control = data.frame(x = x, y = 2*run + 200, mine = "Never Close", which = "Never Close")

preB = 10*run + 70
postB = 30*run -230
B = data.frame(x = x, y = c(preB[1:16], postB[17:21]) + 20, mine = "B", which = "Actual")

data <- rbind(A, B, A_counter, bad_control)
data$group = paste0(data$mine, data$which)
# dates <- data.frame(mine = c("A","B"), date = c(2011, 2015),y = 0, event = c("Mine A Closes", "Mine B Closes"))

# base
base = ggplot(data = data %>% filter(mine == "A"), aes(x = x, y = y, linetype = which, color = mine)) + 
    geom_vline(xintercept = 2011, linetype = 1, color = "gray") + 
    geom_text(aes(x = 2010.75, y = 120, label = "Mine A Closes"), color = "gray", angle = 90, size = 3) +
    geom_line() + 
    theme_few() +
    labs(x = "Year", y = "Overdoses", linetype = "", color = "")

never_close = ggplot(data = data %>% filter(mine != "B"), aes(x = x, y = y, linetype = which, group = group, color = mine)) + geom_line() + theme_few()

final = ggplot(data = data %>% filter(mine %in% c("A", "B")), aes(x = x, y = y, linetype = which, group = group, color = mine)) + 
    geom_vline(xintercept = 2011, linetype = 1, color = "gray") + 
    geom_text(aes(x = 2010.75, y = 120, label = "Mine A Closes"), color = "gray", angle = 90, size = 3) +
    geom_vline(xintercept = 2015, linetype = 1, color = "gray") + 
    geom_text(aes(x = 2014.75, y = 120, label = "Mine B Closes"), color = "gray", angle = 90, size = 3) +
    geom_line() + 
    theme_few() +
    labs(x = "Year", y = "Overdoses", linetype = "", color = "")
    


# load mine_data from somewhere
map_data <- data.frame(table(mine_data$County.Code))
shp <- counties(state = NULL, cb = FALSE, resolution = "500k", year = 2017)
plot(shp)

library("maps")
us <- fortify(map_data("state"), region = "region")
ggplot() +
    geom_map(data  =  us, map = us,
             aes(x = long, y = lat, map_id = region, group = group),
             fill = "white", color = "black", size = 0.25) +
    coord_map("albers", lat0 = 39, lat1 = 45) +
    theme_map()
