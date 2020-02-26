library(ggplot2)
library(ggthemes)
library(tigris)
options(tigris_use_cache = FALSE)
library(dplyr)
library(maps)
library(stringr)
library(broom)
library(sf)

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
    geom_text(aes(x = 2010.75, y = 140, label = "Mine A Closes"), color = "gray", angle = 90, size = 3) +
    geom_line() + 
    theme_few() +
    labs(x = "Year", y = "Overdoses", linetype = "", color = "") + 
    ylim(75,500)

final = ggplot(data = data %>% filter(mine %in% c("A", "B")), aes(x = x, y = y, linetype = which, group = group, color = mine)) + 
    geom_vline(xintercept = 2011, linetype = 1, color = "gray") + 
    geom_text(aes(x = 2010.75, y = 140, label = "Mine A Closes"), color = "gray", angle = 90, size = 3) +
    geom_vline(xintercept = 2015, linetype = 1, color = "gray") + 
    geom_text(aes(x = 2014.75, y = 140, label = "Mine B Closes"), color = "gray", angle = 90, size = 3) +
    geom_line() + 
    theme_few() +
    labs(x = "Year", y = "Overdoses", linetype = "", color = "") + 
    ylim(75,500)
    

ggsave("~/Documents/Pitt/Projects/opioid_mine_closings/plots/base.png", plot = base, device = "png", width = 6, height = 4)
ggsave("~/Documents/Pitt/Projects/opioid_mine_closings/plots/final.png", plot = final, device = "png", width = 6, height = 4)

# load mine_data from somewhere
#source("~/git/opioid-mine-proj/00-clean-msha-closings.R")
mine_data <- read.csv("~/Documents/Pitt/Data/msha_output/mine_data.csv")
map_data <- mine_data %>% filter(as.Date(date_closed) >= as.Date("2010-01-01")) %>% group_by(County.Code) %>% summarise(cnt_mines = length(unique(MINE_ID)), closed_mines = sum(ever_closed))
map_data$County.Code <- str_pad(map_data$County.Code, 5, pad = "0")
map_data$experienced_closing <- ifelse(test = map_data$closed_mines > 0 , yes = 1, no = 0)

#us <- fortify(map_data("county"), region = "region")
#us <- maps::map(database = 'county')
us <- tigris::counties(cb = TRUE)
us <- us[!us$STATEFP %in% c("02", "15", "72", "66", "78", "60", "69", "64", "68", "70", "74", "81", "84", "86", "87", "89", "71", "76", "95", "79"),]
us@data$id <- rownames(us@data)
us@data <- merge(us@data, map_data, by.x = "GEOID", by.y = "County.Code", all.x = TRUE)
us@data[is.na(us@data)] <- 0
us.df <- ggplot2::fortify(us)
us.df <- merge(x = us.df, y = us@data, by = "id", all.x = TRUE)

map = 
    ggplot(us.df) + 
    geom_polygon(aes(x = long, y = lat, group = group, fill = closed_mines), color = "black", size = 0.5) +
    scale_fill_continuous(high = "darkblue", low = "white") +
    labs(x = "", y = "", fill = "Count of Closed Mines") +
    coord_equal()
ggsave("~/Documents/Pitt/Projects/opioid_mine_closings/plots/map_total_closed.png", plot = map, device = "png")


map2 = 
    ggplot(us.df) + 
    geom_polygon(aes(x = long, y = lat, group = group, fill = cnt_mines), color = "black", size = 0.5) +
    scale_fill_continuous(high = "#de2d26", low = "#f5f5f5") +
    labs(x = "", y = "", fill = "Count of Closed Mines") +
    theme_minimal()
ggsave("~/Documents/Pitt/Projects/opioid_mine_closings/plots/map_total_closed.png", plot = map, device = "png", width = 6, height = 4)


map3 = 
    ggplot(us.df) + 
    geom_polygon(aes(x = long, y = lat, group = group, fill = as.factor(experienced_closing)), color = "black", size = 0.5) +
    #scale_fill_continuous(high = "#de2d26", low = "#f5f5f5") +
    labs(x = "", y = "", fill = "At least 1 Mine Closed since 2010") +
    theme_minimal()
ggsave("~/Documents/Pitt/Projects/opioid_mine_closings/plots/map_total_closed.png", plot = map, device = "png", width = 6, height = 4)






