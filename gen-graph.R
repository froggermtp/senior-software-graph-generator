library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)

setwd("C:\\Users\\Matthew\\Documents\\TU\\Senior Software 1\\Sprint 5")

minutes <- read.csv("minutes.csv") %>%
  mutate(Month = as.numeric(str_extract(Date, "^\\d+"))) %>%
  mutate(Day = as.numeric(str_extract(Date, "\\d+$"))) %>%
  mutate(Date = make_datetime(year = 2020, month = Month, day = Day)) %>%
  group_by(Date) %>%
  summarize(N = n(), Total_Hours = sum(Man.Hours)) %>%
  add_row(Date = first(minutes$Date) - 1, N = 0, Total_Hours = 0) %>%
  arrange(Date)

goal_line = data.frame(x_ = c(first(minutes$Date), last(minutes$Date)), y_ = c(sum(minutes$Total_Hours), 0))

graph <- ggplot(data = minutes, aes(x = Date, y = sum(Total_Hours) - cumsum(Total_Hours))) +
  geom_line(data = goal_line, aes(x = x_, y = y_), color="red") +
  geom_point(size = 4) +
  geom_line(width = 2) +
  ylab("Hours") +
  ggtitle("Burndown Chart")
graph
