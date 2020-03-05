library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)

start_date <- make_datetime(year = 2020, month = 2, day = 7)
end_date <- make_datetime(year = 2020, month = 3, day = 5)

minutes <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSCzOjDcd53IfLYBk1v7OUZdhUcfNfGUOHjbpwDPuSQBtKAQEoViKmCmz71UQ6_JFRjnuDqIdoDOZv5/pub?output=csv") %>%
  mutate(Month = as.numeric(str_extract(Date, "^\\d+"))) %>%
  mutate(Day = as.numeric(str_extract(Date, "\\d+$"))) %>%
  mutate(Date = make_datetime(year = 2020, month = Month, day = Day)) %>%
  group_by(Date) %>%
  summarize(N = n(), Total_Hours = sum(Man.Hours)) %>%
  arrange(Date) %>%
  add_row(Date = start_date, N = 0, Total_Hours = 0) %>%
  arrange(Date) %>%
  mutate(Total_Hours = if_else(is.na(Total_Hours), 0, Total_Hours))

goal_line = data.frame(x_ = c(start_date, end_date), y_ = c(sum(minutes$Total_Hours), 0))

graph <- ggplot(data = minutes, aes(x = Date, y = sum(Total_Hours) - cumsum(Total_Hours))) +
  geom_line(data = goal_line, aes(x = x_, y = y_), color="red") +
  geom_point(size = 4) +
  geom_line(width = 2) +
  ylab("Hours") +
  ggtitle("Burndown Chart")
graph
