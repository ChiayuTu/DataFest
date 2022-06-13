# clear the decks
rm(list = ls(all = TRUE))

setwd("C:/Users/tulou/Desktop/PSU_Classes/DataFest")

library(tidyverse)
library(googlesheets4)
library(dplyr)
library(plyr)

#Read table from logs.csv
myData <- read_csv("logs.csv", guess_max = 2106600)
myData <- myData[c(1:2106597), c(1:14)]
first_part <- myData[c(1:526650), c(1:14)]
second_part <- myData[c(526651:1053300), c(1:14)]
thrid_part <- myData[c(1053301:1579950), c(1:14)]
fourth_part <- myData[c(1579951:2106597), c(1:14)]
#view(first_part)
#view(second_part)
#view(thrid_part)
#view(fourth_part)

School_3561 <- myData %>% 
  filter(school == 3561)
School_2570 <- myData[c(473422:493391), c(1:14)]
School_1827 <- myData[c(493392:594445), c(1:14)]
School_647 <- myData[c(594446:1163716), c(1:14)]
School_2238 <- myData[c(1163717:1324416), c(1:14)]
School_5340 <- myData[c(1324417:1502212), c(1:14)]
School_4829 <- myData %>% 
  filter(school == 4829)
School_1531 <- fourth_part %>% 
  filter(school == 1531)
School_9691 <- fourth_part %>% 
  filter(school == 9691)
School_7167 <- fourth_part %>% 
  filter(school == 7167)
School_6266 <- fourth_part %>% 
  filter(school == 6266)
School_3205 <- fourth_part %>% 
  filter(school == 3205)


#view(School_1531)

#Normalize Data 
Finish_Time <- function(x){
  x %>%
  select(player_id, school, session, event_id, event_time, event_time_dbl) %>% 
  filter(event_id == 708) %>% 
  group_by(player_id) %>% 
  top_n(1, event_time_dbl)
}
 
Finish_Time_3561 <- Finish_Time(School_3561)
Finish_Time_2570 <- Finish_Time(School_2570)
Finish_Time_1827 <- Finish_Time(School_1827)
Finish_Time_647 <- Finish_Time(School_647)
Finish_Time_2238 <- Finish_Time(School_2238)
Finish_Time_5340 <- Finish_Time(School_5340)
Finish_Time_4829 <- Finish_Time(School_4829)
Finish_Time_1531 <- Finish_Time(School_1531)
Finish_Time_9691 <- Finish_Time(School_9691)
Finish_Time_7167 <- Finish_Time(School_7167)
Finish_Time_6266 <- Finish_Time(School_6266)
Finish_Time_3205 <- Finish_Time(School_3205)
view(Finish_Time_3205)

#view(Finish_Time_3561)

#Find an average for each school 
average_Time <- function(School){
  S_sum <- sum(School$event_time_dbl)
  S_row <- nrow(School)
  Ave_time <- S_sum / S_row
  return(Ave_time)
}
average_Time(Finish_Time_1531)
average_Time(Finish_Time_1827)

#Plot
#join finish times of all schools to put it into one plot
joinTimes= Finish_Time_3561 %>% full_join(Finish_Time_2570) %>% 
  full_join(Finish_Time_1827) %>% full_join(Finish_Time_647) %>%
  full_join(Finish_Time_2238)%>% full_join(Finish_Time_5340)%>%
  full_join(Finish_Time_4829)%>%full_join(Finish_Time_1531)%>%
  full_join(Finish_Time_9691)%>%full_join(Finish_Time_7167)%>%
  full_join(Finish_Time_6266)%>%full_join(Finish_Time_6266)%>%
  full_join(Finish_Time_3205)

#-----FIRST PLOT
ggplot(joinTimes,aes(x=school, y=event_time_dbl, fill=school)) +
  geom_boxplot() +
  #geom_jitter(width=0.25, alpha=0.5)
  scale_y_log10()+
  #scale_fill_brewer(palette="Spectral")+
  #geom_boxplot(alpha=0.3) +
  theme(legend.position="none")+ 
  labs(x="School", y="Time in seconds")



#Second part
#Find the most repeat values in the column
# Value_Repeat <- ddply(School_1531, .(player_id, event_id, event_description, event_category), nrow)
# most_repeat_value <- Value_Repeat %>% 
#   group_by(player_id) %>% 
#   top_n(4, V1)
# 
# test1 <- most_repeat_value[order(most_repeat_value$player_id, -most_repeat_value$V1),] 
# test2 <- subset(test1, event_id != 211)
# view(test2)
# view(test1)  
# view(most_repeat_value)

Repeat_Value <- function(x){
  value_repeat <- ddply(x, .(player_id, event_id, event_description, event_category), nrow) %>% 
    return(value_repeat)
  
}
myData_repeat <- Repeat_Value(myData)
School_1531_repeat <- Repeat_Value(School_1531)
School_1827_repeat <- Repeat_Value(School_1827)
School_2238_repeat <- Repeat_Value(School_2238)
School_2570_repeat <- Repeat_Value(School_2570)
School_3205_repeat <- Repeat_Value(School_3205)
School_3561_repeat <- Repeat_Value(School_3561)
School_4829_repeat <- Repeat_Value(School_4829)
School_5340_repeat <- Repeat_Value(School_5340)
School_6266_repeat <- Repeat_Value(School_6266)
School_647_repeat <- Repeat_Value(School_647)
School_7167_repeat <- Repeat_Value(School_7167)
School_9691_repeat <- Repeat_Value(School_9691)

Most_repeat <- function(x){
  top_4_repeat <- x %>% 
    group_by(player_id) %>%
    top_n(4, V1)
  order_table <- top_4_repeat[order(top_4_repeat$player_id, -top_4_repeat$V1),]
  remove_211 <- subset(order_table, event_id != 211)
  return(remove_211)
}

myData_most_repeat <- Most_repeat(myData_repeat)
School_1531_most_repeat <- Most_repeat(School_1531_repeat)
School_1827_most_repeat <- Most_repeat(School_1827_repeat)
School_2238_most_repeat <- Most_repeat(School_2238_repeat)
School_2570_most_repeat <- Most_repeat(School_2570_repeat)
School_3205_most_repeat <- Most_repeat(School_3205_repeat)
School_3561_most_repeat <- Most_repeat(School_3561_repeat)
School_4829_most_repeat <- Most_repeat(School_4829_repeat)
School_5340_most_repeat <- Most_repeat(School_5340_repeat)
School_6266_most_repeat <- Most_repeat(School_6266_repeat)
School_647_most_repeat <- Most_repeat(School_647_repeat)
School_7167_most_repeat <- Most_repeat(School_7167_repeat)
School_9691_most_repeat <- Most_repeat(School_9691_repeat)

view(School_1827_most_repeat)

test1 <- School_1827_most_repeat %>% 
  ggplot(aes(x = event_category, fill = event_description)) +
  geom_bar() +
  theme(legend.position = "bottom")
test1

test2 <- School_6266_most_repeat %>% 
  ggplot(aes(x = event_category, fill = event_description)) +
  geom_bar()
test2

test3 <- School_647_most_repeat %>% 
  ggplot(aes(x = event_category, fill = event_description)) +
  geom_bar()
test3


#find mean, standard deviation, ...
#summarize data
summary_time <- myData_most_repeat %>% 
  group_by(event_id) %>% 
  dplyr::summarize(
    mean = mean(V1),
    std_dev = sd(V1), 
    n = n()
  )
view(summary_time)
event_210_sample <- myData_most_repeat %>% 
  filter(event_id == 210) 
view(event_210_sample)

true.mean <- mean(event_210_sample$V1)
true.sd <- sd(event_210_sample$V1)
true.mean
true.sd

pop.distn_210 <-tibble(
  Event210_Time = event_210_sample$V1,
  density = dnorm(Event210_Time, mean = true.mean, sd = true.sd))
#view(pop.distn_210)
ggplot(pop.distn_210) +
  geom_line(aes(Event210_Time, density)) +
  geom_vline(
    xintercept = true.mean,
    color = "red",
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = true.mean + true.sd,
    color = "blue",
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = true.mean - true.sd,
    color = "blue",
    linetype = "dashed"
  ) +
  labs(
    x = "Event 210", y = "Density",
    title = "PDF of standard normal (Event 210)"
  )

event_902_sample <- myData_most_repeat %>% 
  filter(event_id == 902) 
#view(event_207_sample)

true.mean <- mean(event_902_sample$V1)
true.sd <- sd(event_902_sample$V1)
true.mean
true.sd

pop.distn_902 <-tibble(
  Event902_Time = event_902_sample$V1,
  density = dnorm(Event902_Time, mean = true.mean, sd = true.sd))
#view(pop.distn_207)
ggplot(pop.distn_902) +
  geom_line(aes(Event902_Time, density)) +
  geom_vline(
    xintercept = true.mean,
    color = "red",
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = true.mean + true.sd,
    color = "blue",
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = true.mean - true.sd,
    color = "blue",
    linetype = "dashed"
  ) +
  labs(
    x = "Event 902", y = "Density",
    title = "PDF of standard normal (Event 902)"
  )

