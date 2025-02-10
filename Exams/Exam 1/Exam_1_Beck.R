#Exam 1
#Ben Beck
#2/8/25


library(tidyverse)
library(ggplot2)
library(dplyr)


#1
df <- read_csv("cleaned_covid_data.csv")
view(df)

#2
A_States <- subset(df, grepl("^A", Province_State))


#3 
plot(x = A_States$Last_Update, y = A_States$Deaths) #Simple version

ggplot(data = A_States, aes(Last_Update, y = Deaths)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Deaths Over Time by State", x = "Time", y = "Deaths") +
  theme_minimal()

#4 

state_max_fatality_rate <- A_States %>%
  group_by(Province_State) %>%          
  summarise(Max_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE))  # Find the max Case_Fatality_Ratio for each state

print(state_max_fatality_rate)

#5

ggplot(state_max_fatality_rate, aes(x = Province_State, y = Max_Fatality_Ratio)) +
  geom_bar(stat = "identity", color = "darkblue", fill = "darkblue") +
  labs(title = "Maximum Fatality Ratio by State", 
    x = "State", 
    y = "Maximum Fatality Ratio") +
  theme(axis.text.x = element_text(angle = 0, hjust = .5))

#6
# For this one I had to do some research and get some help

total_cumulative_deaths <- df %>%
  group_by(Last_Update) %>% 
  summarise(Daily_Deaths = sum(Deaths, na.rm = TRUE)) %>% 
  mutate(Cumulative_Deaths = cumsum(Daily_Deaths))


ggplot(total_cumulative_deaths, aes(x = Last_Update, y = Cumulative_Deaths)) +
  geom_line() +
  labs(
    title = "Cumulative Deaths Over Time in the US",
    x = "Date",
    y = "Cumulative Deaths"
  ) +
  theme_minimal()
