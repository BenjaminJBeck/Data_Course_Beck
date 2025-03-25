library(tidyverse)
library(ggimage)
library(dplyr)
library(gganimate)
library(wesanderson)
library(GGally)
library(skimr)
library(janitor)
library(easystats)
library(MASS)
library(caret)
library(performance)

#Read the data
data <- read.csv("C:/Users/beckb/Desktop/Data_Course_Beck/BIOL3100_Exams/BIOL3100_Exams/Exam_2/unicef-u5mr.csv", stringsAsFactors = FALSE)

colnames(data)

# Clean the Data

tidy_data <- data %>%
  pivot_longer(
    cols = starts_with("U5MR."),
    names_to = "Year",
    names_prefix = "U5MR.",
    values_to = "U5MR"
  ) %>%
  mutate(Year = as.numeric(Year))

head(tidy_data)

#Plot each country's U5MR over time
ggplot(tidy_data, aes(x = Year, y = U5MR, group = CountryName, color = CountryName)) +
  geom_line() +
  labs(title = "Under-5 Mortality Rate Over Time",
       x = "Year",
       y = "U5MR",
       color = "Country") +
  theme_minimal() +
  theme(legend.position = "none")

#Facet each of the countries
ggplot(tidy_data %>% filter(!is.na(U5MR)), 
       aes(x = Year, y = U5MR, group = CountryName, color = CountryName)) +
  geom_line() +  
  labs(title = "Under-5 Mortality Rate Over Time by Continent",
       x = "Year",
       y = "U5MR",
       color = "Country") +
  facet_wrap(~ Continent) +
  theme_minimal() +
  theme(legend.position = "none")

#Save this plot
ggsave("Beck_Plot_1.png", width = 10, height = 6, dpi = 300)

# Calculate mean U5MR per continent per year
continent_avg <- tidy_data %>%
  group_by(Continent, Year) %>%
  summarize(mean_U5MR = mean(U5MR, na.rm = TRUE))

# Plot the mean U5MR for each continent over time
ggplot(continent_avg, aes(x = Year, y = mean_U5MR, color = Continent)) +
  geom_line() +  
  labs(title = "Mean Under-5 Mortality Rate by Continent Over Time",
       x = "Year",
       y = "Mean U5MR",
       color = "Continent") +
  theme_minimal()

#Save this plot
ggsave("Beck_Plot_2.png", width = 10, height = 6, dpi = 300)

# Model 1: Account for only Year
mod1 <- lm(U5MR ~ Year, data = tidy_data)
summary(mod1)

# Model 2: Account for Year and Continent
mod2 <- lm(U5MR ~ Year + Continent, data = tidy_data)
summary(mod2)

# Model 3: Account for Year, Continent, and their interaction term
mod3 <- lm(U5MR ~ Year * Continent, data = tidy_data)
summary(mod3)

#Compare the models and their performance
compare_performance(mod1, mod2, mod3) %>% plot()
#Based on the performance plot, I would assume mod3 is the best fitting model for this data.

# Get predictions from each model
pred_mod1 <- predict(mod1, newdata = tidy_data)
pred_mod2 <- predict(mod2, newdata = tidy_data)
pred_mod3 <- predict(mod3, newdata = tidy_data)


pred_data <- tidy_data %>%
  mutate(
    Pred_mod1 = pred_mod1,
    Pred_mod2 = pred_mod2,
    Pred_mod3 = pred_mod3
  ) %>%
  gather(key = "Model", value = "Predicted_U5MR", Pred_mod1, Pred_mod2, Pred_mod3)

#Create a plot of the data
ggplot(pred_data, aes(x = Year, y = Predicted_U5MR, color = Continent, group = Continent)) +
  geom_line(size = 1) +
  facet_wrap(~ Model, scales = "free_y") +
  labs(
    title = "Predictions from Models Faceted by Model",
    x = "Year",
    y = "Predicted U5MR",
    color = "Continent"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
