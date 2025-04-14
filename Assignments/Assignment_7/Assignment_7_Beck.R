#Assignment 7


#Loading the data
library(tidyverse)
library(ggplot2)
library(gganimate)
library(janitor)
library(dplyr)

#Importing the data
getwd()
setwd("C:/Users/beckb/Desktop/Data_Course_Beck/Assignments/Assignment_7")
dat<-read.csv("Utah_Religions_by_County.csv")
view(dat)
head(dat)

#Reshape the data to make it more tidy
dat_tidy <- dat %>%
  pivot_longer(
    cols = -(County:Non.Religious),  # keeps County, Pop_2010, Religious, Non.Religious
    names_to = "Religion",
    values_to = "Proportion"
  )

#Finish Cleaning the data (column names)
dat_tidy <- dat_tidy %>%
  clean_names()
view(dat_tidy)
head(dat_tidy)

# Proportion of LDS vs. Proportion of Non-religious
ggplot(dat_tidy %>% filter(religion == "LDS"), aes(x = proportion, y = non_religious)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Proportion of LDS vs. Proportion of Non-religious",
       x = "Proportion of LDS in County",
       y = "Proportion of Non-religious in County") +
  theme_minimal()

# Calculate correlation: Proportion of LDS vs. Proportion of Non-religious
cor_LDS_nonreligious <- cor(dat_tidy %>% filter(religion == "LDS") %>% select(proportion, non_religious), use = "complete.obs")
cor_LDS_nonreligious


# Population vs. Proportion of each religion (Facet by Religion)
ggplot(dat_tidy, aes(x = pop_2010, y = proportion)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  scale_x_log10() +  # Log scale for population
  facet_wrap(~ religion) +  # Facet by religion
  labs(title = "Population vs. Proportion of Religious Groups",
       x = "Log of Population of County",
       y = "Proportion of Religion in County") +
  theme_minimal()


# Compute correlation for each religion
cor_results <- dat_tidy %>%
  group_by(religion) %>%
  summarize(correlation = cor(pop_2010, proportion, use = "complete.obs"))

# Print correlation results for each religion
cor_results
