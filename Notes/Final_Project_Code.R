#Final Project Code
#How do temperature, time, and location effect covid-19 (and related diseases) deaths across the US?
#Sources:
#https://catalog.data.gov/dataset/provisional-covid-19-death-counts-by-sex-age-and-state
#https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/statewide/time-series/48/tavg/1/0/2020-2023

#Load dependencies
setwd("C:/Users/beckb/Desktop/Data_Course_Beck/Data")
dat_origional<-read.csv("Covid_19_Death_by_Sex_Age.csv")
library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(purrr)
library(lubridate)
library(mgcv)
library(tidyr)
library(forecast)
library(randomForest)
library(caret)
#Check the column names, tidy
dat <- janitor::clean_names(dat_origional)
#Remove rows with "United States" under column "states"
dat <- dat %>% filter(dat$state != "United States")
#Only have 2 sexes
dat <- dat %>% filter(sex != "All Sexes")
#Filter "By Month"
dat <- dat %>% filter(group == "By Month")
#Remove Unnecessary Columns
dat <- dat %>% 
  select(-data_as_of, -start_date, -end_date, -group, -footnote, -pneumonia_influenza_or_covid_19_deaths)
#Remove conflicting ages
dat <- dat %>% 
  filter(age_group != "All Ages", 
         age_group != "0-17 years", 
         age_group != "18-29 years")

#Remove any NAs from the data
dat <- na.omit(dat)

#Change column names for simplicity
names(dat) <- c("year", "month", "state", "sex", "age_group", 
                "covid_deaths", "total_deaths", "pneumonia_deaths", 
                "pneumonia_covid_deaths", "flu_deaths")

#Change months from numerical to character/group
dat <- dat %>%
  mutate(month = factor(month, levels = 1:12, 
                        labels = c("January", "February", "March", "April", 
                                   "May", "June", "July", "August", 
                                   "September", "October", "November", "December")))

#Now load in other data
all_states_data <- list.files(path = "C:/Users/beckb/Desktop/Data_Course_Beck/Data/Final_Data", pattern = "\\.csv$", full.names = TRUE)

# Read and combine all files
all_states_data <- all_states_data %>%
  map_df(~ {
    state_name <- gsub("_Temperature.*", "", basename(.x))
    read_csv(.x, skip = 4, col_names = c("Date", "Temperature")) %>%
      mutate(
        Date = as.Date(paste0(Date, "01"), format = "%Y%m%d"),
        Temperature = as.numeric(Temperature),
        State = state_name
      )
  })

#Remove states that don't match:
dat <- dat %>%
  filter(!state %in% c("New York City", "District of Columbia", "Puerto Rico"))
#Make the format the same
all_states_data <- all_states_data %>%
  mutate(State = gsub("_", " ", State))
all_states_data <- janitor::clean_names(all_states_data)
#Change the date format to year, month
all_states_data <- all_states_data %>%
  mutate(
    month = month(date),
    year = year(date)
  )
#Remove date
all_states_data <- all_states_data %>% 
  select(-date)
#Change the months from numerical to words:
all_states_data <- all_states_data %>%
  mutate(month = factor(month, levels = 1:12, 
                        labels = c("January", "February", "March", "April", 
                                   "May", "June", "July", "August", 
                                   "September", "October", "November", "December")))
#Combine data
dat <- left_join(dat, all_states_data, by = c("state", "year", "month"))
dat <- dat %>% filter(!is.na(temperature))

#Create a time-based df
dat <- dat %>%
  mutate(
    date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%B-%d")
  ) %>%
  arrange(date)
#Remove flu
dat <- dat %>% select(-flu_deaths)
# Ensure months are in order
dat$month <- factor(dat$month, levels = month.name)

###################
#Analyzing the Data
###################



#Plot of Monthly Deaths by Month and Year
monthly_deaths <- dat %>%
  group_by(year, month) %>%
  summarise(
    covid = sum(covid_deaths, na.rm = TRUE),
    pneumonia = sum(pneumonia_deaths, na.rm = TRUE),
    pneumonia_covid = sum(pneumonia_covid_deaths, na.rm = TRUE)
  ) %>%
  ungroup()
# Convert to long format for ggplot
deaths_long <- monthly_deaths %>%
  pivot_longer(cols = c(covid, pneumonia, pneumonia_covid),
               names_to = "cause", values_to = "deaths")
# Plot
ggplot(deaths_long, aes(x = factor(month, levels = month.name), y = deaths, color = cause, group = cause)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ year) +
  labs(title = "Monthly Deaths by Cause and Year",
       x = "Month",
       y = "Total Deaths",
       color = "Cause of Death") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Statistical Tests
monthly_deaths_summary <- dat %>%
  group_by(year, month) %>%
  summarise(total_deaths = sum(covid_deaths, na.rm = TRUE)) %>%
  ungroup()

# Perform ANOVA to compare the means of total deaths by month
anova_result <- aov(total_deaths ~ month, data = monthly_deaths_summary)
summary(anova_result)

print("The ANOVA analysis conducted on the total deaths across different months 
indicates that there is no statistically significant difference in the mean 
death counts between months. The F-statistic of 0.837, along with a p-value 
of 0.606, suggests that the variation observed in death counts between months 
is likely due to random chance rather than a systematic effect of the month 
itself. Since the p-value is greater than the typical significance threshold 
of 0.05, we fail to reject the null hypothesis, which posits that the mean 
deaths across the months are equal. In practical terms, this means that, based 
on the available data, there is no strong evidence to suggest that the month of 
the year has a significant impact on the number of deaths. The observed 
differences in death counts from one month to another could be attributed 
to random variation rather than seasonal or other temporal effects. 
Further investigations, possibly incorporating other variables such as 
regional factors, underlying health conditions, or external events, may be 
needed to better understand the patterns in mortality over time.")
#Create a plot?
ggplot(dat, aes(x = temperature, y = total_deaths)) +
  geom_point() +                     # Add points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add linear regression line
  labs(x = "Temperature (°C)", y = "Total Deaths", title = "Temperature vs Total Deaths") +
  theme_minimal()

#Does tempearture have an influence on total deaths?
correlation <- cor(dat$temperature, dat$total_deaths, method = "pearson")
print(correlation)

# Perform correlation test to get p-value
cor_test_result <- cor.test(dat$temperature, dat$total_deaths)
print(cor_test_result$p.value)

print("The analysis of the relationship between temperature and total deaths 
indicates a very weak positive correlation, with a Pearson correlation 
coefficient of 0.1003. This suggests that while there is a slight tendency 
for total deaths to increase as temperature rises, the relationship is not 
strong. However, the correlation is statistically significant, as 
evidenced by the extremely small p-value of 2.048246e-36, which 
indicates that the correlation is unlikely to be due to random 
chance. Despite the statistical significance, the weak strength of the 
correlation suggests that temperature is not a strong predictor of total 
deaths.")

#Is there any correlation between Temperature by each month and Covid Deaths?
# Create a scatter plot of covid_deaths vs temperature, faceted by month, with a regression line and error
ggplot(dat, aes(x = temperature, y = covid_deaths)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter points
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Regression line with error (confidence interval)
  labs(title = "Covid Deaths vs Temperature by Month", 
       x = "Temperature (°C)", 
       y = "Covid Deaths", 
       subtitle = "Linear regression with confidence interval",
       caption = "Data source: Your dataset") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 10, face = "italic"),
        strip.text = element_text(size = 10)) +  # Adjust facet label size
  facet_wrap(~ month, scales = "free")  # Facet by month with free scales for each plot

lm_model <- lm(covid_deaths ~ temperature, data = dat)
summary(lm_model)

anova_result <- aov(covid_deaths ~ month + temperature, data = dat)
summary(anova_result)

print("The results from the linear regression and ANOVA tests suggest that the 
      month of the year significantly affects COVID-19 deaths, with 
      month-to-month variations being highly statistically significant 
      (p-value < 2e-16 in both tests). The regression model, however, shows 
      that temperature has a small negative effect on COVID-19 deaths, but this 
      effect is not statistically significant (p-value = 0.696), and the model 
      overall explains very little of the variability in COVID-19 deaths 
      (R-squared = 9.716e-06). On the other hand, the ANOVA analysis indicates 
      that temperature also significantly influences COVID-19 deaths 
      (p-value < 2e-16), suggesting that while temperature does show a 
      statistically significant relationship with deaths, it may not be a 
      strong individual predictor when considered alongside other factors.")

#Does state have an influence on disease deaths? Are some diseases greater then others?

# Convert data to long format for plotting
long_dat <- dat %>%
  pivot_longer(cols = c(covid_deaths, pneumonia_deaths, pneumonia_covid_deaths),
               names_to = "cause", values_to = "deaths")

# Group by year, state, and cause
state_year_deaths <- long_dat %>%
  group_by(year, state, cause) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE), .groups = "drop")

# Plot
ggplot(state_year_deaths, aes(x = state, y = total_deaths, fill = cause)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ year, scales = "free_y") +
  labs(title = "Disease Deaths by State and Year",
       x = "State", y = "Number of Deaths", fill = "Cause") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Anova test
anova_state <- aov(covid_deaths ~ state, data = dat)
summary(anova_state)

dat %>%
  group_by(state) %>%
  summarise(total_covid_deaths = sum(covid_deaths, na.rm = TRUE)) %>%
  arrange(desc(total_covid_deaths))

print("The ANOVA test results indicate that there is a statistically significant 
      relationship between the state and COVID-19 deaths 
      (F(47, 15655) = 53.52, p < 2e-16). This means that the average number of 
      COVID-19 deaths varies significantly across different states. The very 
      small p-value suggests that these differences are unlikely to have 
      occurred by chance, supporting the conclusion that state-level factors 
      likely play an important role in COVID-19 death rates.")

head(dat)

# Split data into training and test sets (80% training, 20% testing)
set.seed(123)  # Set seed for reproducibility
trainIndex <- createDataPartition(dat$covid_deaths, p = 0.8, list = FALSE)
train_data <- dat[trainIndex, ]
test_data <- dat[-trainIndex, ]

# Fit a Random Forest model
rf_model <- randomForest(covid_deaths ~ temperature + state + sex + age_group, 
                         data = train_data, 
                         ntree = 100)

# Make predictions
predictions <- predict(rf_model, test_data)

# Evaluate model performance (e.g., MSE)
mse <- mean((predictions - test_data$covid_deaths)^2)
print(paste("Mean Squared Error (MSE):", mse))

# Optionally, visualize feature importance
importance(rf_model)
varImpPlot(rf_model)

print("The predictive model for COVID-19 deaths showed a Mean Squared Error 
      (MSE) of 5631.59, indicating the model's performance in predicting actual 
      deaths based on available features. Feature importance analysis revealed 
      that Age Group is the most significant predictor of COVID-19 deaths, 
      followed by Temperature, State, and Sex. These findings suggest that 
      age-related factors play a critical role in COVID-19 mortality, while 
      environmental conditions (temperature) and regional factors (state) also 
      contribute to the model's predictions. Further improvements to the model 
      could focus on refining these features or exploring additional variables 
      for better accuracy.")


#How does age group effect the deaths?
# Create the histogram with age group on the x-axis, total deaths on the y-axis, faceted by sex
ggplot(dat, aes(x = age_group, y = total_deaths)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ sex) +
  labs(title = "Distribution of Total Deaths by Age Group and Sex",
       x = "Age Group",
       y = "Total Deaths") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Perform ANOVA to test if sex and age group influence total deaths
anova_result <- aov(total_deaths ~ sex * age_group, data = dat)
#Another Plot
ggplot(dat, aes(x = age_group, y = total_deaths, fill = sex)) +
  geom_boxplot() +
  labs(title = "Deaths by Age Group and Sex", x = "Age Group", y = "Total Deaths") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Show the summary of the ANOVA result
summary(anova_result)
print("The results of the ANOVA indicate significant effects for sex, 
      age group, and their interaction on the dependent variable. The sex 
      variable showed a significant effect (F = 18.52, p < 0.001), as did age 
      group (F = 877.07, p < 2e-16), with both factors contributing notably to 
      the variation in the data. Additionally, the interaction between sex and 
      age group was highly significant (F = 40.58, p < 2e-16), suggesting that 
      the relationship between the dependent variable and age group differs 
      depending on sex. The residuals indicate that there is still some unexplained 
      variation in the model, but the significant p-values for the factors and 
      their interaction suggest a strong model fit.")

ggplot(dat, aes(x = age_group, y = total_deaths, fill = sex)) +
  geom_boxplot() +
  labs(title = "Deaths by Age Group and Sex", x = "Age Group", y = "Total Deaths") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print("Based on the analysis of COVID-19 and related deaths across different 
      states, age groups, sexes, and years, we observed several key patterns. 
      Statistical tests revealed significant influences of state, sex, and age 
      group on the total number of deaths, with age group showing the most 
      pronounced effect on death rates. For example, older age groups, 
      particularly those aged 85 years and over, were associated with much 
      higher death counts compared to younger groups. Additionally, temperature 
      and state showed considerable effects, with certain states, such as 
      Alabama and Florida, consistently exhibiting higher death counts. Sex 
      also played a role, but its influence was less pronounced compared to 
      age and state. Predictive modeling using random forests provided a solid 
      approach for forecasting total deaths, with temperature and state being 
      the most influential features. Furthermore, a variety of visualizations, 
      including histograms, boxplots, and heatmaps, helped illustrate these 
      relationships and trends over time. Overall, this comprehensive analysis 
      highlights the complex interplay of demographic, environmental, and 
      geographic factors in influencing COVID-19-related mortality, offering 
      valuable insights for public health strategies.")
