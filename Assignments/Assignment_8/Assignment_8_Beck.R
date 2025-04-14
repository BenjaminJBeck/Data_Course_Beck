setwd("C:/Users/beckb/Desktop/Data_Course_Beck/Data")
dat<-read.csv("mushroom_growth.csv")
head(dat)
library(ggplot2)
library(performance)

# Scatterplot: Light vs GrowthRate
ggplot(dat, aes(x = Light, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Light vs Growth Rate", x = "Light", y = "Growth Rate")

# Scatterplot: Nitrogen vs GrowthRate
ggplot(dat, aes(x = Nitrogen, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Nitrogen vs Growth Rate", x = "Nitrogen", y = "Growth Rate")

# Boxplot: Humidity vs GrowthRate
ggplot(dat, aes(x = Humidity, y = GrowthRate)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Humidity vs Growth Rate", x = "Humidity", y = "Growth Rate")

# Scatterplot: Temperature vs GrowthRate
ggplot(dat, aes(x = Temperature, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Temperature vs Growth Rate", x = "Temperature", y = "Growth Rate")

# Model 1: Simple linear regression with Light
model1 <- lm(GrowthRate ~ Light, data = dat)

# Model 2: Additive model with Light and Nitrogen
model2 <- lm(GrowthRate ~ Light + Nitrogen, data = dat)

# Model 3: Include categorical variable Humidity
model3 <- lm(GrowthRate ~ Light + Nitrogen + Humidity, data = dat)

# Model 4: Full model with interaction term
model4 <- lm(GrowthRate ~ Light * Nitrogen + Humidity + Temperature, data = dat)

# Compare models
compare <- compare_performance(model1, model2, model3, model4)
print(compare)
# Plot the comparison
plot(compare)

#Calculate the mse
mse <- function(model) mean(residuals(model)^2)

mse1 <- mse(model1)
mse2 <- mse(model2)
mse3 <- mse(model3)
mse4 <- mse(model4)

mse1; mse2; mse3; mse4

#Select the best model
best_model <- model4

dat$Humidity <- as.factor(dat$Humidity)

# Use the same levels for new data
new_data <- data.frame(
  Light = c(0, 10, 20),
  Nitrogen = c(0, 5, 10),
  Humidity = factor(c("Low", "Low", "Low"), levels = levels(dat$Humidity)),
  Temperature = c(20, 20, 20)
)

# Predict using best model
new_data$PredictedGrowthRate <- predict(best_model, newdata = new_data)

# View predictions
print(new_data)

#Plot predicted data
ggplot(dat, aes(x = Light, y = GrowthRate)) +
  geom_point(color = "blue", alpha = 0.6, size = 2) +
  geom_point(data = new_data, aes(x = Light, y = PredictedGrowthRate), 
             color = "red", size = 3, shape = 17) +
  geom_line(data = new_data, aes(x = Light, y = PredictedGrowthRate), 
            color = "red", linetype = "dashed", size = 1) +
  labs(title = "Actual vs Predicted Growth Rate",
       x = "Light",
       y = "Growth Rate") +
  theme_minimal()

