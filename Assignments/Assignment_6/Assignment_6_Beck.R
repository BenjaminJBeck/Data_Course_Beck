library(tidyverse)
library(ggplot2)
library(gganimate)

# Import the data
setwd("C:/Users/beckb/Desktop/Data_Course_Beck/Data")
getwd()

# Assign the data to a name
dat <- read_csv("BioLog_Plate_Data.csv")
view(dat)
head(dat)

# Clean the titles
dat <- dat %>%
  rename(Sample_ID = `Sample ID`)

# Change the values of Hr_ to "Hour" and the values to absorbance
tidy_data <- dat %>%
  pivot_longer(cols = starts_with("Hr_"),
               names_to = "Hour",
               values_to = "Absorbance") %>%
  mutate(
    Hour = as.numeric(str_remove(Hour, "Hr_")),
    Sample_Type = case_when(
      str_detect(Sample_ID, regex("soil", ignore_case = TRUE)) ~ "Soil",
      str_detect(Sample_ID, regex("water|creek|pond|lake|river", ignore_case = TRUE)) ~ "Water",
      TRUE ~ "Unknown"
    )
  )

head(tidy_data)

# Summarize the data by taking the mean absorbance for each combination of Sample_ID and Hour
tidy_data_avg <- tidy_data %>%
  group_by(Sample_ID, Hour) %>%
  summarise(mean_absorbance = mean(Absorbance, na.rm = TRUE), .groups = "drop")

# Create a plot with the summarized data
ggplot(tidy_data_avg, aes(x = Hour, y = mean_absorbance, color = Sample_ID)) +
  geom_line() +
  labs(title = "Absorbance Over Time (Averaged)", x = "Time (Hour)", y = "Mean Absorbance")

# Filter data for dilution 0.1 only
plot_data <- tidy_data %>%
  filter(Dilution == 0.1)

# Create a plot with the x-axis as time, y-axis as absorbance, facet each of the substrates
ggplot(plot_data, aes(x = Hour, y = Absorbance, color = Sample_Type)) +
  geom_smooth(se = FALSE, size = 1) +
  facet_wrap(~ Substrate, scales = "free_y") +
  labs(
    title = "Just dilution 0.1",
    x = "Time",
    y = "Absorbance",
    color = "Type"
  ) +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(size = 8))

# Create an animated plot with the mean absorbance on the y-axis and time on the x-axis
# Filter the plot data for selected sample IDs
plot_data_avg <- tidy_data_avg %>%
  filter(Sample_ID %in% c("Clear_Creek", "Soil_1", "Soil_2", "Waste_Water"))

# Create an animated plot
p <- ggplot(plot_data_avg, aes(x = Hour, y = mean_absorbance, color = Sample_ID, group = Sample_ID)) +
  geom_line(size = 1) +
  labs(
    title = 'Absorbance Over Time (Animated)',
    subtitle = 'Time: {frame_time}',
    x = 'Time (Hour)',
    y = 'Mean Absorbance',
    color = 'Sample'
  ) +
  theme_minimal(base_size = 12) +
  transition_reveal(Hour) # animate the plot based on time

# Optional: loop/cycle with transition for a smoother effect
p_loop <- p +
  transition_reveal(Hour) +
  enter_fade() + exit_fade() +
  view_follow(fixed_y = TRUE)

# Print the animation plot
print(p)
