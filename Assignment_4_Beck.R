Bird<-read.csv("Bird_Measurements1.csv")
library(tidyverse)
ggplot(Bird, aes(x = Bird$Wingspan..in., y = Bird$Weight.Capacity..lb.)) + 
  geom_point() + geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Bird Wingspan vs. Weight Carrying Capacity", 
       x = "Wingspan (In)", 
       y = "Weight Carrying Capacity (lb)") +
  theme_minimal()
