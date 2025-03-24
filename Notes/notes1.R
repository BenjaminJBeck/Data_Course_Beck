vector1 <-c(1,2,3)
vector2 <-c(4,5,6)
vector1*vector2
vector3 <- letters
vector3 + 1
vector1 <- c(2, 4, 6, 8)
vector1 > 3
letters %in% c("a", "e", "i", "o", "u")
getwd()
setwd("C:/Users/beckb/Desktop/Data_Course_Beck/Data")
iris<-read.csv("iris.csv")
nrow(iris)
plot(iris$Species, iris$Sepal.Length)
iris$Species <- as.factor(iris$Species)
plot(iris$Species, iris$Sepal.Length)
Sepal.Area <- c(iris$Sepal.Length * iris$Sepal.Width)
Sepal.Area
iris$Sepal.Area <- Sepal.Area
big_area_iris <- iris$Sepal.Area > 20
big_area_iris <- iris[iris$Sepal.Area > 20, ]
cumsum(iris$Sepal.Length)
rnorm(15, mean = 7.5, sd=1)
class(iris$Sepal.Length)
class(iris$Species)
Species_Numeric <- as.numeric(iris$Species)
Species_Numeric
head(iris$Species, n=10)
sd(iris$Sepal.Length)
table(iris$Sepal.Length)
subset_iris <- iris[iris$Sepal.Length > 6,]
subset_iris
iris_L_W <- iris[iris$Sepal.Length > 3 & iris$Sepal.Width > 3, c("Sepal.Length", "Sepal.Width")]
iris_L_W <- iris[iris$Sepal.Area > 25 & iris$Sepal.Length > 3, c("Sepal.Length", "Sepal.Area")]
iris_L_W
nrow(iris$Species)
nrow(iris)
length(iris$Sepal.Length)
library(ggplot2)
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Area)) + geom_point(color="Blue",shape = 15) + geom_smooth(method = "lm", color="red",fill="salmon")+
  ggtitle("Special Area vs Spatial Length")+
  theme(plot.title = element_text(size = 16, face = "bold"))+
  labs(x="Sepal Length (cm)", y= "Sepal Area (cm^2)") +
  theme(text = element_text(family = PAPYRUS))
install.packages("extrafont")
library(extrafont)
font_import()   # This imports all available system fonts
loadfonts(device = "win")  # For Windows users
