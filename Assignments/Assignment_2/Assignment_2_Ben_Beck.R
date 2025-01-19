list.files()
getwd()
csv_files<-list.files(path="Data", pattern = ".csv", all.files = TRUE, full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
mypath <- list.files(path="C:/Users/beckb/OneDrive/Desktop/Data_Course_Beck/Data")
length(csv_files)
read.csv(wingspan_vs_mass.csv)
df<-"wingspan_vs_mass.csv"
read.csv(wingspan_vs_mass.csv)
getwd()
df<-"./Data/wingspan_vs_mass.csv"
read.csv(df)
getwd()
?list.files()
list.files(path="Data", pattern = ".csv", all.files = TRUE)
list.files(path="Data", pattern = ".csv", all.files = TRUE, full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
class(csv_files)
length(csv_files)
csv_files[1]
csv_files[c(1,3,5)]
as.numeric(c(x,y,100))
head(csv_files,10)
6L+1
bird<-list.files(recursive = TRUE,pattern = "cleaned_bird_data.csv", full.names = TRUE)
file.copy(bird,".",overwrite = TRUE)
bird
?read.csv
dat<-read.csv(bird)
class(dat)
dim(dat)
dat[c(1,3,5),]
dat[,"Egg_mass"]
dat$Egg_mass
keepers <-dat$Egg_mass > 10
big_egg_mamas<-dat[keepers,]
is.na(dat$Egg_mass)
#subset where egg_mas is greater than 10 and not NA
big_egg_mamas<-dat[dat$Egg_mass>10 & !is.na(dat$Egg_mass),]
# anything where you are trying to invert the true and false you use !=. 
big_egg_mamas
plot(density(big_egg_mamas$Egg_mass))
summary(big_egg_mamas$Egg_mass)
str(dat)
summary(dat$mass)
summary(dat$tarsus)
length(dat$tarsus)
bigmassbuddies <-
dat$mass < median(dat$mass,na.rm = TRUE) &
dat$tarsus < median(dat$tarsus, na.rm = TRUE)
dat[bigmassbuddies,]
plot(dat[bigmassbuddies,"Egg_mass"])
file.remove("./cleaned_bird_data.csv")
head(df,n=6L)
df
df<-read.csv(df)
df
head(df,5)
lower_b<-list.files(path="Data", pattern="^[b]",full.names = TRUE,recursive = TRUE)
?head
head -3 | list.files(path="Data", pattern="^[b]",full.names = TRUE,recursive = TRUE)
list.files(path="Data", pattern="^[b]",full.names = TRUE,recursive = TRUE) | head-3

first_line_b<-for (file in lower_b) {
  if (file.exists(file)) { # Ensure the file exists
    first_line <- readLines(file, n = 1) # Read only the first line
    cat("First line of", file, ":", first_line, "\n")
  }
}

first_line_csv<-for (file in csv_files) {
  if (file.exists(file)) { # Ensure the file exists
    first_line_csvfiles <- readLines(file, n = 1) # Read only the first line
    cat("First line of", file, ":", first_line, "\n")
  }
}
