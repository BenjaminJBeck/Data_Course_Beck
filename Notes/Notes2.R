library(palmerpenguins)
library(tidyverse)
library(ggimage)
library(gganimate)
library(wesanderson)
library(GGally)
library(skimr)
library(janitor)


p<-
penguins %>%
  filter(!is.na(sex), !is.na(body_mass_g)) %>% 
  mutate(sex=sex %>% str_to_sentence()) %>% 
ggplot(aes(x=bill_length_mm, y=body_mass_g, color=species))+
  labs(title="Body mass vs Bill Length for Each Sex", y="Body Mass (g)", x="Bill Length (mm)")+
  geom_point()+
  stat_ellipse()+
  theme_bw()+
  facet_wrap(~sex)

q<-
penguins %>% 
  ggplot(aes(x=flipper_length_mm, fill = species)) +
  geom_histogram(binwidth = 2, position = "identity", alpha = .5) +
  labs(title = "Penguins Flipper Length",
       x="Flipper Length (mm)",
       y="Frequency") +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) +
  theme_minimal()

r<-
penguins %>% 
  ggplot(aes(x=species, y=flipper_length_mm))+
  geom_boxplot(aes(color=species),width = 0.3, show.legend = FALSE)+
  geom_jitter(aes(color=species), alpha = .5, show.legend = FALSE, position = position_jitter(width = .2, seed = 0))+
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(title = "Flipper Length over Species",
       x="Species",
       y="Flipper Length (mm)")

s<-
penguins %>% 
  ggplot(aes(x=body_mass_g, fill = species))+
  geom_histogram(binwidth = 150, position = "identity", alpha = .5)+
  labs(title = "Body Mass Over Species",
       x= "Body Mass (g)",
       y= "Frequency")+
  scale_fill_manual(values = c("darkorange", "purple", "cyan4"))+
  theme_minimal()

t<-
penguins %>% 
  ggplot(aes(x=flipper_length_mm, y=body_mass_g))+
  geom_point(aes(color=sex))+
  labs(title = "Flipper Length vs Body Mass",
       x="Flipper Length (mm)",
       y="Body Mass (g)")+
  scale_color_manual(values = c("darkorange","cyan4"), na.translate = FALSE) +
  theme(legend.position = "bottom",
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot") +
  facet_wrap(~species)

v<-
penguins %>% 
  ggplot(aes(x=bill_length_mm, y=bill_depth_mm))+
  geom_point()+
  geom_smooth(method = 'lm', se = FALSE, color = 'darkgrey')+
  labs(title = "Bill Length vs Bill Depth",
       x="Bill Length (mm)",
       y="Bill Depth (mm)")

#Class 2/13/2025
p1<-
iris %>%
  ggplot(aes(x=Sepal.Length, y=Sepal.Width))+
  geom_point()

ggsave(p1, filename="./figures/basic_plot.png",
       width=6,
       height=6,
       dpi=300)

#CLEAN DATA
#Every row is a single observation
#Every collum is a single variable
#That's it

iris %>% view()
table1
table2
table3
table4a
table4b
table5

table3 %>% 
  separate(rate, into = c("Cases","Population"),convert=TRUE)

table5 %>% 
  separate(rate, into=c("cases","population"),convert=TRUE) %>% 
  mutate(year=paste0(century,year) %>% as.numeric) %>% 
  select(-century)

table2 %>% 
  pivot_wider(names_from=type, values_from=count)


table4a %>% 
  pivot_longer(cols=c("1999","2000"),
               names_to = "year",
               values_to = "cases", names_transform = as.numeric)
  

table4b %>% 
  pivot_longer(cols=c("1999","2000"),
               names_to = "year",
               values_to = "population", names_transform = as.numeric)

#2/20/25
dat<-read_csv("./Data/Bird_Measurements.csv") %>% clean_names()
view(dat)
skim(dat)

dat2<-
  dat %>% 
  clean_names()
names(dat)
names(dat2)


#What's wrong
# - some collums have multiple variables in them
# - we need a collum for sex
# - get rid of the -N columns
# - remove species_number
# - Split into f,m,u dfs
# pivot longer
# merge back together

male <-
  dat %>% 
  select(species_name,m_mass,m_wing,m_tarsus,m_bill,clutch_size,egg_mass) %>% 
  mutate(sex="male")
names(male) <- names(male) %>% str_remove("^m_")


female<-
  dat %>% 
  select(species_name,f_mass,f_wing,f_tarsus,f_bill,clutch_size,egg_mass) %>% 
  mutate(sex="female")
names(female) <- names(female) %>% str_remove("^f_")

unsexed<-
  dat %>% 
  select(species_name,unsexed_mass,unsexed_wing,unsexed_tarsus,unsexed_bill,clutch_size,egg_mass) %>% 
  mutate(sex="unsexed")
names(unsexed) <- names(unsexed) %>% str_remove("^unsexed_")

dat<-
  male %>% 
  full_join(female) %>% 
  full_join(unsexed)

dat %>% 
  ggplot(aes(x=tarsus, y=mass, color=sex))+
  geom_point()+
  geom_smooth()+
  labs(title="Mass vs Tarsus")

#2/25/25 Notes
#What we need to do:
#We have to make a collum for "religion" and make a collum for "proportion".
#We have to make a bar chart for x=religion, y=proportion.
#We have to create facets for each state
dat<-read.csv("./Data/Utah_Religions_by_County.csv")

dat<-dat %>% 
  clean_names()

view(dat)
dat_long<-
  dat %>%
  pivot_longer(cols = -c(county,pop_2010,non_religious),
               names_to = "religion",
               values_to = "proportion") %>% 
  group_by(religion) %>% 
  summarise(sum=sum(proportion)) %>% 
  arrange(desc(sum))
  
view(dat)
view(dat_long)
dat_long$religion
dat<-dat %>%
  clean_names() %>% 
  pivot_longer(-c(county,pop_2010,religious),
               names_to = "religion",
               values_to = "proportion") %>% 
  mutate(religion=factor(religion,levels=dat_long$religion))

view(dat)
dat %>% 
  ggplot(aes(x=religion, y=proportion))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  facet_wrap(~county)


