library(ggplot2)
library(dplyr)
library(plyr)
library(scales)
library(xlsx)
library(rJava)
library(xlsxjars)
library(reshape2)
library(lubridate)
library(ggthemes)
library(gridExtra)
# http://www.oecd.org/pisa/aboutpisa/
con <- url("http://beta.icm.edu.pl/PISAcontest/data/student2012.rda")
load(con)

student2012 <- tbl_df(student2012)
# Scores variables have attributes which dplyr cannot use group_by with
## These can be cast as numeric to remove attributes
## mutate_each can apply a function(as.numeric) to each column matching a certain criteria

country_data <- student2012 %>% 
  mutate(Math = as.numeric((PV1MATH + PV2MATH + PV3MATH + PV4MATH + PV5MATH)/5),
         Reading = as.numeric((PV1READ + PV2READ + PV3READ + PV4READ + PV5READ)/5),
         Science = as.numeric((PV1SCIE + PV1SCIE + PV1SCIE + PV1SCIE + PV1SCIE)/5)) %>%
  select(Gender = ST04Q01,
         birth_year = ST03Q02,
         birth_month = ST03Q01,
         PC_at_home = ST26Q04,
         Country = CNT,
         Math, Reading, Science)

country_scores <- country_data %>%
  group_by(Country, Gender) %>%
  summarise(mean_math = mean(Math),
            mean_reading = mean(Reading),
            mean_science = mean(Science),
            count = n() ) %>%
  arrange(mean_math, mean_reading, mean_science)

ggplot(country_scores, aes(x = mean_reading, y = mean_math, color = Gender, size=count)) +
  geom_point() +
  geom_smooth() +
  geom_point(data = subset(country_scores, Country %in% c("United States of America", "Mexico", "Peru", "Qatar", "China-Shanghai", "Singapore", "Korea", "Finland", "Chile")), 
             aes(label = Country),
             color="#0266c8",
             show_guide = FALSE) +
  geom_text(data = subset(country_scores, Country %in% c("United States of America", "Mexico", "Peru", "Qatar", "China-Shanghai", "Singapore", "Korea", "Finland", "Chile")),
            aes(label = Country),
            color = "black",
            size = 3,
            vjust = -0.5,
            show_guide = FALSE) +
  #geom_abline(intercept = 0, slope = 1, linetype=2, color="orange") +
  ylab("Average Math Score") + 
  xlab("Average Reading Score") + 
  ggtitle("Average Reading and Math Scores per country, by gender")

ggplot(country_scores, aes(x = mean_reading, y = mean_science, color = Gender, size=count)) +
  geom_point() +
  geom_smooth() +
  geom_point(data = subset(country_scores, Country %in% c("United States of America", "Mexico", "Peru", "Qatar", "China-Shanghai", "Singapore", "Korea", "Finland", "Chile")), 
             aes(label = Country),
             color="#0266c8",
             show_guide = FALSE) +
  geom_text(data = subset(country_scores, Country %in% c("United States of America", "Mexico", "Peru", "Qatar", "China-Shanghai", "Singapore", "Korea", "Finland", "Chile")),
            aes(label = Country),
            color = "black",
            size = 3,
            vjust = -0.5,
            show_guide = FALSE) +
  #geom_abline(intercept = 0, slope = 1, linetype=2, color="orange") +
  ylab("Average Science Score") + 
  xlab("Average Reading Score") + 
  ggtitle("Average Reading and Science Scores per country, by gender")

# The trend is similar for science vs reading.

country_scores_PC <- country_data %>%
  group_by(Country, PC_at_home) %>%
  summarise(mean_math = mean(Math),
            mean_reading = mean(Reading),
            mean_science = mean(Science),
            count = n() ) %>%
  arrange(mean_math, mean_reading, mean_science)

g1 <- ggplot(subset(country_scores_PC, !(is.na(PC_at_home))), aes(x = mean_reading, fill = PC_at_home, size=count)) +
  geom_density(alpha=0.66) +
  ylab("Density") + 
  xlab("Average Reading Score") + 
  ggtitle("Density Plot of Average Reading Scores with/without a PC at home")

g2 <- ggplot(subset(country_scores_PC, !(is.na(PC_at_home))), aes(x = mean_math, fill = PC_at_home, size=count)) +
  geom_density(alpha=0.66) +
  ylab("Density") + 
  xlab("Average Math Score") + 
  ggtitle("Density Plot of Average Math Scores with/without a PC at home")

g3 <- ggplot(subset(country_scores_PC, !(is.na(PC_at_home))), aes(x = mean_science, fill = PC_at_home, size=count)) +
  geom_density(alpha=0.66) +
  ylab("Density") + 
  xlab("Average Science Score") + 
  ggtitle("Density Plot of Average Science Scores with/without a PC at home")

grid.arrange(g1, g2, g3, ncol=1)
## Across all countries, those having a PC at home  had a with higher distribution of scores in reading, math, and science.