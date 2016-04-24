library(ggplot2)
library(dplyr)
library(scales)
library(rJava)
library(xlsxjars)
library(xlsx)
library(reshape2)
library(lubridate)
library(ggthemes)
library(gridExtra)

hours <- tbl_df(read.xlsx('/Users/Vineets/Downloads/EDA_Course_Materials/Problem Set #1/indicator_hours per week.xlsx',
                  sheetName = "Data", header = TRUE))
summary(hours)
names(hours)

hours <- hours %>%
  select(-NA.) %>% # Removing NA columns
  rename(Country=Working.hours.per.week) %>%
  filter(Country != "<NA>") # Removing NA rows

hours.long <- melt(hours, id=c("Country"), value.name="Hours", 
                   variable.name="Year")
hours.long <- tbl_df(hours.long)
summary(hours.long)

hours.long <- hours.long %>%
  mutate(Year = as.character(Year), # Converting to character
         Year = substr(Year, 2, 5), # Removing 'X' from data and subsetting the remaining text
         Year = as.numeric(Year))  # Converting to numeric

yearStats <- hours.long %>%
  group_by(Year) %>%
  summarize(median = median(Hours, na.rm =TRUE),
            mean = mean(Hours, na.rm=TRUE),
            lower = min(Hours, na.rm=TRUE),
            upper = max(Hours, na.rm=TRUE),
            se = sd(Hours, na.rm=TRUE)/sqrt(length(Hours)),
            avg_upper = mean + (2.101*se),
            avg_lower = mean - (2.101*se),
            quant.25 = quantile(Hours, na.rm=TRUE, 0.25),
            quant.75 = quantile(Hours, na.rm=TRUE, 0.75))

yearStats <- round(yearStats, 2)

p <- ggplot(yearStats, aes(x=Year, y = median)) +
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  geom_linerange(yearStats, mapping=aes(x=Year, ymin=lower, ymax=upper), colour = "wheat2", alpha=1, size=5) + 
  geom_linerange(yearStats, mapping=aes(x=Year, ymin=quant.25, ymax=quant.75), colour = "wheat4", size=5) +
  geom_line(yearStats, mapping=aes(x=Year, y=median, group=1)) +
  geom_vline(xintercept = 1980, colour = "wheat4", linetype=1, size=1) + 
  geom_hline(yintercept=seq(26, 56, 2), color="white", linetype=1)

dottedYears <- seq(1980, 2007, 5)
p <- p + geom_vline(xintercept = dottedYears, color="wheat4", linetype=3, size=0.5)
p <- p + coord_cartesian(ylim = c(26,58))+
  scale_y_continuous(breaks=seq(26, 60, 2)) +
  scale_x_continuous(breaks=seq(1980, 2005, 5), expand=c(0,0) )

p <- p + geom_line(data = subset(hours.long, Country == "United States"), aes(x = Year, y = Hours, group = Country), color ="brown") +
  annotate("segment", x=2000, xend=2002, y=35.5, yend=36, color="brown") +
  annotate("text", x=2003, y=36.3, label="U.S. Hours", size=3.5, color="brown") + 
  annotate("segment", x=2000, xend=2001, y=33.5, yend=32) + 
  annotate("text", x=2002, y=31.7, label="World Medians", size=3.5)


p1 <- p + annotate("text", x=1999.9, y=56, label="Data represents hours worked per week for 52 countries   ", size=3, color="gray30") + 
  annotate("text", x=2000, y=55, label="from 1980 to 2007. Outer lighter bands show the min/max  ", size=3, color="gray30") +
  annotate("text", x=2000, y=54, label="hours for each year, and inner darker bands show the IQR.", size=3, color="gray30") + 
  ggtitle("World's Working Hours") +
  theme(plot.title=element_text(face="bold",hjust=.95,vjust=.8,color="#3C3C3C",size=20)) + 
  annotate("text", x=1994.6, y=57.5, label="Weekly", size=4, fontface="bold")
p1

p2 <- p + coord_cartesian(ylim=c(30, 38)) + 
  ggtitle("Inter-Quartile Range of Weekly Hours Worked Per Year Per Country") +
  theme(plot.title=element_text(face="bold",color="#3C3C3C",size=12)) + 
  geom_text(data = hours.long[hours.long$Country == "United States",], aes( x = Year, y = Hours, color = Country, group = Country, label = round(Hours, 2)), hjust = -.1, vjust = -1.2, size = 2, color = "brown") +
  geom_text( aes(x = Year, y = median, label = median), hjust = -.1, vjust = 1.2, size = 2, color = "black")
p2

p3 <- ggplot(subset(hours.long, Country %in% c("Switzerland", "United States", "Japan", "United Kingdom", "France")),
              aes(x = Country, y = Hours, fill= Country)) + 
  geom_boxplot() + 
  stat_summary(fun.y = mean, geom = "point", shape = 5) + 
  ylab("Hours worked per week") + xlab("") + 
  theme(legend.position = "none") + 
  ggtitle("Box plot of Hours worked weekly for 5 countries") + 
  theme(plot.title =element_text(face = "bold", color = "Black", size = 12))
p3

p4 <- ggplot(subset(hours.long, Country %in% c("Switzerland", "United States", "Japan", "United Kingdom", "France")), 
             aes(x = Year, y = Hours)) +
  geom_line(aes(color = Country, group = Country)) +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_y_continuous(breaks=seq(26, 56, 2)) +
  scale_x_continuous(breaks=seq(1980, 2005, 5), expand=c(0,0) ) +
  ylab("Hours worked per week") + xlab("") + 
  ggtitle("Hours worked weekly from 1980-2007 for 5 countries") + 
  theme(plot.title =element_text(face = "bold", color = "Black", size = 12))
p4