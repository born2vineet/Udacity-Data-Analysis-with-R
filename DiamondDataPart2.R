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

data(diamonds)

ggplot(data =diamonds, aes(x = x, y = price)) +
  geom_point(alpha = 1/20) + 
  coord_cartesian(xlim = c(3.5, 12)) + 
  scale_y_continuous(breaks = seq(1000, 20000, 1000), labels = dollar)

cor.test(diamonds$price, diamonds$x, method = 'pearson')
with(diamonds, cor.test(price, y, method = 'pearson'))
with(diamonds, cor.test(price, z))

ggplot(diamonds, aes(x = depth, y = price)) + 
  geom_point()

ggplot(data = diamonds, aes(x = depth, y = price)) + 
  geom_point(alpha = 1/100) + 
  scale_x_continuous(breaks = seq(min(diamonds$depth), max(diamonds$depth), 2),
                     labels = seq(min(diamonds$depth), max(diamonds$depth), 2))

with(diamonds, cor.test(depth, price))

ggplot(diamonds, aes(x = carat,  y = price)) + 
  geom_point(alpha = 1/20) + 
  scale_x_continuous(limits = c(0, quantile(diamonds$carat, 0.99))) + 
  scale_y_continuous(breaks = seq(0, 18000, 2000),
                     limits = c(0, quantile(diamonds$price, 0.99)),
                     labels = dollar)

diamonds2 <- diamonds %>%
  mutate(volume = x*y*z)
ggplot(diamonds2, aes(x = volume, y = price)) +
  geom_point()

count(diamonds2$volume == 0)

with(subset(diamonds2, !(volume == 0 | volume >= 800)), 
     cor.test(price, volume))

smaller <- diamonds2 %>%
  filter(volume !=0, volume <= 800)

ggplot(smaller, aes(x = volume, y = price)) + 
  geom_point(alpha = 1/20) + 
  geom_smooth(method = 'lm', se = TRUE)

diamondsByClarity <- diamonds %>%
  group_by(clarity) %>%
  summarise(mean_price = mean(price),
            median_price = median(price),
            min_price = min(price),
            max_price = max(price), 
            n = n() )%>%
  arrange(clarity)

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))


p1 <- ggplot(diamonds_mp_by_clarity, aes(x=clarity, y=mean_price, fill=clarity)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_brewer(palette="Set3") + 
  guides(fill = guide_legend(ncol=2, title.hjust=0.3))

p2 <- ggplot(diamonds_mp_by_color, aes(x=color, y=mean_price, fill=color)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_brewer(palette="Set2") + 
  guides(fill = guide_legend(ncol=2, title.hjust=0.4))

grid.arrange(p1, p2)
