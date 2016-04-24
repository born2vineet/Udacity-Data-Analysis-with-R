library(ggplot2)
data("diamonds")

names(diamonds)
summary(diamonds)

qplot(price, data = diamonds)

summary(diamonds$price < 500) # 1729
summary(diamonds$price < 250) # 0
summary(diamonds$price >= 15000) # 1656

qplot(price, data = diamonds, binwidth = 25,
      color = I('black'), fill = I('#099DD9')) +
  scale_x_continuous(breaks = seq(300, 2000, 100)) +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_cartesian(c(0,2000)) +
  xlab("Price") + ylab("Count")

qplot(price, data = diamonds, binwidth = 25,
      color = I('black'), fill = I('#099DD9')) +
  scale_x_continuous(breaks = seq(0, 4000, 100)) +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_cartesian(c(0, 4000)) +
  xlab("Price") + ylab("Count") +
  facet_grid(cut~.)
  
by(diamonds$price, diamonds$cut, max) # Premium
by(diamonds$price, diamonds$cut, min) # Premium & Ideal
by(diamonds$price, diamonds$cut, median) # Ideal

qplot(x = price, data = diamonds) + facet_wrap(~cut, scales = "free")

ggplot(aes(x = price/carat), data = diamonds) + 
  geom_histogram(color = "black", fill = "DarkOrange", binwidth = .05) +
  scale_x_log10() + facet_wrap(~cut, scales = "free")
  
qplot(x = carat, y = price, data = diamonds, geom='boxplot') + 
  scale_y_continuous(breaks = seq(0, 10000))

ggplot(diamonds, aes(x = clarity, y = price, color = cut)) + 
  geom_boxplot() + 
  facet_grid(color~., margins = TRUE)