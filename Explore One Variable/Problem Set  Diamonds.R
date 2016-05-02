# Problem Set One Variable
library(ggplot2)
data(diamonds)
summary(diamonds)
dim(diamonds)
str(diamonds)

ggplot(aes(x=price),data=diamonds)+
  geom_histogram()

summary(diamonds$price)

diamdons_less_500  <- subset(diamonds, diamonds$price <500)
dim(diamdons_less_500)

diamdons_less_250 <- subset(diamonds, diamonds$price <250)
dim(diamdons_less_250)

diamdons_more_15000  <- subset(diamonds, diamonds$price >=15000)
dim(diamdons_more_15000)


ggplot(aes(x=price),data=subset(diamonds,!is.na(diamonds$price)))+
  geom_histogram(binwidth = 100) +
  scale_x_continuous(limits = c(0,5000), breaks = seq(0,5000,100))


ggplot(aes(x=price),data=diamonds)+
  geom_histogram(binwidth = 100) +
  scale_x_continuous()+
  facet_wrap(~cut)

by(diamonds$price, diamonds$cut, summary)

qplot(x = price, data = diamonds) + facet_wrap(~cut,scales = "free_y")

ggplot(aes(x=price/carat),data=subset(diamonds,!is.na(diamonds$price)))+
  geom_histogram(binwidth = 0.25) +
  scale_x_log10()+
  facet_wrap(~cut)


ggplot(aes(x=color, y=price), data=subset(diamonds,!is.na(diamonds$price))) +
  geom_boxplot(aes(fill =color))

by(diamonds$price, diamonds$color, summary)

ggplot(aes(x=cut, y=price), data=subset(diamonds,!is.na(diamonds$price))) +
  geom_boxplot(aes(fill =cut))

by(diamonds$price, diamonds$cut, summary)

ggplot(aes(x=clarity, y=price), data=subset(diamonds,!is.na(diamonds$price))) +
  geom_boxplot(aes(fill =clarity))

by(diamonds$price, diamonds$clarity, summary)

color_D <- subset(diamonds,color =="D")
IQR(color_D$price)

color_J <- subset(diamonds,color =="J")
IQR(color_J$price)

ggplot(aes(x=color, y=price/carat), data=subset(diamonds,!is.na(diamonds$price))) +
  geom_boxplot(aes(fill =color))

ggpplot(aes(x=carat),data=subset(diamonds,!is.na(diamonds$carat))) +
  geom_freqpoly()

# Problem Set Two Variables

ggplot(aes(x = price, y = x), data = diamonds) +
  geom_point()

cor(x = diamonds$price, y = diamonds$x)
cor(x = diamonds$price, y = diamonds$y)
cor(x = diamonds$price, y = diamonds$z)

ggplot(aes(x = price, y = depth), data = diamonds) +
  geom_point()

summary(diamonds$carat)
cor(x = diamonds$price, y = diamonds$depth)

ggplot(aes(x = price, y = carat), data = diamonds) +
  geom_point() +
  ylim(c(quantile(diamonds$carat,0.01),5.01))

diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
ggplot(aes(x = price, y = volume), data = diamonds) +
  geom_point()

with(subset(diamonds, diamonds$volume!=0 & diamonds$volume < 800), cor.test(x=price, y = volume))

ggplot(aes(x = price, y = volume), data = subset(diamonds, diamonds$volume!=0 & diamonds$volume < 800)) +
  geom_jitter(alpha=1/10) +
  geom_smooth()

library(dplyr)

diamondsByClarity <- diamonds %>%
  group_by(clarity) %>%
  summarise(mean_price= mean(price),
            median_price =median(price),
            min_price =min(price),
            max_price =max(price),
            n =n())

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

library(gridExtra)

p1 <- ggplot(aes(x = clarity, y = mean_price), data = diamonds_mp_by_clarity)+
  geom_bar()

p2 <- ggplot(aes(x = colour, y = mean_price), data = diamonds_mp_by_color)+
  geom_bar()
grid.arrange(p1,p2, ncol=1)