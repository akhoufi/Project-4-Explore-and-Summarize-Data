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

ggplot(aes(x=carat),data=subset(diamonds,!is.na(diamonds$carat))) +
  geom_freqpoly(binwidth=0.1) +
  scale_x_continuous(breaks = seq(0,6,0.1)) +
  scale_y_continuous(breaks = seq(0,10000,1000))
  
  