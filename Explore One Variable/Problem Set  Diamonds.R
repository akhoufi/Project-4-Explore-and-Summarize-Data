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
  geom_bar(stat = 'identity')

p2 <- ggplot(aes(x = color, y = mean_price), data = diamonds_mp_by_color)+
  geom_bar(stat = 'identity')
grid.arrange(p1,p2, ncol=1)

# Problem Set Three Variables
ggplot(aes(x = price),
       data = diamonds) +
  geom_histogram(aes(fill = cut)) +
  facet_wrap( ~ color) +
  scale_x_log10() +
  scale_y_continuous(limits = c(0,600)) +
  scale_fill_brewer(type = 'qual')


ggplot(aes(x = table, y = price), 
       data = diamonds) +
  geom_point(aes(color = cut)) +
  scale_color_brewer(type = 'qual') +
  scale_x_continuous(limits = c(50,80), breaks = seq(50,80,2))

ggplot(aes(x = volume, y = price),
       data = subset(diamonds,!is.na(diamonds$price) & volume > 0)) +
  geom_point(aes(color = clarity, size =1)) +
  scale_color_brewer(type = 'div') +
  scale_x_continuous(limits = c(0,350)) +
  scale_y_log10()

pf  <- read.csv('pseudo_facebook.tsv', sep='\t')
pf$prop_initiated <- pf$friendships_initiated / pf$friend_count

pf$year_joined <-floor( 2014-pf$tenure/365)
pf$year_joined.bucket <- cut(pf$year_joined,c(2004,2009,2011,2012,2014))
ggplot(aes(x=tenure, y = prop_initiated),
       data = subset(pf,!is.na(tenure)&!is.na(prop_initiated))) +
  geom_smooth(aes(color = year_joined.bucket), stat = 'summary', fun.y = median) +
  scale_y_continuous(limits =c(0,0.75), breaks = c(0,0.25,0.50,0.75))

pf_year_2012 <- subset(pf, pf$year_joined.bucket == '(2012,2014]')
with(pf_year_2012, summary(prop_initiated))

ggplot(aes(x=cut, y=price/carat), 
       data=diamonds) +
  facet_wrap(~clarity) +
  geom_jitter(aes(color=color)) +
  scale_color_brewer(type = 'div')
