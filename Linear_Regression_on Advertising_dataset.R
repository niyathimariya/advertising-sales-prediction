install.packages("ggplot2")
library(ggplot2)
ad<- read.csv("C:/Users/nmg28/Downloads/advertising.csv")
View(ad)

t=lm(Sales~TV,ad)
summary(t)

ggplot(data = ad, aes(y = Sales, x = TV)) +
  geom_point(alpha = 0.2) +        # Add data points with transparency
  geom_smooth(method = "lm", method.args = list(family = "binomial"), se = FALSE) 

r=lm(Sales~Radio,ad)
summary(r)

ggplot(data = ad, aes(y = Sales, x = Radio)) +
  geom_point(alpha = 0.2) +        # Add data points with transparency
  geom_smooth(method = "lm", method.args = list(family = "binomial"), se = FALSE) 

n=lm(Sales~Newspaper,ad)
summary(n)

ggplot(data = ad, aes(y = Sales, x = Newspaper)) +
  geom_point(alpha = 0.2) +        # Add data points with transparency
  geom_smooth(method = "lm", method.args = list(family = "binomial"), se = FALSE) 

tr=lm(Sales~TV+Radio,ad)
summary(tr)

ggplot(data = ad, aes(y = Sales, x = TV+Radio)) +
  geom_point(alpha = 0.2) +        # Add data points with transparency
  geom_smooth(method = "lm", se = FALSE) 

tn=lm(Sales~TV+Newspaper,ad)
summary(tn)

ggplot(data = ad, aes(y = Sales, x = TV+Newspaper)) +
  geom_point(alpha = 0.2) +        # Add data points with transparency
  geom_smooth(method = "lm", method.args = list(family = "binomial"), se = FALSE) 

trn=lm(Sales~TV+Radio+Newspaper,ad)
summary(trn)

ggplot(data = ad, aes(y = Sales, x = TV+Radio+Newspaper)) +
  geom_point(alpha = 0.2) +        # Add data points with transparency
  geom_smooth(method = "lm", method.args = list(family = "binomial"), se = FALSE) 
