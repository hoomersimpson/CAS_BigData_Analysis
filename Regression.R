## Aufgabe -1- ##
attach(mtcars)

plot(Bauvolumen$Bev,Bauvolumen$Bau)
baulm <- lm(Bau ~ Bev, Bauvolumen)
summary(baulm)
# Bau = -0.06 * 0.071 x Bev
# Adj - R-squared = 0.922

baulm_cor_test <- cor.test(Bauvolumen$Bau,Bauvolumen$Bev)
baulm_cor_test
# p-value = 1.9e-14, cor = 0.962 > Strong positive correlation

# Draw regression line into scatter plot
abline(baulm, col= "red")

# Do prediction
pred.bev <- c(2.5, 3.0)
pred.df <- data.frame(Bev=pred.bev)

# predict(baulm,newdata=pred.df,interval="prediction")
predict(baulm,newdata=pred.df)
# Bau_1 = 0.1176402
# Bau_2 = 0.1533450

## Aufgabe -2- **

set.seed(7777)
n <- 100
alter <- round((20 + 30 * runif(n)),1)
alter
lohn <- round((2 * Alter + 40 * runif(n)),1)
lohn
plot (alter,lohn)
cor(alter, lohn)

## Aufgabe -3- ##

library(MASS)
cats <- cats
summary(cats)
str(cats)
plot(cats$Bwt,cats$Hwt)
cats.lm <- lm(Hwt ~ Bwt, cats)
summary(cats.lm)
abline(cats.lm, col="red")
cats.lm.sex <- lm(Hwt ~ Bwt + Sex, cats)
summary(cats.lm.sex)
