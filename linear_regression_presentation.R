# LINEAR REGRESSION (lm())
#
# by thomas.stump@bluewin.ch
# CAS Big Data Analysis
# HWZ - Zurich - Switzerland
# 2018-Q4

## Data Frame used
# New York Air Quality Measurements
# Daily air quality measurements in New York, May to September 1973.
#
# A data frame with 154 observations on 6 variables.
# 
# [,1]	Ozone	numeric	Ozone (ppb)
# [,2]	Solar.R	numeric	Solar R (lang)
# [,3]	Wind	numeric	Wind (mph)
# [,4]	Temp	numeric	Temperature (degrees F)
# [,5]	Month	numeric	Month (1--12)
# [,6]	Day	numeric	Day of month (1--31)
#
# Ozone: Mean ozone in parts per billion from 1300 to 1500 hours at Roosevelt Island
# Solar.R: Solar radiation in Langleys in the frequency band 4000–7700 Angstroms
# Wind: Average wind speed in miles per hour at 0700 and 1000 hours at LaGuardia Airport
# Temp: Maximum daily temperature in degrees Fahrenheit at La Guardia Airport.

# Do a linear regression analysis for numeric predictor and response

library(psych)
library(ggplot2)

str(airquality)
summary(airquality)

# Remove NAs
airquality_wa_NA <- airquality[complete.cases(airquality),]

# Graphical representation of data frame (Histograms and Correlations)
pairs.panels(airquality_wa_NA, main="New York Air Quality Measurements")

# Check for linear regression Ozone vs. Temp

# Do a correlation test
cor.test(airquality_wa_NA$Ozone, airquality_wa_NA$Temp)
# cor = 0.699 > positiv correlation

# Establish regression model
airq_model <- lm(Ozone ~ Temp,airquality_wa_NA)

airq_model
# R-Model: Ozone = -147.646 * 2.439 Temp

summary(airq_model)
# R-Squared = 0.483
# p-value = < 2.2e-16
# With P-Value less than 5% we can assume for a statistical significant correlation

# Predict data

# Add prediction values to DF in new column
airquality_wa_NA$prediction <- predict(airq_model)

# Check prediction in data plot
# Seen in DataCamp - Supervised Learning in R: Regression

ggplot(airquality_wa_NA, aes(x=prediction, y=Ozone)) +
  geom_point() +
  geom_abline(color = "blue")

# Do prectiion for new values

ozone_predict.data <- data.frame(Temp=c(65,70,75))
ozone_predict.prediction <- predict(airq_model,ozone_predict.data)
ozone_predict.prediction

# Temp = 65 > Ozone = 10.9
# Temp = 70 > Ozone = 23.1
# Temp = 75 > Ozone = 35.3






