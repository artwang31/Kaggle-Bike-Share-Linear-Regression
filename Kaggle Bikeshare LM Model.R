library(tidyverse)
library(dplyr)
library(tidyr)
library(caTools)
library(ggplot2)
library(RColorBrewer)
library(wesanderson)
library(corrplot)

bike <- bikeshare
colnames(bike)
head(bike)

# Predict the total amount of bikes rented during each timestamp

count.temp <- ggplot(bike, aes(x = temp, y = count)) +
  geom_point(aes(color = temp), alpha = .1, size = 1) +
  scale_colour_gradient() +
  xlab('Temperature') +
  ylab('Count') + 
  ggtitle('Bikes Rented and Temperature') 

bike$datetime <- as.POSIXct(bike$datetime, tz = "%Y-%m-%d %H:%M", optional = FALSE)

count.datetime <- ggplot(bike, aes(x = bike$datetime, y = count)) +
  geom_point(aes(color = count), alpha = .15, size = 1) +
  scale_colour_gradient(low="blue", high="red") +
  xlab('Date & Time') +
  ylab('Count') + 
  ggtitle('Bikes Rented and Temperature') 
count.datetime

cor(bike$temp, bike$count)

# Seasonal bike rentals
bike$season <- factor(bike$season,
                           labels = c('Summer','Fall','Winter','Spring'))
count.season <- ggplot(bike, aes(x = season, y = count)) +
  geom_boxplot(aes(fill = season))
count.season

# Time Stamp for Hours in a Day
time.stamp <- bike$datetime
bike$datetimehour <- format(time.stamp, "%H")

# Bike Rentals and Temperature on Working Days
workingdays <- group_by(bike) %>% select(datetimehour, temp, workingday, count) %>% filter(workingday == 1)
ggplot(workingdays, aes(x = datetimehour, y = count)) +
  geom_point(aes(color = temp), alpha = .15, size = 2, position=position_jitter(w=1, h=0)) +
  scale_color_gradientn(colors=c('blue','yellow','dark green'))

# Bike Rentals and Temperature on Non-Working days
nonworkingdays <- group_by(bike) %>% select(datetimehour, temp, workingday, count) %>% filter(workingday == 0)
ggplot(nonworkingdays, aes(x = datetimehour, y = count)) +
  geom_point(aes(color = temp), alpha = .15, size = 2, position=position_jitter(w=1, h=0)) +
  scale_color_gradientn(colors=c('blue','purple','red'))

# Build Regression Model, predict count based on temperature
temperature.model <- lm(formula = count ~ temp, data = bike) 
summary(temperature.model)

# 6.0462 as intercept and 9.17 as the temperature coefficien
# Y value when X = 0, Around 6 bike rentals at 0 degrees. 
# Temperature is a huge factor in bike rentals
# 9.17 bikes per single (1) temperature increase

# Predicts the number of bikes rented at certain temperatures
predTemps <- data.frame(temp = c(25,26,27,28,29,30))
predict(temperature.model, predTemps)




