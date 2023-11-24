# Script to estimate the model parameters using a linear approximation

install.packages("dplyr")

library(dplyr)

growth_data <- read.csv("experiment1.csv")

#Case 1. K >> N0, t is small

data_subset1 <- growth_data %>% filter(t<1600) %>% mutate(N_log = log(N))

model1 <- lm(N_log ~ t, data_subset1)
summary(model1)

## Intercept plotted as 6.903e+00 and gradient estimated at 9.990e-03

#Case 2. N(t) = K

data_subset2 <- growth_data %>% filter(t>1800)

model2 <- lm(N ~ 1, data_subset2)
summary(model2)

## Carrying capacity estimated as 5.903e+10

# Script to plot the logistic growth data

growth_data <- read.csv("experiment1.csv")

install.packages("ggplot2")
library(ggplot2)

ggplot(aes(t,N), data = growth_data) +
  
  geom_point() +
  
  xlab("t") +
  
  ylab("y") +
  
  theme_bw()

ggplot(aes(t,N), data = growth_data) +
  
  geom_point() +
  
  xlab("t") +
  
  ylab("y") +
  
  scale_y_continuous(trans='log10')

  # Script to plot data and model

growth_data <- read.csv("experiment1.csv")

logistic_fun <- function(t) {
  
  N <- (N0*K*exp(r*t))/(K-N0+N0*exp(r*t))
  
  return(N)
  
}

N0 <- 6.903e+00 #initial population size
  
r <- 9.990e-03 #gradient
  
K <- 5.903e+10 #carrying capacity

ggplot(aes(t,N), data = growth_data) +
  
  geom_function(fun=logistic_fun, colour="red") +
  
  geom_point()+

  scale_y_continuous(trans='log10')

## Plotting the model on the data to see how well the model fits the data
