# Script to estimate the model parameters using a linear approximation

### Installing and loading in the packages
install.packages("dplyr")

library(dplyr)

### Loading in the data under the name growth_data
growth_data <- read.csv("experiment1.csv")

## Case 1) K >> N0, t is small

### In this scenario we are attempting to estimate the starting population size and gradient by setting a large carrying capacity and small time value. From the original population growth model, we can simplify under these limits and are now presented with an exponential model: N(t) = N0e^rt. Under a log transformation of the data we can produce a linear model from which we extract our estimates. Here I set the time bounded under 1600 so that the only values considered from the logarithmic model that are linear.

data_subset1 <- growth_data %>% filter(t<1600) %>% mutate(N_log = log(N))

model1 <- lm(N_log ~ t, data_subset1)

summary(model1)

### The summary from the linear model estiamtes the y-intercept (N0) plotted as 6.903e+00 and gradient (r) estimated at 9.990e-03

## Case 2. N(t) = K
### At N(t) = K, the population size at time t is equal to the carrying capacity (K). Under this scenario, we estimate K by simplifying our population growth model again such that as t tends to inifinty, the size of the population will equal K: lim N(t) = K. We can follow the same procedure as the last step. By only considering t>1800, we can fit the linear model with the plateau of the population growth model and find the estimate for the carrying capacity.

data_subset2 <- growth_data %>% filter(t>1800)

model2 <- lm(N ~ 1, data_subset2)
summary(model2)

### The summary from this model estimates the carrying capacity (K) at 5.903e+10

# Script to plot the logistic growth data

### Loading in the data and installing the ggplot2 package
growth_data <- read.csv("experiment1.csv")

install.packages("ggplot2")
library(ggplot2)

## This code plots the logistic population growth model

ggplot(aes(t,N), data = growth_data) +
  geom_point() +
  xlab("t") +
  ylab("y") +
  theme_bw()

## This code plots the population growth under a logarithmic transformation

ggplot(aes(t,N), data = growth_data) +
  geom_point() +
  xlab("t") +
  ylab("y") +
  scale_y_continuous(trans='log10')

  # Script to plot data and model

### Loading in the data

growth_data <- read.csv("experiment1.csv")

### Here I define the entire logistic population growth model under the values that I have estimated from the earlier analysis. The model is named logistic_fun.

logistic_fun <- function(t) {
  N <- (N0*K*exp(r*t))/(K-N0+N0*exp(r*t))
  return(N) 
}

## Here I clearly define each of the variables for the model with the estimates

N0 <- 6.903e+00 #initial population size
  
r <- 9.990e-03 #gradient
  
K <- 5.903e+10 #carrying capacity

### Next I plot the estimate model with ggplot against the true data to see how well the model fits the data

ggplot(aes(t,N), data = growth_data) +
  geom_function(fun=logistic_fun, colour="red") +
  geom_point()
  scale_y_continuous(trans='log10')

## The trend of the model aligns well with the data but finds the starting pouplation size estimate to be too large which causes a lag in the model against the data
