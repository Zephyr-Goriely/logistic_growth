#Script to plot data and model

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

#Plotting the model on the data to see how well the model fits the data
