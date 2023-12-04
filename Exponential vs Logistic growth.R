# Exponential vs Logistic growth modelled

# Reading in the data
growth_data <- read.csv("experiment1.csv")

# Defining a logistic function that will be plotted using the equation of a typical logistic population growth model
logistic_fun <- function(t) {
  
  N <- (N0*K*exp(r*t))/(K-N0+N0*exp(r*t))
  
  return(N)

}
# Defining. an exopnential function which will be used to compare against the logistic model, the equation used here is from the typical exponential growth model
exponential_fun <- function(t) {
  N <- (N0*exp(r*t))
  
  return(N)
}

# Defining the model estimates using the values from the previous exercise
N0 <- 6.903e+00 #initial population size

r <- 9.990e-03 #gradient

K <- 5.903e+10 #carrying capacity

# Code to plot the two functions against each other such that the trend is visible
ggplot(data = growth_data, aes(t,N)) +
  geom_function(fun=logistic_fun, linetype = "solid", aes(color = "Logistic")) +
  geom_function(fun=exponential_fun, linetype = "dashed", aes(color = "Exponential")) +
  xlim(1500,3200) +
  ylim(0, 6e+10) +
  labs(title = "Exponential vs Logistic growth",
       x = "Time",
       y = "Population Size",
       color = "Function Type") +
  scale_color_manual(values = c("red", "blue"), name = "Function Type")

