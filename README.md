# Script to estimate the model parameters using a linear approximation

### Installing and loading in the packages
The package used in this method is "dplyr" which allows for the use of the pipe function.

```{r}
install.packages("dplyr")
library(dplyr)
```

### Loading in the data under the name growth_data
The dataset 'experiment1' includes the data from an experimental growth culture of _E. coli_ 
```{r}
growth_data <- read.csv("experiment1.csv")
```
## Case 1) K >> N0, t is small
In this scenario I attempt to estimate the starting population size (N0) and population growth rate (r) by setting a large carrying capacity and small time value. From the original population growth model, I can simplify the model under these limits and am now presented with an exponential model: N(t) = N0e^rt. Under a log transformation of the data I can produce a linear model from which we extract our estimates.

Here I set the time bounded under 1600 so that the only values considered from the logarithmic model that are linear. This is done by subsetting the data and filtering out all values t>1600. Additionally, this data is mutated such that all values are under a logarithmic transformation so they form a line instead of an exponential curve. This subset is ran through a linear model using the lm() function to extract the key values:
```{r}
data_subset1 <- growth_data %>% filter(t<1600) %>% mutate(N_log = log(N))
model1 <- lm(N_log ~ t, data_subset1)
summary(model1)
```
### The summary from the linear model estiamtes the y-intercept (N0) plotted as 6.903e+00 and gradient (r) estimated at 9.990e-03

## Case 2. N(t) = K
At N(t) = K, the population size at time t is equal to the carrying capacity (K). Under this scenario, I estimate K by simplifying our population growth model again such that as t tends to inifinty, the size of the population will equal K: lim N(t) = K. I follow the same procedure as the previous step to subset the data. By only considering t>1800, I fit the linear model with the plateau of the population growth model and find the estimate for the carrying capacity:
```{r}
data_subset2 <- growth_data %>% filter(t>1800)
model2 <- lm(N ~ 1, data_subset2)
summary(model2)
```
### The summary from this model estimates the carrying capacity (K) at 5.903e+10

# Script to plot the logistic growth data

### Loading in the data and installing the ggplot2 package
The ggplot2 package introduces many functions that allow for efficient plotting of graphs which is useful for this exercise.
```{r}
install.packages("ggplot2")
library(ggplot2)
```
## This code plots the logistic population growth model
```{r}
ggplot(aes(t,N), data = growth_data) +
  geom_point() +
  xlab("t") +
  ylab("y") +
  theme_bw()
```
## This code plots the population growth under a logarithmic transformation
```{r}
ggplot(aes(t,N), data = growth_data) +
  geom_point() +
  xlab("t") +
  ylab("y") +
  scale_y_continuous(trans='log10')
```
  # Script to plot data and model

### Loading in the data
```{r}
growth_data <- read.csv("experiment1.csv")
```
### Here I define the entire logistic population growth model under the values that I have estimated from the earlier analysis. The model is named logistic_fun.
```{r}
logistic_fun <- function(t) {
  N <- (N0*K*exp(r*t))/(K-N0+N0*exp(r*t))
  return(N) 
}
```
### Here I clearly define each of the parameters for the model with the estimates
```{r}
N0 <- 6.903e+00 #initial population size 
r <- 9.990e-03 #gradient (population growth rate) 
K <- 5.903e+10 #carrying capacity
```
### Next I plot the estimate model with ggplot against the true data to see how well the model fits the data
```{r}
ggplot(aes(t,N), data = growth_data) +
  geom_function(fun=logistic_fun, colour="red") +
  geom_point()
  scale_y_continuous(trans='log10')
```
## The trend of the model aligns well with the data but finds the starting pouplation size estimate to be too large which causes a lag in the model against the data

# Results

### In this exercise I have estimated the values of starting population size (N0), population growth rate (r), and carrying capacity (K) using our simplifying assumptions of a logistic population model to adjust the trend from the experimental data. The data used in this analysis was taken from the experiment1.csv file that was provided. These esitmations were produced as follows:
### N0 = 6.903e+00
### r = 9.990e-03
### K = 5.903e+10
### Following, I plotted the model and data together to see if the population growth model fits the data. The results show that the data does align with the trend of the estimated population growth model, however, there is a little bit of delay caused by the difference in the true and estiamted starting population size.
