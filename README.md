# Question 1

## Script to estimate the model parameters using a linear approximation

#### Installing and loading in the packages
The package used in this method is "dplyr" which allows for the use of the pipe function.

```{r}
install.packages("dplyr")
library(dplyr)
```

#### Loading in the data under the name growth_data
The dataset 'experiment1' includes the data from an experimental growth culture of _E. coli_ 
```{r}
growth_data <- read.csv("experiment1.csv")
```
### Case 1) K >> N0, t is small
In this scenario I attempt to estimate the starting population size (N0) and population growth rate (r) by setting a large carrying capacity and small time value. From the original population growth model, I can simplify the model under these limits and am now presented with an exponential model: N(t) = N0e^rt. Under a log transformation of the data I can produce a linear model from which we extract our estimates.

Here I set the time bounded under 1600 so that the only values considered from the logarithmic model are linear. This is done by subsetting the data and filtering out all values t>1600. Additionally, this data is mutated such that all values are under a logarithmic transformation so they form a line instead of an exponential curve. This subset is ran through a linear model using the lm() function to extract the key values:
```{r}
data_subset1 <- growth_data %>% filter(t<1600) %>% mutate(N_log = log(N))
model1 <- lm(N_log ~ t, data_subset1)
summary(model1)
```
<img width="469" alt="Case_1_linear_model_summary" src="https://github.com/hiddenuser3/logistic_growth/assets/150150268/b8669e5f-9d90-4918-87c5-16595ce3e457">

(Intercept = N_log: exp(6.903e+00) = N0)

#### The summary from the linear model estimates the initial population size (H0) as 995.256 and population growth rate (r) estimated at 9.990e-03

### Case 2. N(t) = K
At N(t) = K, the population size at time t is equal to the carrying capacity (K). Under this scenario, I estimate K by simplifying our population growth model again such that as t tends to inifinty, the size of the population will equal K: lim N(t) = K. I follow the same procedure as the previous step to subset the data. By only considering t>1800, I fit the linear model with the plateau of the population growth model and find the estimate for the carrying capacity:
```{r}
data_subset2 <- growth_data %>% filter(t>1800)
model2 <- lm(N ~ 1, data_subset2)
summary(model2)
```
<img width="458" alt="Case_2_linear_model_summary" src="https://github.com/hiddenuser3/logistic_growth/assets/150150268/8167e265-d6e2-42e2-bc87-b65fc6e464d1">


#### The summary from this model estimates the carrying capacity (K) at 5.979e+10

## Script to plot the logistic growth data
Above I made estimates for the population growth trend using simplifications of the typical population growth equation. Below I plot the true data to observe the trend that actually occurs from this experiment. This is done through the ggplot2 package which loads many functions that allow for efficient graph plotting in R:
```{r}
install.packages("ggplot2")
library(ggplot2)
```
#### Next the logistic population growth model can be plotted using the growth data:
```{r}
ggplot(aes(t,N), data = growth_data) +
  geom_point() +
  xlab("time (min)") +
  ylab("N #Cells") +
  theme_bw()
```
![logistic_growth_data-2](https://github.com/hiddenuser3/logistic_growth/assets/150150268/04557a02-9d06-49d0-9445-12ba9976ed25)



While this figure is useful in showing the general trend of the population growth. In order to compare it to the estimated model, it may be more useful to transform the model under a logarithimic transformation
#### This code plots the population growth under a logarithmic transformation
```{r}
ggplot(aes(t,N), data = growth_data) +
  geom_point() +
  xlab("t") +
  ylab("y") +
  scale_y_continuous(trans='log10') # This line log transforms the data
```

![logarithmic_growth_data](https://github.com/hiddenuser3/logistic_growth/assets/150150268/d2c3eca4-d701-41fa-94ed-124637212e3d)


## Script to plot data and model
Now that the data has been plotted and the model esitmates have been achieved. The model can be compared with the data to see how well the model fits the data. If the trends align, the conclusion can be drawn that the experimental _E.coli_ experienced population growth that is well simlulated by a logistic population growth model.

#### First I define the entire logistic population growth model under the values that I have estimated from the earlier analysis. The model is named logistic_fun.
```{r}
logistic_fun <- function(t) {
  N <- (N0*K*exp(r*t))/(K-N0+N0*exp(r*t))
  return(N) 
}
```
The values of the model estimates are set below:
```{r}
N0 <- 995.256 #initial population size 
r <- 9.990e-03 #gradient (population growth rate) 
K <- 5.979e+10 #carrying capacity
```
#### Next I plot the estimate model with ggplot against the true data on the same graph to see how well the population growth model fits the data:
```{r}
ggplot(aes(t,N), data = growth_data) +
  geom_function(fun=logistic_fun, colour="red") +
  geom_point()
  scale_y_continuous(trans='log10')
```

![log_model](https://github.com/hiddenuser3/logistic_growth/assets/150150268/9630236f-767f-494d-bc78-7dfdbf200fa3)


#### The trend of the model aligns well with the data suggesting that the population growth observed follows a typical logistic pattern. Although the shown trend is a logarithmic trend, this is due to the transformation of the data and the relationship is in fact logistic.

## Results

#### In this exercise I have estimated the values of starting population size (N0), population growth rate (r), and carrying capacity (K) using our simplifying assumptions of a logistic population model to adjust the trend from the experimental data. The data used in this analysis was taken from the experiment1.csv file that was provided. These estimations were produced as follows:
### N0 = 995.256
### r = 9.990e-03
### K = 5.979e+10
#### Following, I plotted the model and data together to see if a logistic population growth model fits the trend of the data. The results show that the data does align with the trend of the estimated population growth model. From this I can conlcude that the _E. coli_ growth culture follows a typical logistic growth pattern in the laboratory controlled conditions.

# Question 2

Assuming that the population grows exponentially, it will follow the equation N(t) = N0e^rt. Therefore, using the estimates from the previous exercise, the population size at t = 4980 mins can be calculated as N(4980) = 995.256*e^(9.990e-03*4980)
### = 4.02e+24

The population size predicted under logistic growth at t = 4980 mins will equal the carrying capacity as the population growth will have plateaued far before this time. Therefore, under logsitic growth, the popoulation size at t = 4980 mins would equal K
### = 5.979e+10

4.02e+24 >> 5.979e+10. As observed, under an assumption of exponential growth, the population size would be far greater than under logistic growth. This is expected as a logistic model is regulated by density dependence which prevents the population size from exceeding a certain point. The model creates density dependence through the use of the carrying capcity variable, which determines a maximum population size that can be maintained in this experiment. However, the exponential model does not have this regulation and is not limited by any factors, this means that the population would follow an exponential trend and grow infinitely. As such, the population under an exponential would exceed the population under a logistic model once the density dependence starts to plateau the curve of logsitic growth.

# Question 3

The R file 'Exponential vs Logistic growth' in this repository contains the code I used to plot the logistic model against the exponential to observe the difference in behaviour of these two models. This graph is shown below:

![exp_vs_logistic](https://github.com/hiddenuser3/logistic_growth/assets/150150268/75a0bb0d-d762-4be0-acc4-2ccfd25c9f73)
