# R-project

## shiny

It's a visualization project to visualize the time series data of australia weather and spatial map visualization from 2013 to 2017.
publish at [shinyaap.io changshen](https://changshen.shinyapps.io/shiny/)

## multivariate time series（Sydney Weather Forecasting） 
### 1.The presentation slide available at [here](https://slides.com/changshen/multivariate#/)
### 2. The proposal and report
### 3. code for data manipulation and modelling(to be uploaded)

## Myfunction
list some of the simple functions I write for statistical computing or visualization
### 1. Emprical Power
### 2. Visualization of output of Emperical power

>Example
```r
N <- c(100, 200, 300)
alpha <- .01
sd <- 5
delta <- seq(0.5, 5, 0.5)
mu1 <- 5
power.1<-Emperical.power(N, alpha, sd,delta,mu1)
View(power.1)
plot.emperical.power(power.1)
```

<img src="https://github.com/diana12333/R-project/blob/master/Myfunction/image/EmpricalPower1.png" width="600">

```r
N <- c(20, 40)
alpha <- c(.05, .10)
sd <- c(0.5,1)
delta <- seq(0.1, 1, 0.1)
mu1 <- 2
power.2<-Emperical.power(N, alpha, sd,delta,mu1)
View(power.2)
plot.emperical.power(power.2)
```

<img src="https://github.com/diana12333/R-project/blob/master/Myfunction/image/EmpricalPower2.png" width="650">

## Daxing Population Analysis
### the Map Visualization of Anaysis is as follows

<img src="https://github.com/diana12333/R-project/blob/master/DaxingPopulationAnalysis/map_new.gif" width="200">
