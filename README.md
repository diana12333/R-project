# R-project

## Old Faithful data manipulation project
> regex expression /rmd report/ rmd latex

see the report [here](https://htmlpreview.github.io/?https://github.com/diana12333/R-project/blob/master/Old%20Faithful%20Raw%20Data%20manipulation%20and%20Modeling/OldFaithful_DanEdit.html)

## shiny

It's a visualization project to visualize the time series data of australia weather and spatial map visualization from 2013 to 2017.
publish at [shinyaap.io changshen](https://changshen.shinyapps.io/shiny/)

## multivariate time series（Sydney Weather Forecasting） 
#### 1.The presentation slide available at [here](https://slides.com/changshen/multivariate#/)
#### 2. The proposal and report
#### 3. code for data manipulation and modelling(to be uploaded)

## Data Manipulation
### Use advanced R to conduct data manipulation and anlysis 
#### 1.Clinical Data
including the database join(merge),cleaning, format standardization(from SAS to R), group and summarize

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

<img src="https://github.com/diana12333/R-project/blob/master/Myfunction/image/EmpricalPower1.png" width="500">

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

<img src="https://github.com/diana12333/R-project/blob/master/Myfunction/image/EmpricalPower2.png" width="550">

### 3. stratified
> a function for stratified randomization for a two-arm study
> Example: returned a list include stratified randomized data set, seed, distribution summary
```r
testa <- stratified(t=2, s=4, samplesize = 100, equal = TRUE, seed = 89676);testa
testb <- stratified(t=3, s=3, samplesize = c(80, 70, 50), equal = FALSE, seed = 124589);testb
testc <- stratified(t=5, s=3, samplesize = 60, equal = TRUE, seed = 907563);testc
```


## Daxing Population Analysis
### the Map Visualization of Anaysis is as follows

<img src="https://github.com/diana12333/R-project/blob/master/DaxingPopulationAnalysis/map_new.gif" width="200">
