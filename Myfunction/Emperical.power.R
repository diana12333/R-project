######################################################################################################
#Function: Emperical.power

#Author:Chang Shen

#Creation Date:  November 30, 2019

#Purpose:   To create a dataframe with each sample size(N), alpha(level of significance), 
#         sd(the standard deviation of groups), delta(he true different between group2 and group1),
#         true mean of group1 and the true power and emperical power with simulation
#        

# Required Parameters: 
#N: vector of The total sample size of 2 groups(1:1 setting) (must be integer)
#alpha: vector of  level of significance (must be value between 0 and 1)
#sd ：vector of the standard deviation of groups
#delta : vector of the true different between group2 and group1 i.e. mu2 - mu1
#mu1 : vector of true mean of group1

# Optional Parameters
# seed: random seed for simulation default is random integer within [0, 99999999]
# simulation: number of simulation for emperical power calculation


#Output:  A data.frame with N alpha sd delta mu and the true power and simulation emperical power correspond to each (N alpha sd delta mu)

#Required packages: 
# dplyr

#Example: 
#N <- c(100, 200, 300)
#alpha <- .01
#sd <- 5
#delta <- seq(0.5, 5, 0.5)
#mu1 <- 5
#power.1<-Emperical.power(N, alpha, sd,delta,mu1)         

########################################################
Emperical.power <- function(N, alpha, sd, delta,mu1,seed = sample(99999999,1),simulation = 1000){
  set.seed(seed)
  
  #load packages you need
  if(!"dplyr" %in%installed.packages()) install.packages("dplyr")
  require(dplyr)
  
  #check the valid condiyion for alpha and N
  if(sum(alpha>1|alpha<0)>0){ stop("alpha should be within 0 and 1")}
  if(sum(round(N)!=N)!=0){stop("N should be a integer")}
  
  #generate the final table by expanding grid
  result.table <- expand.grid(N, alpha, sd, delta,mu1)
  colnames(result.table) <- c("N", "alpha", "sd", "delta","mu1")
  
  #internal function for emperical power calculation
  emperical <- function(i){
    with(result.table,sum(replicate(simulation,t.test(rnorm(N[i]/2,mu1[i],sd[i]),rnorm(N[i]/2,mu1[i]+
                                                                                         delta[i],sd[i]))$p.value < alpha[i]))/simulation)}
  
  #calculate the final data frame with the parameters you provide
  result.table <- result.table%>%mutate(
    true.power = pnorm(sqrt(N*delta^2/(4*sd^2))-qnorm(1-alpha/2)),
    emperical.power = sapply(1:dim(result.table)[1], emperical)
  )
  #return the result with each parameter
  return(result.table)
}



