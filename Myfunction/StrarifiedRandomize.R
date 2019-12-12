######################################################################################################

#Function: stratified

#Author: Chang Shen

#Creation Date: November 14, 2019 (version 1.2.2)

#Purpose: This function will do a stratified randomization for a two-arm study.  
#         Individuals have an equal probability of being assigned to treatment or control
#         There is no guarantee of equal distribution to treatment arms

# Required Parameters: 
#      t - the number of treatment group 
#      s - the number of stratified
#      samplesize - when equal = TRUE, the stratified group is balanced distributed. it should contain only 1 number i.e.the whole sample size
#                   when equal = FALSE it should contain a vector which each entry conresponed to the number of each treatment
#                   by default, equal = TRUE
#
# Optional Parameters: 
#      equal- a logical parameter to determined whether the stratified group is even distributed or just follow by certain stratified number
#      seed - random seed for sampling


#Output: The function will return a list contains:
#         sample.random - a matrix with the individual identifier, stratum number, and the treatment assignment
#         distribution -  a matrix with the distribution of treatments by strata (i.e. a summary table);
#         seed - the seed used to generate the randomization scheme. 

#Example: The following gives the results 60 participants, 3 stratas, equal allocation to strata, 5 treatment groups, seed=907563
#         stratified(t=5, s=3, samplesize = 60, equal = TRUE, seed = 907563)
#         200 participants, 3 stratas, stratum sizes = 80, 70, 50, 3 treatment groups, seed=124589
#         stratified(t=3, s=3, samplesize = c(80, 70, 50), equal = FALSE, seed = 124589)

########################################################################################################

stratified <- function(t, s, samplesize, equal=TRUE, seed = sample(99999999,1)){
  #load the packages
  if(!"dplyr" %in%installed.packages()) install.packages("dplyr")
  library(dplyr)
  #check the restriction
  if(!t %in% 2:5){
    stop("'t' must range between 2 and 5")
  }
  if(!s %in% 1:8){
    stop("'s' must range between 1 and 8")
  }
  if(length(samplesize) == 1L && !(samplesize%%s == 0)){
    samplesize <- ceiling(samplesize/s)*s
  }
  if(length(samplesize) != s && (!equal)){
    stop("the dimension of sample size should equal to s when do unequal size stratified")
  }
  #set randomized seed
  set.seed(seed)
  #equal strata methods
  if(length(samplesize) == 1L  && equal){
    samplesize <- ceiling(samplesize/(s*t))*s*t
    sample.random <- data.frame(indicators = 1:samplesize,
                                stratumNo = rep(1:s,each = samplesize/s))%>%
                     mutate(treatment = as.vector(replicate(s, sample(t, sum(samplesize)/s,replace = TRUE))))
  }
  #strata by sample size 
  else if(length(samplesize) == s && (!equal)){
    samplesize <- ceiling(samplesize/t)*t
    sample.random <- data.frame(indicators = 1:sum(samplesize),
                                stratumNo = rep(1:s,samplesize))%>%
                     mutate(treatment = unlist(sapply(samplesize, function(x){sample(t, x, replace = TRUE)})))
  }
  #return sampling result
  distribution <- table(sample.random$stratumNo, sample.random$treatment)%>%
    `colnames<-`(paste("treatment",1:t))%>%
    `rownames<-`(paste("strata",1:s))
  list(sample.random = sample.random, seed=seed, distribution = distribution )
}

