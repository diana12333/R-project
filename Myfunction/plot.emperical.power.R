######################################################################################################
#Function: plot.emperical.power

#Author: Chang Shen

#Creation Date:  November 30, 2019

#Purpose:  Visualization function for  the result of Emperical.power

# Required Parameters: 
#result.table : output of Emperical.power function 


#Output:  A line plot grouped by sample size and with facet of alpha and sd

#Required packages: ggplot2 devtools ggthemr pacman

#Example: plot.emperical.power(power.1)
#         
########################################################
plot.emperical.power <- function(result.table){
  if(!"pacman" %in%installed.packages()) install.packages("pacman")
  require(pacman)
  p_load(devtools,ggplot2)
  devtools::install_github('cttobin/ggthemr')
  ggthemr('fresh')
  p <- ggplot(result.table, aes(delta, emperical.power,group =N, color = as.factor(N))) + geom_point()+
    geom_line()+labs(color = "sample size") + facet_grid(vars(sd),vars(alpha),labeller = label_both)+
    theme_bw()+ggtitle("Emperical power Visualization")
  p
}