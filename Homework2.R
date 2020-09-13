##################### STOCHASTIC SIMULATION ####################
#####################  HOMEWORK 2 ##############################
#Johannes Talero
library(ggplot2)
library(dplyr)
#1. Stopping generating new simulation data
X_var<-0
n<-99
d<-0.1
S=1
while ((S/sqrt(n))>=d){
  n<-n+1
  set.seed(3)
  x<-rnorm(n,0,1)
  X_Var<-mean(x)
  S<-sqrt(sum((x-X_Var)^2)/(n-1))
  }

