##################### STOCHASTIC SIMULATION ####################
#####################  HOMEWORK 1 ##############################
#Johannes Talero
library(ggplot2)
library(dplyr)
#2. Inverse method for discrete r.v.'s (Jones):
#  Consider the discrete rand?m variable (r.v.) X with probability mass function (pmf ) given by:
#  P(X = ???1) = 0.2, P(X = 0) = 0.5, P(X = 1) = 0.3

#a) Calculate and plot the Cumulative Distribution Function (CDF) F_X(x) of X.
df<- data.frame('X'= -2:2, 'CDF'=c(0,0.2,0.7,1,1))
ggplot(df, aes(x=X, y=CDF),color='Red')+
  geom_point()+ theme_bw()+
  ggtitle('Cumu?ative Distribution Function of X')+
  theme(plot.title = element_text(hjust = 0.5))

#b) Write a program to generate n values of X with this distribution, using the function runif.
inver_trans_X<-function(uni){
    if (uni<=0.2)
    {val=-1}
    else if (u?i<=0.7)
    {val=0} 
    else 
    {val=1} 
  return(val)
}
random_x<-function(n=1){
  vec<-sapply(runif(n),inver_trans_X)
  return(vec)
}
#c) Let n = 1000, run the program, and determine the proportion of values that are equal to 1.
set.seed(123)
n<-1000
?ample_X <-data.frame('ID'= 1:n, 'X'=random_x(n))
sample_X %>%group_by(X)%>%count(X)/n
#d) Repeat c) with n = 10000.
set.seed(123)
n<-10000
sample_X <-data.frame('ID'= 1:n, 'X'=random_x(n))
sample_X %>%group_by(X)%>%count(X)/n






