##################### STOCHASTIC SIMULATION #####################
#####################  HOMEWORK 1 ##############################
#Johannes Talero
library(ggplot2)

#2. Inverse method for discrete r.v.'s (Jones):
#  Consider the discrete random variable (?.v.) X with probability mass function (pmf ) given by:
#  P(X = ???1) = 0.2, P(X = 0) = 0.5, P(X = 1) = 0.3

#a) Calculate and plot the Cumulative Distribution Function (CDF) F_X(x) of X.
df<- data.frame('X'= -2:2, 'CDF'=c(0,0.2,0.7,1,1))
ggplot(df, aes(x=X, y=CDF),color='Red')+
  geom_point()+ theme_bw()+
  ggtitle('Cumu?ative Distribution Function of X')+
  theme(plot.title = element_text(hjust = 0.5))








