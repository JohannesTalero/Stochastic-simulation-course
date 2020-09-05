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
count_case <-sample_X %>%group_by(X)%>%count(X)
count_case$relative_f <-count_case$n/n
#d) Repeat c) with n = 10000.
set.seed(123)
n<-10000
sample_X <-data.frame('ID'= 1:n, 'X'=random_x(n))
count_case <-samp?e_X %>%group_by(X)%>%count(X)
count_case$relative_f <-count_case$n/n


#3. Inverse method for continuous r.v.'s (Jones):
#Consider the continuous random variable X with probability density function (pdf ) given by:
#  fX(x) = ( 3(x ??? 1)**2  for 1 < x ??? 2, 
#            0            otherwise.

#b) Find the CDF FX(x) of X. Plot this function
F_X<-function(uni){
  val=uni*3
  if (val<=1)
  {num=0}
  else if (val<=2)
  {num=(val-1)**3} 
  else 
  {num=1} 
  return(num)
}

set.seed(123)
n=10000
ran<-runif(n?
vec<-sapply(ran,F_X)

df<- data.frame('X'= ran*3, 'CDF'=vec)

ggplot(df, aes(x=X, y=CDF))+
  geom_line(color="darkblue")+theme_bw()+
  ggtitle('Cumulative Distribution Function of X')+
  theme(plot.title = element_text(hjust = 0.5))


#d) Write a program ?n R that draws 1000 samples of X.
#Plot a normalized histogram of thesample along with the PDF o fX.

pdf_X<- function(x){
  if (x>1 && x<=2)
  {return((3*(x-1)**(2))/9)}#Change in dimension only for graph
  else{return(0)}
}

X <- function(u){
  return(u*?(1/3)+1)
}

set.seed(123)
n=1000
ran<-runif(n)
vec<-sapply(ran,X)

df<- data.frame('ID'= ran, 'X'=vec)
df_pdf<-data.frame('X'= seq(1, 2, by = 0.01), 'pdf'=sapply(seq(1, 2, by = 0.01),pdf_X))

ggplot(df,aes(x=X)) + 
  geom_histogram(data=subset(df),
       ?         aes(y=(..count../sum(..count..))), 
                 breaks= seq(0.5, 2.5, by = 0.1),
                 alpha = 0.2,
                 color='red',fill="red")+
  geom_line(data=df_pdf, aes(x = X, y=pdf),color="darkblue")+
  scale_y_continuous("Relat?ve frequency of occurrences", sec.axis = sec_axis(~ . *(1*9),name='PDF'))+
  theme_bw()+ ggtitle('Histogram of the sample along with the PDF')+
    theme(plot.title = element_text(hjust = 0.5))