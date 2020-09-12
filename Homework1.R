##################### STOCHASTIC SIMULATION ####################
#####################  HOMEWORK 1 ##############################
#Johannes Talero
library(ggplot2)
library(dplyr)
#2. Inverse method for discrete r.v.’s (Jones):
#  Consider the discrete random variable (r.v.) X with probability mass function (pmf ) given by:
#  P(X = −1) = 0.2, P(X = 0) = 0.5, P(X = 1) = 0.3

#a) Calculate and plot the Cumulative Distribution Function (CDF) F_X(x) of X.
df<- data.frame('X'= -2:2, 'CDF'=c(0,0.2,0.7,1,1))
ggplot(df, aes(x=X, y=CDF),color='Red')+
  geom_point()+ theme_bw()+
  ggtitle('Cumulative Distribution Function of X')+
  theme(plot.title = element_text(hjust = 0.5))

#b) Write a program to generate n values of X with this distribution, using the function runif.
inver_trans_X<-function(uni){
  if (uni<=0.2)
  {val=-1}
  else if (uni<=0.7)
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
sample_X <-data.frame('ID'= 1:n, 'X'=random_x(n))
count_case <-sample_X %>%group_by(X)%>%count(X)
count_case$relative_f <-count_case$n/n
#d) Repeat c) with n = 10000.
set.seed(123)
n<-10000
sample_X <-data.frame('ID'= 1:n, 'X'=random_x(n))
count_case <-sample_X %>%group_by(X)%>%count(X)
count_case$relative_f <-count_case$n/n


#3. Inverse method for continuous r.v.’s (Jones):
#Consider the continuous random variable X with probability density function (pdf ) given by:
#  fX(x) = ( 3(x − 1)**2  for 1 < x ≤ 2, 
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
ran<-runif(n)
vec<-sapply(ran,F_X)

df<- data.frame('X'= ran*3, 'CDF'=vec)

ggplot(df, aes(x=X, y=CDF))+
  geom_line(color="darkblue")+theme_bw()+
  ggtitle('Cumulative Distribution Function of X')+
  theme(plot.title = element_text(hjust = 0.5))


#d) Write a program in R that draws 1000 samples of X.
#Plot a normalized histogram of thesample along with the PDF o fX.

pdf_X<- function(x){
  if (x>1 && x<=2)
  {return((3*(x-1)**(2))/9)}#Change in dimension only for graph
  else{return(0)}
}

X <- function(u){
  return(u**(1/3)+1)
}

set.seed(123)
n=1000
ran<-runif(n)
vec<-sapply(ran,X)

df<- data.frame('ID'= ran, 'X'=vec)
df_pdf<-data.frame('X'= seq(1, 2, by = 0.01), 'pdf'=sapply(seq(1, 2, by = 0.01),pdf_X))

ggplot(df,aes(x=X)) + 
  geom_histogram(data=subset(df),
                 aes(y=(..count../sum(..count..))), 
                 breaks= seq(0.5, 2.5, by = 0.1),
                 alpha = 0.2,
                 color='red',fill="red")+
  geom_line(data=df_pdf, aes(x = X, y=pdf),color="darkblue")+
  scale_y_continuous("Relative frequency of occurrences", sec.axis = sec_axis(~ . *(1*9),name='PDF'))+
  theme_bw()+ ggtitle('Histogram of the sample along with the PDF')+
  theme(plot.title = element_text(hjust = 0.5))

#4. Rejection method for continuous r.v.’s (Jones):
#Let X a r.v. with pdf:fX(x) = 2xe−x2,0< x <∞(a)  
#Use the rejection method to find an efficient way to simulate X

rejection_exp <- function(f_X, c) {
  while (TRUE) {
    Y <- rexp(1)
    u <- runif(1, 0, c*exp(-Y))
    if (u <= f_X(Y)) return(Y)
  }
}

pdf_X<-function(x){2*x*exp(-x**2)}
pdf_X_graph<-function(x){2*x*exp(-x**2)*(1/9)}#same PDF change in dimension only for graph

set.seed(123)
n=10000
Observations<-rep(0,n)
for (i in 1:n)
{Observations[i]<- rejection_exp(pdf_X,2)}

df<- data.frame('ID'= 1:n, 'X'=Observations)

df_pdf<-data.frame('X'= seq(0, 5, by = 0.01), 'pdf'=sapply(seq(0, 5, by = 0.01),pdf_X_graph))

ggplot(df,aes(x=X)) + 
  geom_histogram(data=subset(df),
                 aes(y=(..count../sum(..count..))), 
                 breaks= seq(0, 5, by = 0.1),
                 alpha = 0.2,
                 color='red',fill="red")+
  geom_line(data=df_pdf, aes(x = X, y=pdf),color="darkblue")+
  scale_y_continuous("Relative frequency of occurrences", sec.axis = sec_axis(~ . *(9),name='PDF'))+
  theme_bw()+ ggtitle('Histogram of the sample along with the PDF')+
  theme(plot.title = element_text(hjust = 0.5))


#5. Monte Carlo Integration (Ross):
#with respect to the following integrals: (i) Find their exact value analytically or with software
#(e.g., WolframAlpha), (ii) with Monte Carlo integration approximate the integrals and compare
#with the exact answer.
monte_carlo_int <- function(a=0,b,c=0,d,fun,num){
  t<-0
  for (n in 1:num){
    x<-runif(1,a,b)
    y<-runif(1,c,d)
    if (y < fun(x)){t<-t+1}
    
  }
  return((b-a)*(d-c)*(t/num))
}

monte_carlo_int_imp <- function(a=0, b, fun, num) {
  u <- runif(num, a, b)
  x <- sapply(u, fun)
  return(mean(x)*(b-a))
}


monte_carlo_int_2d <- function(a_1=0,b_1,a_2=0,b_2,fun,num){
  x<-runif(num,a_1,b_1)
  y<-runif(num,a_2,b_2)
  
  z <- mapply(fun,x,y)
  return(mean(z)*(b_1-a_1)*(b_2-a_2))
}


#a. 
set.seed(123)
n=10^6
f_a<-function(x){return(exp(exp(x)))}
monte_carlo_int(a=0,b=1,c=0,d=16,fun=f_a,num=n)

#b.
set.seed(123)
f_b<-function(x){return(exp(x+(x**2)))}
monte_carlo_int(a=-2,b=2,c=0,d=405,fun=f_b,num=n)

#c.
set.seed(123)
f_c<-function(x){return((x/(1+x^2)^2))}
f_c_m<-function(y){return(f_c((1/y)-1)/(y^2))}
monte_carlo_int_imp(a=0,b=1,fun=f_c_m,num=n)

#d.
set.seed(123)
f_d<-function(x,y){return(exp((x+y)^2))}
monte_carlo_int_2d(a_1=0,b_1=1,a_2=0,b_2=2,fun=f_d,num=n)


#6. Estimating expected values with Monte Carlo (Ross):

N_fun<-function(){
  tot <- 0
  N <- 0
  while(tot <= 1)
  {tot<-runif(num,0,1)+tot
  N<- N+1 }
  return(N)
}

set.seed(123)
n<-100
E_N<-0
for (i in 1:n){
  E_N<-(N_fun()/n)+E_N
}
print(E_N)

set.seed(123)
n<-1000
E_N<-0
for (i in 1:n){
  E_N<-(N_fun()/n)+E_N
}
print(E_N)


set.seed(123)
n<-10000
E_N<-0
for (i in 1:n){
  E_N<-(N_fun()/n)+E_N
}
print(E_N)


