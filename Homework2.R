##################### STOCHASTIC SIMULATION ####################
#####################  HOMEWORK 2 ##############################
#Johannes Talero
library(ggplot2)
library(dplyr)
#Gaining confidence with confidence intervals (Jones, et al.)
#inputs
N<-100
a<- -1
b<- 1
m<- 1000
set.seed(100)
#Generate Sample 
df<- data.frame()

for (i in 1:m){ 
  X<- runif(N,a,b)
  X_Var<-mean(X)
  S<-sqrt(sum((x-X_Var)^2)/(N-1))
  lower_bound<- (mean(X)-1.96*(S/sqrt(N)) )
  Upper_bound<- (mean(X)+1.96*(S/sqrt(N)) )
  df <- rbind(df, i = c(X_Var, S,lower_bound,Upper_bound))
}
names(df) <- c("Mean X", "S", "lower_bound", "Upper_bound")

#Is the mean in the range?
df$contains_mean <- (df$lower_bound < 0)*1 * (df$Upper_bound > 0)*1
sum(df$contains_mean)/m

#2. Stopping generating new simulation data
n<-99
d<-0.1
S=1
while ((S/sqrt(n))>=d){
  n<-n+1
  set.seed(4)
  x<-rnorm(n,0,1)
  X_Var<-mean(x)
  S<-sqrt(sum((x-X_Var)^2)/(n-1))
  }
print(n)
print(X_Var)
print(S)
print(1-S)

#3. Bootstrap (Robert and Casella):
N<- 1000
set.seed(4)

#The sample 
y=c(4.313, 4.513, 5.489, 4.265, 3.641, 5.106, 8.006, 5.087)

#--- 1 Bootstrap ---
ystar=sample(y,replace=T)
ystar
mean(ystar)


#--- N Bootstraps ---
ystar <- matrix(sample(y,N*8,replace=T),nrow=N,ncol=8)

#--- Evaluate the mean for each bootstrap sample ---
meanystar <- apply(ystar,1,mean)

hist(meanystar, freq = FALSE, xlab="Bootstrap Means",ylab="Relative Frequency")

q_95<-sort(meanystar)[0.95*N]

#Construct a bootstrap experiment that provides a 95% confidence interval on
#ˆq.95(y).

#External bootstrap
N_external<- 1000 
N_internal<- 1000
ystar_External <- matrix(sample(y,N_external*8,replace=T),nrow=N_external,ncol=8)
q_95_vec<-array(0,dim=c(N_external,1))
for (i in 1:N_external){
  #Internal  bootstrap
  ystar_internal <- matrix(sample(ystar_External[i,],N_internal*8,replace=T),nrow=N_internal,ncol=8)
  meany_internal <- apply(ystar_internal,1,mean)
  q_95_vec[i]<-sort(meany_internal)[0.95*N_internal]
}

hist(q_95_vec, freq = FALSE, xlab="Bootstrap q95",ylab="Relative Frequency")
quantile(q_95_vec,c(.05,.95))

# Bets
set.seed(10)
coin<-c('h','t')

profit <- function(){ 
  tossing<-sample(coin,2,replace=T) 
  if (all(tossing == c('h','h'))){
    X<-5000} else if (all(tossing==c('t','h')) || all(tossing==c('h','t'))){
    X<- -5000 } else {X<- -10000}
  return(X)
}

N<-10**5
profit_sim<-array(0,dim=c(N,1))
for (i in 0:N){profit_sim[i]<-profit()}

S<-sqrt(sum((profit_sim-mean(profit_sim))^2)/(N-1))
S/sqrt(N)

E_X_Real<-5000*0.25-5000*0.5-10000*0.25
abs(E_X_Real-mean(profit_sim))

count <-data.frame(profit_sim) %>%group_by(profit_sim)%>%count(profit_sim)
count$n<-count$n/N
