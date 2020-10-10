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
  set.seed(10)
  x<-rnorm(n,0,1)
  X_Var<-mean(x)
  S<-sqrt(sum((x-X_Var)^2)/(n-1))
  }
print(n)
print(X_Var)
print(S)
print(abs(1-S))
print(abs(0-X_Var))


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

df<- data.frame('ID'= seq(1, 1000, by = 1), 'X'=meanystar)
#-----------------------------------
ggplot(df,aes(x=X)) + 
  geom_histogram(data=subset(df),
                 aes(y=(..count../sum(..count..))),
                 breaks= seq(4, 6.5, by = 0.25),
                 alpha = 0.2,
                 color='red',fill="red")+
  theme_bw()+ ggtitle('Histogram of Bootstrap Means')+
  scale_y_continuous("Relative Frequency")
  theme(plot.title = element_text(hjust = 0.5))
  
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
quantile(q_95_vec,c(.025,.975))

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

#Calculate Values
mean(profit_sim)
S<-sqrt(sum((profit_sim-mean(profit_sim))^2)/(N-1))
S/sqrt(N)

E_X_Real<-5000*0.25-5000*0.5-10000*0.25
abs(E_X_Real-mean(profit_sim))
#Estimates the probability mass function p
count <-data.frame(profit_sim) %>%group_by(profit_sim)%>%count(profit_sim)
count$n<-count$n/N



#5. Reliability of a system:

p1<-0.9
p2<-0.5
p3<-0.2
p4<-0.1

struct<-function(x1,x2,x3,x4){
    if ((x1+x2+x3+x4)>=3){
      return(1)
    }else{return(0)}}
  
simulation_Realiability <- function(N){
  worked<-0
  sim_X<-runif(4, 0, 1)
  X<-sim_X<c(p1,p2,p3,p4)
  worked<- worked+struct(X[1],X[2],X[3],X[4])
  return(worked)
}
set.seed(10)
df<-data.frame('Num'= seq(2, 10^4, by = 01))

df$simulation_Realiability<-sapply(df$Num, simulation_Realiability)
df$Realiability_sum<-cumsum(df$simulation_Realiability)
df$mean<-df$Realiability_sum/df$Num
df$s<-sqrt((((1-df$mean)^2*df$Realiability_sum)+(df$mean)^2 *(df$Num-df$Realiability_sum))/(df$Num-1))
df$sdt_R<-df$s/sqrt(df$Num)
df$lower_bound<- (df$mean-1.96*(df$sdt_R) )
df$Upper_bound<- (df$mean+1.96*(df$sdt_R) )

ggplot(df, aes(x=Num)) + 
  geom_line(aes(y = lower_bound), color = "orange2") + 
  geom_line(aes(y = Upper_bound), color="orange2")+
  geom_line(aes(y = mean), color="black", linetype="twodash")+
  scale_y_continuous("Realiability")+
  theme_bw()+ ggtitle('Reliability estimation and its 95% confidence interval')+
  theme(plot.title = element_text(hjust = 0.5))
  
tail(df)

#6. Markov Chains (Liliana Blanco, ”Probabilidad”, 2a Edici ́on)
prop_p<-0.7
prop_q<-1-prop_p
P<-matrix(0,6,6)
for (i in 1:6){P[i,1+(i%%6)]<-prop_p
               P[i,((i-1)%%6)]<-prop_q }
P[1,6]<-prop_q
e<-c(1,1,1,1,1,1)
I<-diag(e)
E<-matrix(1,nrow=6,ncol=6)
solve(I+E-t(P),e)


move<- function(x){
  x_n<-x-1
  prob<-runif(1,0,1)
  if (prob<prop_p)
    {x_m<-(x_n+1)%%6
  }else{x_m<-(x_n-1)%%6}
  return(x_m+1)
}

movements <-function(x,N)
{df<-data.frame('Num'= seq(0, N, by = 01))
  df$state<-0
  df$state[1]<-x
  for (i in 1:N){df$state[i+1]<-move(df$state[i])}
  return(df)
}
set.seed(123)
df_m<-movements(1,200)

ggplot(df_m, aes(x=Num)) + 
  geom_line(aes(y = state), color = "grey", linetype = "dashed" )+
  geom_point(aes(y = state), color = "darkred")+ylim(0, 7)+
  theme_bw()+ ggtitle('DTMC simulation')+
  theme(plot.title = element_text(hjust = 0.5))

longrun.prob <- function(df){
  count <- df %>%group_by(state)%>%count(state)
  count$n<-count$n/length(df$Num)
  return(count)}

set.seed(123)
df_m_1<-movements(1,200)
longrun.prob(df_m)

set.seed(123)
df_m_2<-movements(1,10000)
longrun.prob(df_m_2)

