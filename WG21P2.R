##Group member:
##1. Aditya Prabaswara Mardjikoen(s2264710)
##2. Xiangtian Duan(s2248742)
##3. Huiying Chen(s2264943)

## Github repositories link: 
## https://github.com/aprabaswara/Statistical-Programming-Task-2-Covid-and-the-hazards-of-opportunistic-sampling-Group-21.git


#for wholle number
n=5500000

ne=10
nt=150
gamma=1/3
delta=1/5
lamda=0.4/n
x<-rep(0,n)
beta<- rlnorm(n,0,0.5); beta <- beta/mean(beta)
x[1:ne]<-1 #exposed

S<-E<-I<-R<-rep(0,nt)

S[1]<-n-ne;E[1]<-ne

for (i in 2:nt){
  
  u<-runif(n)
  
  t<-sum(beta[which(x==2)])
  x[x==2&u<delta]<-3## I -> R with prob delta
  
  x[x==1&u<gamma]<-2 ## E -> I with prob gamma
  
  x[which(x==0)][u[which(x==0)]<t*lamda*beta[which(x==0)]]<-1
  
  
  
  #x[which(x==0)][u[which(x==0)]<sum(beta[which(x==2)])*lamda*beta[which(x==0)]]<-1
  
  S[i]<-sum(x==0);E[i]<-sum(x==1)
  I[i]<-sum(x==2);R[i]<-sum(x==3)
  
}


list (S=S,E=E,I=I,R=R,beta=beta)

plot(I,ylim=c(0,max(I)),xlab="day",ylab="N",col='red',type='l')

#for 10%

n=550000

ne=10
nt=150
gamma=1/3
delta=1/5
lamda=0.4/n
x<-rep(0,n)
beta<- rlnorm(n,0,0.5); beta <- beta/mean(beta)
beta_sort <- sort(beta,decreasing=FALSE)
threshold<-beta_sort[n]
#length(beta[which(beta==threshold)])
beta <- beta[which(beta<=threshold)]



x[1:ne]<-1 #exposed

S<-E<-I<-R<-rep(0,nt)

S[1]<-n-ne;E[1]<-ne

for (i in 2:nt){
  
  u<-runif(n)
  
  t<-sum(beta[which(x==2)])
  x[x==2&u<delta]<-3## I -> R with prob delta
  
  x[x==1&u<gamma]<-2 ## E -> I with prob gamma
  
  x[which(x==0)][u[which(x==0)]<t*lamda*beta[which(x==0)]]<-1
  
  
  
  
  S[i]<-sum(x==0);E[i]<-sum(x==1)
  I[i]<-sum(x==2);R[i]<-sum(x==3)
  
}


list (S=S,E=E,I=I,R=R,beta=beta)

plot(I,ylim=c(0,max(I)),xlab="day",ylab="N",col='red',type='l')


#for 0.1%


n<-5500000*0.001
beta<-sample(beta,n)


ne=10
nt=150
gamma=1/3
delta=1/5
lamda=0.4/n
x<-rep(0,n)


x[1:ne]<-1 #exposed

S<-E<-I<-R<-rep(0,nt)

S[1]<-n-ne;E[1]<-ne

for (i in 2:nt){
  
  u<-runif(n)
  
  t<-sum(beta[which(x==2)])
  x[x==2&u<delta]<-3## I -> R with prob delta
  
  x[x==1&u<gamma]<-2 ## E -> I with prob gamma
  
  x[which(x==0)][u[which(x==0)]<t*lamda*beta[which(x==0)]]<-1
  
  
  
  S[i]<-sum(x==0);E[i]<-sum(x==1)
  I[i]<-sum(x==2);R[i]<-sum(x==3)
  
}


list (S=S,E=E,I=I,R=R,beta=beta)

plot(I,ylim=c(0,max(I)),xlab="day",ylab="N",col='red',type='l')






#write in one function
covid_simulation <- function(n,nt){
  ## covid stochastic simulation model.
  ## n = population size; ne = initially exposed; nt = number of days
  ## gamma = daily prob E -> I; delta = daily prob I -> R;
  x <- rep(0,n) ## initialize to susceptible state
  #beta <- rlnorm(n,0,0.5) ## individual infection rates
  #beta <- beta/mean(beta)
  #beta_sort <- sort(beta,decreasing=TRUE)
  #beta_lower <- tail(beta_sort,n/10) ## find the lowest beta value
  ne=10
  lambda <- 0.4/n
  x[1:ne] <- 1 ## create some exposed
  S <- E <- I <- R <- rep(0,nt) ## set up storage for pop in each state
  S[1] <- n-ne;E[1] <- ne ## initialize
  gamma=1/3
  delta=1/5
  #`%ni%` = Negate(`%in%`)
  
  ##State: S=0; E=1; I=2; R=3
  
  for (i in 2:nt){ ## loop over days
    u<-runif(n)
    
    t<-sum(beta[which(x==2)])
    x[x==2&u<delta]<-3## I -> R with prob delta
    
    x[x==1&u<gamma]<-2 ## E -> I with prob gamma
    
    x[which(x==0)][u[which(x==0)]<t*lamda*beta[which(x==0)]]<-1
    
    
    
    S[i] <- sum(x==0); E[i] <- sum(x==1)
    I[i] <- sum(x==2); R[i] <- sum(x==3)
  }
  list(S=S,E=E,I=I,R=R,beta=beta)
} ## covid

n=5500000
nt=150
beta<- rlnorm(n,0,0.5); beta <- beta/mean(beta)
ep1<-covid_simulation (n,nt)


beta_sort<-sort(beta,decreasing = TRUE)
beta_lower <- tail(beta_sort,n*0.1)
beta<-beta[which(beta%in%beta_lower)]
ep2<-covid_simulation (n=n*0.1,nt=150)


# <- sort(beta,decreasing=FALSE)
#threshold<-beta_sort[n/10]
#length(beta[which(beta==threshold)])
#beta <- beta[which(beta<=threshold)]
#ep2<-covid_simulation (n=n*0.1,nt=150)




beta<- rlnorm(n,0,0.5); beta <- beta/mean(beta)
beta<-sample(beta,n*0.001)
ep3<-covid_simulation (n=n*0.001,nt=150)





plot(ep1$I,ylim=c(0,max(ep1$I)),xlab="day",ylab="N",col='red',type='l')
points(ep2$I,ylim=c(0,max(ep2$I)),xlab="day",ylab="N",col='green',type='l')
lines(ep3$I,ylim=c(0,max(ep3$I)),xlab="day",ylab="N",col='blue',type='l')

df <- data.frame(infections=seir()$I, days=1:100)
par(mfcol=c(2,3),mar=c(4,4,1,1)) ## set plot window up for multiple plots
#hist(epi$beta,xlab="beta",main="") ## beta distribution
plot(df$infections,ylim=c(0,max(df$infections)),xlab="day",ylab="N",type='l',col='red') 
abline(v = df$days[df$infections==max(df$infections)], col = "black", lty = 2)

#Red : Number of new infection each day
#Blue : Number of new infection among the 10% of the population with lowest beta value
#Green : Number of new infection in a random sample of 0.1% of the population.
#Black : The peak of each infection