##Group member:
##1. Aditya Prabaswara Mardjikoen(s2264710)
##2. Xiangtian Duan(s2248742)
##3. Huiying Chen(s2264943)

## Github repositories link: 
## https://github.com/aprabaswara/Statistical-Programming-Task-2-Covid-and-the-hazards-of-opportunistic-sampling-Group-21.git
## Github repositories invitation link:
## https://github.com/aprabaswara/Statistical-Programming-Task-2-Covid-and-the-hazards-of-opportunistic-sampling-Group-21/invitations


##IDEA 1
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

n=5500000/10

ne=10
nt=150
gamma=1/3
delta=1/5
lamda=0.4/550000
x<-rep(0,550000)
beta<- rlnorm(5500000,0,0.5); beta <- beta/mean(beta)
beta_sort <- sort(beta,decreasing=FALSE)
threshold<-beta_sort[550000]
#length(beta[which(beta==threshold)])
beta <- beta[which(beta<=threshold)]



x[1:ne]<-1 #exposed

S<-E<-I<-R<-rep(0,nt)

S[1]<-n-ne;E[1]<-ne

for (i in 2:nt){
  
  u<-runif(550000)
  
  t<-sum(beta[which(x==2)])
  x[x==2&u<delta]<-3## I -> R with prob delta
  
  x[x==1&u<gamma]<-2 ## E -> I with prob gamma
  
  x[which(x==0)][u[which(x==0)]<t*lamda*beta[which(x==0)]]<-1
  
  
  
  
  S[i]<-sum(x==0);E[i]<-sum(x==1)
  I[i]<-sum(x==2);R[i]<-sum(x==3)
  
}


list (S=S,E=E,I=I,R=R,beta=beta)

plot(I,ylim=c(0,max(I)),xlab="day",ylab="N",col='red',type='l')

max(I)
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

  
  ne=10

  gamma=1/3
  delta=1/5
  lambda=0.4/n
  
  x<-rep(0,n)

  
  
  
  x[1:ne]<-1 #exposed
  
  S<-E<-I<-R<-rep(0,nt)
  
  S[1]<-n-ne;E[1]<-ne
  
  for (i in 2:nt){
    
    u<-runif(n)
    
    t<-sum(beta[which(x==2)])
    
    x[x==2&u<delta]<-3## I -> R with prob delta
    
    x[x==1&u<gamma]<-2 ## E -> I with prob gamma
    
    x[which(x==0)][u[which(x==0)]<t*lambda*beta[which(x==0)]]<-1
    
    
    
    
    S[i]<-sum(x==0);E[i]<-sum(x==1)
    I[i]<-sum(x==2);R[i]<-sum(x==3)
    
  }
  
  
  list (S=S,E=E,I=I,R=R,beta=beta)


  
  
} ## covid

n=5500000
nt=150
beta<- rlnorm(n,0,0.5); beta <- beta/mean(beta)
ep1<-covid_simulation(n,nt)


beta_sort<-sort(beta,decreasing = TRUE)
beta_lower <- tail(beta_sort,n/10)
beta<-beta[which(beta%in%beta_lower)]
ep2<-covid_simulation (n/10,nt)



#beta_sort <- sort(beta,decreasing=FALSE)
#n<-n/10
#threshold<-beta_sort[n]
##length(beta[which(beta==threshold)])
#beta <- beta[which(beta<=threshold)]
#ep2<-covid_simulation (550000,150)



beta<- rlnorm(n,0,0.5); beta <- beta/mean(beta)

beta<-sample(beta,n*0.001)
ep3<-covid_simulation (n=n*0.001,nt)





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



#IDEA 2


#for wholle number
n=5500000

ne=10
nt=150
gamma=1/3
delta=1/5
lamda=0.4/5500000
x<-rep(0,5500000)
beta<- rlnorm(5500000,0,0.5); beta <- beta/mean(beta)



x[1:ne]<-1 #exposed

S<-E<-I<-R<-rep(0,nt)

S[1]<-5500000-ne;E[1]<-ne


#length(beta[which(beta==threshold)])
#x <- x[which(beta<=threshold)]

for (i in 2:nt){
  
  u<-runif(5500000)
  
  t<-sum(beta[which(x==2)])
  x[x==2&u<delta]<-3## I -> R with prob delta
  
  x[x==1&u<gamma]<-2 ## E -> I with prob gamma
  
  x[which(x==0)][u[which(x==0)]<t*lamda*beta[which(x==0)]]<-1
  
  
  
  
  
  #x[which(x==0)][u[which(x==0)]<sum(beta[which(x==2)])*lamda*beta[which(x==0)]]<-1
  
  S[i]<-sum(x==0);E[i]<-sum(x==1)
  I[i]<-sum(x==2);R[i]<-sum(x==3)
  
  #S[i]<-sum(x==0);E[i]<-sum(x==1);I[i]<-sum(x==2);R[i]<-sum(x==3)
  
  
}


list (S=S,E=E,I=I,R=R,beta=beta)

plot(I,ylim=c(0,max(I)),xlab="day",ylab="N",col='red',type='l')















#for 10%




n=5500000

ne=10
nt=150
gamma=1/3
delta=1/5
lamda=0.4/5500000
x<-rep(0,5500000)
beta<- rlnorm(5500000,0,0.5); beta <- beta/mean(beta)
beta_sort <- sort(beta,decreasing=FALSE)

threshold<-beta_sort[550000]


x[1:ne]<-1 #exposed

S<-E<-I<-R<-rep(0,nt)

S[1]<-550000-ne;E[1]<-ne


for (i in 2:nt){
  
  u<-runif(5500000)
  
  t<-sum(beta[which(x==2)])
  x[x==2&u<delta]<-3## I -> R with prob delta
  
  x[x==1&u<gamma]<-2 ## E -> I with prob gamma
  
  x[which(x==0)][u[which(x==0)]<t*lamda*beta[which(x==0)]]<-1
  
  y <- x[which(beta<=threshold)]
  
  
  
  #x[which(x==0)][u[which(x==0)]<sum(beta[which(x==2)])*lamda*beta[which(x==0)]]<-1
  
  S[i]<-sum(y==0);E[i]<-sum(y==1)
  I[i]<-sum(y==2);R[i]<-sum(y==3)
  
  #S[i]<-sum(x==0);E[i]<-sum(x==1);I[i]<-sum(x==2);R[i]<-sum(x==3)
  
  
}


list (S=S,E=E,I=I,R=R,beta=beta)

plot(I,ylim=c(0,max(I)),xlab="day",ylab="N",col='red',type='l')




#for 0.1%



n=5500000

n<-5500000*0.001
beta<- rlnorm(5500000,0,0.5); beta <- beta/mean(beta)
sample_beta<-sample(beta,n)


m<-which(beta%in%sample_beta)
ne=10
nt=150
gamma=1/3
delta=1/5
lamda=0.4/5500000
x<-rep(0,5500000)

x[1:ne]<-1 #exposed

S<-E<-I<-R<-rep(0,nt)

S[1]<-550000-ne;E[1]<-ne


for (i in 2:nt){
  
  u<-runif(5500000)
  
  t<-sum(beta[which(x==2)])
  x[x==2&u<delta]<-3## I -> R with prob delta
  
  x[x==1&u<gamma]<-2 ## E -> I with prob gamma
  
  x[which(x==0)][u[which(x==0)]<t*lamda*beta[which(x==0)]]<-1
  
  z <- x[m]
  
  
  
  
  
  S[i]<-sum(z==0);E[i]<-sum(z==1)
  I[i]<-sum(z==2);R[i]<-sum(z==3)
  
 
  
  
}


list (S=S,E=E,I=I,R=R,beta=beta)

plot(I,ylim=c(0,max(I)),xlab="day",ylab="N",col='red',type='l')





#write in one function(wrong)

covid_simulation<- function(n=5500000,nt=150,choice){
  
  ne=10
  gamma=1/3
  delta=1/5
  lamda=0.4/n
  x<-rep(0,n)
  beta<- rlnorm(n,0,0.5); beta <- beta/mean(beta)
  
  
  #beta_sort<-sort(beta,decreasing = TRUE)

  beta_lower <- tail(beta_sort,n*0.1)
  beta_lower_index<-which(beta%in%beta_lower)

  
  sample_beta<-sample(beta,n*0.001)
  sample_index<-which(beta%in%sample_beta)
  
  
  x[1:ne]<-1 #exposed
  
  S<-E<-I<-R<-rep(0,nt)
  #A<-W<-U<-P<-rep(0,nt)
  #D<-Q<-G<-K<-rep(0,nt)
  
  S[1]<-n-ne;E[1]<-ne
  #A[1]<-n-ne;W[1]<-ne
  #D[1]<-n-ne;Q[1]<-ne
  
  
  
  for (i in 2:nt){
    
    u<-runif(n)
    
    sum_beta_I<-sum(beta[which(x==2)])
    
    x[x==2&u<delta]<-3## I -> R with prob delta
    
    x[x==1&u<gamma]<-2 ## E -> I with prob gamma
    
    x[which(x==0)][u[which(x==0)]<sum_beta_I*lamda*beta[which(x==0)]]<-1
    
    y <- x[beta_lower_index]
    
    z <- x[sample_index]
    
    
    #x[which(x==0)][u[which(x==0)]<sum(beta[which(x==2)])*lamda*beta[which(x==0)]]<-1
    
    S[i]<-sum(x==0);E[i]<-sum(x==1);I[i]<-sum(x==2);R[i]<-sum(x==3)
    
    #A[i]<-sum(y==0);W[i]<-sum(y==1);U[i]<-sum(y==2);P[i]<-sum(y==3)
    
    #D[i]<-sum(z==0);Q[i]<-sum(z==1);G[i]<-sum(z==2);K[i]<-sum(z==3)
    
    
    
    
  }
  
  
  list (S=S,E=E,I=I,R=R,beta=beta)
  #list (A=A,W=W,U=U,P=P,beta=beta)
  #list (D=D,Q=Q,G=G,K=K,beta=beta)
  
  
}

epi<-covid_simulation(n=5500000,nt=150)
plot(epi$I,ylim=c(0,max(epi$I)),xlab="day",ylab="N",col='red',type='l')
points(epi$i,ylim=c(0,max(epi$i)),xlab="day",ylab="N",col='green',type='l')
lines(epi$II,ylim=c(0,max(epi$II)),xlab="day",ylab="N",col='blue',type='l')


#write in one function(newest but longer sunning time)

covid_simulation<- function(n,nt,choice){
  
  ne=10
  gamma=1/3
  delta=1/5
  lamda=0.4/n
  x<-rep(0,n)
  
  
  
  #beta_sort<-sort(beta,decreasing = TRUE)
  
  #beta_lower <- tail(beta_sort,n*0.1)
  index<-which(beta%in%choice)
  
  
  #sample_beta<-sample(beta,n*0.001)
  #sample_index<-which(beta%in%sample_beta)
  
  
  x[1:ne]<-1 #exposed
  
  S<-E<-I<-R<-rep(0,nt)
  #A<-W<-U<-P<-rep(0,nt)
  #D<-Q<-G<-K<-rep(0,nt)
  
  S[1]<-n-ne;E[1]<-ne
  #A[1]<-n-ne;W[1]<-ne
  #D[1]<-n-ne;Q[1]<-ne
  
  
  
  for (i in 2:nt){
    
    u<-runif(n)
    
    sum_beta_I<-sum(beta[which(x==2)])
    
    x[x==2&u<delta]<-3## I -> R with prob delta
    
    x[x==1&u<gamma]<-2 ## E -> I with prob gamma
    
    x[which(x==0)][u[which(x==0)]<sum_beta_I*lamda*beta[which(x==0)]]<-1
    
    y <- x[index]
    
    
    
    
    #x[which(x==0)][u[which(x==0)]<sum(beta[which(x==2)])*lamda*beta[which(x==0)]]<-1
    
    S[i]<-sum(y==0);E[i]<-sum(y==1);I[i]<-sum(y==2);R[i]<-sum(y==3)
    
    #A[i]<-sum(y==0);W[i]<-sum(y==1);U[i]<-sum(y==2);P[i]<-sum(y==3)
    
    #D[i]<-sum(z==0);Q[i]<-sum(z==1);G[i]<-sum(z==2);K[i]<-sum(z==3)
    
    
    
    
  }
  
  
  list (S=S,E=E,I=I,R=R,beta=beta)
  #list (A=A,W=W,U=U,P=P,beta=beta)
  #list (D=D,Q=Q,G=G,K=K,beta=beta)
  
  
}
n<-5500000
nt<-150
beta<- rlnorm(n,0,0.5); beta <- beta/mean(beta)
epi<-covid_simulation(n,nt,choice=beta)
#plot(epi$I,ylim=c(0,max(epi$I)),xlab="day",ylab="N",col='red',type='l')


beta_sort<-sort(beta,decreasing = TRUE)
beta_lower<- tail(beta_sort,n*0.1)
eplower<-covid_simulation(n,nt,choice=beta_lower)
#points(eplower$I,ylim=c(0,max(eplower$I)),xlab="day",ylab="N",col='green',type='l')

sample_beta<-sample(beta,n*0.001)
epsample<-covid_simulation(n,nt,choice=sample_beta)
#lines(epsample$I,ylim=c(0,max(epsample$I)),xlab="day",ylab="N",col='blue',type='l')

#Standardisation
plot(epi$I/n,ylim=c(0,max(epi$I)/n),xlab="day",ylab="N",col='red',type='l')
points(eplower$I/(0.1*n),ylim=c(0,max(eplower$I)/(0.1*n)),xlab="day",ylab="N",col='green',type='l')
lines(epsample$I/(0.001*n),ylim=c(0,max(epsample$I)/(0.001*n)),xlab="day",ylab="N",col='blue',type='l')


par(mfcol=c(5,2),mar=c(4,4,1,1))
for (i in 1:10){
  beta<- rlnorm(n,0,0.5); beta <- beta/mean(beta)
  epi<-covid_simulation(n,nt,choice=beta)
 
  beta_sort<-sort(beta,decreasing = TRUE)
  beta_lower<- tail(beta_sort,n*0.1)
  eplower<-covid_simulation(n,nt,choice=beta_lower)
  
  sample_beta<-sample(beta,n*0.001)
  epsample<-covid_simulation(n,nt,choice=sample_beta)
  
  plot(epi$I/n,ylim=c(0,max(epi$I)/n),xlab="day",ylab="N",col='red',type='l')
  points(eplower$I/(0.1*n),ylim=c(0,max(eplower$I)/(0.1*n)),xlab="day",ylab="N",col='green',type='l')
  lines(epsample$I/(0.001*n),ylim=c(0,max(epsample$I)/(0.001*n)),xlab="day",ylab="N",col='blue',type='l')
  
}



#df <- data.frame(infections=seir()$I, days=1:100)
#par(mfcol=c(2,3),mar=c(4,4,1,1)) ## set plot window up for multiple plots
#hist(epi$beta,xlab="beta",main="") ## beta distribution
plot(df$infections,ylim=c(0,max(df$infections)),xlab="day",ylab="N",type='l',col='red') 
abline(v = df$days[df$infections==max(df$infections)], col = "black", lty = 2)



#writing in one function(less time)

######check the running time start
covid_simulation<- function(n,nt,beta){
  
  ne=10
  gamma=1/3
  delta=1/5
  lamda=0.4/n
  x<-rep(0,n)
  
  
  
  
  
  beta_sort<-sort(beta,decreasing = TRUE)
  beta_lower <- tail(beta_sort,n*0.1)
  lower_index<-which(beta%in%beta_lower)
  
  
  sample_beta<-sample(beta,n*0.001)
  sample_index<-which(beta%in%sample_beta)
  
  
  x[1:ne]<-1 #exposed
  
  #S<-E<-I<-R<-rep(0,nt);A<-W<-U<-P<-rep(0,nt); D<-Q<-G<-K<-rep(0,nt)
  I<-U<-G<-rep(0,nt)
  
  # S[1]<-n-ne;E[1]<-ne;A[1]<-n-ne;W[1]<-ne;D[1]<-n-ne;Q[1]<-ne
  
  
  
  for (i in 2:nt){
    
    u<-runif(n)
    
    sum_beta_I<-sum(beta[which(x==2)])
    
    x[x==2&u<delta]<-3## I -> R with prob delta
    
    x[x==1&u<gamma]<-2 ## E -> I with prob gamma
    
    x[which(x==0)][u[which(x==0)]<sum_beta_I*lamda*beta[which(x==0)]]<-1
    
    y <- x[lower_index]
    z <- x[sample_index]
    
    
    
    
    #x[which(x==0)][u[which(x==0)]<sum(beta[which(x==2)])*lamda*beta[which(x==0)]]<-1
    
    #S[i]<-sum(x==0);E[i]<-sum(x==1);
    I[i]<-sum(x==2)
    #;R[i]<-sum(x==3)
    
    #A[i]<-sum(y==0);W[i]<-sum(y==1);
    U[i]<-sum(y==2)
    #;P[i]<-sum(y==3)
    
    #D[i]<-sum(z==0);Q[i]<-sum(z==1);
    G[i]<-sum(z==2)
    #;K[i]<-sum(z==3)
    
    
    
    
  }
  
  
  #list (S=S,E=E,I=I,R=R,beta=beta);list (A=A,W=W,U=U,P=P,beta=beta);list (D=D,Q=Q,G=G,K=K,beta=beta)
  list (I=I,U=U,G=G,beta=beta)
  plot(I/n,ylim=c(0,max(I)/n),xlab="day",ylab="N",col='red',type='l')
  points(U/(0.1*n),ylim=c(0,max(U)/(0.1*n)),xlab="day",ylab="N",col='green',type='l')
  lines(G/(0.001*n),ylim=c(0,max(G)/(0.001*n)),xlab="day",ylab="N",col='blue',type='l')
  
  ##Add legend and threshold line at the peak
  df<-data.frame(I=I/n,I_lower=U/(0.1*n),I_sample=G/(0.001*n),days=1:nt)
  abline(v = df$days[df$I==max(df$I)], col = "red", lty = 3)
  abline(v = df$days[df$I_lower==max(df$I_lower)], col = "green", lty = 3)
  abline(v = df$days[df$I_sample==max(df$I_sample)], col = "blue", lty = 3)
  legend(1, 0.12, legend=c("whole", "0.1%", "10%"),col=c("red", "blue","green"), lty=1, cex=0.5, text.font=3,text.col='black',box.lwd = 0,box.co='white')
  
  ##Display horizontal line
  ##abline(h = max(df$I), col = "red", lty = 3)
  ##abline(h = max(df$I_lower), col = "green", lty = 3)
  ##abline(h = max(df$I_sample), col = "blue", lty = 3)
}
n<-5500000
nt<-150
beta<- rlnorm(n,0,0.5); beta <- beta/mean(beta)
covid_simulation(n,nt,beta)
######end check the running time
par(mfcol=c(4,3),mar=c(4,4,1,1))
for (i in 1:10){
  beta<- rlnorm(n,0,0.5); beta <- beta/mean(beta)
  covid_simulation(n,nt,beta) 
}



#correction about newly infected

##Overview:
## ********
##The aim of this project is to answer the following question:
##What do the daily infection trajectories look like for the 10% (say) of the 
##population with the lowest individual transmission probabilities, and how do 
##they compare to the infection trajectories for the whole population?

##******************************************************************************
  
##Model Assumption:
##*****************
##1. The ith person in the whole population has a relative contact rate with
##   other people of beta[i], so that the daily chance of a transmission
##   between infected person i and uninfected person j is beta[i]*beta[j]*lambda,
##   where lambda is an overall viral infectivity parameter. 
##
##2. The average value of beta over the whole population is 1. 
##
##3. Each day an uninfected person, j, has a probability 
##   lambda*beta[j]*sigma(beta[i] for all i in I) of being infected, 
##   and entering the exposed (E) state. I is the set of all currently infected 
##   people. Once exposed, they have a daily probability of 1/3 of entering the 
##   infectious state. 
##
##4. The infectious have a daily probability of 1/5 of leaving the infectious 
##   state (recovery or transition to serious disease). 
##
##5. Over the course of the simulation we do not want to consider re-infection.
##******************************************************************************
##
##Simulation description:
##***********************
##In this code, we simulate the model for 150 days and also we will record the 
##number of new infections each day, and the number of new infections among the 
##10% of the population with the lowest beta[i] values. Also,we will record the 
##number of new infection in a random sample of 0.1% of the population. All of
##the result will be presented in a line graph.
##******************************************************************************
##
##Algorithm:
##1. Set the number of Scotland population size (n), which is 5.5 million.
##
##2. Generate beta using random generation function for log normal distribution. 
##   In R studio, this function is called rlnorm. For this simulation we set the 
##   mean = 0 and standard deviation = 0.5.
##
##3. Standarized the value of all beta by dividing it with its mean.
##
##4. Calculate the overall infectivity parameter (lambda) using this formula:
##   lambda=0.4/n.
##
##5. Set 10 people randomly into the exposed state (E).
##
##6. Find 550 thousand people who have lowest beta value.
##
##7. Select 5.5 thousand people randomly.
##
##8. Calculate the new infections each day for the whole population, 10% of the 
##   population that has lowest beta value, and 0.1% of the people selected 
##   randomly. We can stop counting the new infections if we reach 150 days
##   because we only observed it for 150 days.
##
##9. Plot the result from step 8 in one plot that contain all graph.
##
##10.Run all simulations from step 1 until step 9 for 10 times and plot them in
##   a subplot that contains 10 graph.
##******************************************************************************
##
##States:
##**************
##S=suspectible; E=exposed (infected); I=infectious ; R=recovered
##Over the simulation we use this number to classify each states
##S=0; E=1; I=2; R=3
##******************************************************************************


covid_simulation<- function(pop_size,sim_days,beta_value){
  ##function to plot the new infection each days for the three observed 
  ##population.
  ##pop_size = population size; sim_days = observed days during simulation;
  ##beta_value = the contact rate for this simulation (beta).
  
  
  ##initialize simulation data
  first_exposed=10 ##initial number of exposed people
  gamma=1/3 ##daily probability E -> I
  delta=1/5 ##daily probability I -> R
  lamda=0.4/n ##overall infectivity parameters
  x<-rep(0,n)## initialize to susceptible state
  
  
  ##Search 10% people with lowest beta value
  beta_sort<-sort(beta_value,decreasing = TRUE)##sort value from big to small
  beta_lower <- tail(beta_sort,n*0.1)##take 10% people with lowest beta value
  lower_index<-which(beta_value %in% beta_lower)
  
  
  ##Random sample 0.1% people from the whole population
  sample_beta<-sample(beta_value,n*0.001)##take 0.1% random sample from the beta
  sample_index<-which(beta_value %in% sample_beta)
  
  ##Notation: 
  ##x = whole population; y = 10% of population; z = 0.1% of population
  
  x[1:first_exposed]<-1 #set 10 people to exposed state
  y <- x[lower_index]
  z <- x[sample_index]
  
  #S<-E<-I<-R<-rep(0,nt);A<-W<-U<-P<-rep(0,nt); D<-Q<-G<-K<-rep(0,nt)
  I<-U<-G<-rep(0,sim_days)## set up storage for pop in each new infected each day
  
  ## Details: 
  ##I=new infected each day in whole population;
  ##U=new infected each day in 10% of population with lowest beta value;
  ##G=new infected each day in 0.1% of population;
  
  # S[1]<-n-ne;E[1]<-ne;A[1]<-n-ne;W[1]<-ne;D[1]<-n-ne;Q[1]<-ne
  
  
  ##simulation for 150 days
  for (i in 2:sim_days){
    
    u<-runif(pop_size)##generate random deviates based on the population size
    
    ##total people in S state from each three population
    sum_x<-sum(x==0)
    sum_y<-sum(y==0)
    sum_z<-sum(z==0)
    
    sum_beta_I<-sum(beta_value[which(x==2)])##calculate the sum of beta in I state
    
    x[x==2&u<delta]<-3 ## I -> R with probability delta
    
    x[x==1&u<gamma]<-2 ## E -> I with probability gamma
    
    
    
    x[which(x==0)][u[which(x==0)]<sum_beta_I*lamda*beta_value[which(x==0)]]<-1
    ## S -> E with probability beta[j]*lambda*sum(beta[i] for all i in I)
    y <- x[lower_index]
    z <- x[sample_index]
    
    
    newly_infected_x<-sum_x-sum(x==0)## new infected each day for whole population
    I[i]<-newly_infected_x
    
    newly_infected_y<-sum_y-sum(y==0)## new infected each day for 10% population
    U[i]<-newly_infected_y
    
    newly_infected_z<-sum_z-sum(z==0)## new infected each day for 0.1% population
    G[i]<-newly_infected_z
    
    
    
    #x[which(x==0)][u[which(x==0)]<sum(beta[which(x==2)])*lamda*beta[which(x==0)]]<-1
    
    #S[i]<-sum(x==0);E[i]<-sum(x==1);
    #I[i]<-sum(x==2)-sum_x
    #;R[i]<-sum(x==3)
    
    #A[i]<-sum(y==0);W[i]<-sum(y==1);
    #U[i]<-sum(y==2)-sum_y
    #;P[i]<-sum(y==3)
    
    #D[i]<-sum(z==0);Q[i]<-sum(z==1);
    # G[i]<-sum(z==2)-sum_z
    #;K[i]<-sum(z==3)
    
    
    
    
  }
  
  
  #list (S=S,E=E,I=I,R=R,beta=beta);list (A=A,W=W,U=U,P=P,beta=beta);list (D=D,Q=Q,G=G,K=K,beta=beta)
  list (I=I,U=U,G=G,beta=beta)
  
  ##standarize and plot each new infected
  plot(I/n,ylim=c(0,max(I/n,U/(0.1*n),G/(0.001*n))),xlab="day",ylab="N",col='red',type='l')
  points(U/(0.1*n),ylim=c(0,max(U/(0.1*n))),xlab="day",ylab="N",col='green',type='l')
  lines(G/(0.001*n),ylim=c(0,max(G/(0.001*n))),xlab="day",ylab="N",col='blue',type='l')
  
  ###plot(I,ylim=c(0,max(I)),xlab="day",ylab="N",col='red',type='l')
  ###points(U,ylim=c(0,max(U)),xlab="day",ylab="N",col='green',type='l')
  ###lines(G,ylim=c(0,max(G)),xlab="day",ylab="N",col='blue',type='l')
  
  ##Add legend and threshold line at the peak
  df<-data.frame(I=I/n,I_lower=U/(0.1*n),I_sample=G/(0.001*n),days=1:nt)
  abline(v = df$days[df$I==max(df$I)], col = "red", lty = 3)
  abline(v = df$days[df$I_lower==max(df$I_lower)], col = "green", lty = 3)
  abline(v = df$days[df$I_sample==max(df$I_sample)], col = "blue", lty = 3)
  legend(1, 0.015, legend=c("whole", "0.1%", "10%"),col=c("red", "blue","green"), lty=1, cex=0.5, text.font=3,text.col='black',box.lwd = 0,box.co='white')
  
  ##Display horizontal line
  ##abline(h = max(df$I), col = "red", lty = 3)
  ##abline(h = max(df$I_lower), col = "green", lty = 3)
  ##abline(h = max(df$I_sample), col = "blue", lty = 3)
}

##initialize data
n<-5500000 ##Scotland population
nt<-150 ##number of observed days
beta<- rlnorm(n,0,0.5); beta <- beta/mean(beta) ##beta value
covid_simulation(n,nt,beta)##initialize simulation

##Running 10 replicate simulations

par(mfcol=c(4,3),mar=c(4,4,1,1))
##R build in function to set graphical parameters
##mar = a numerical vector of the form c(bottom, left, top, right) to modify the 
##graph margin where its element indicates the margin size;
##mfcol = a vector in the form c(nr,nc) where nr = number of row and 
##nc = number of column. 
## *************************************************************************

for (i in 1:10){
  ##function to running 10 replicate simulations
  beta<- rlnorm(n,0,0.5); beta <- beta/mean(beta)
  covid_simulation(n,nt,beta) ##initialize simulation
}





#Maybe the final version

##Overview:
## ********
##The aim of this project is to answer the following question:
##What do the daily infection trajectories look like for the 10% (say) of the 
##population with the lowest individual transmission probabilities, and how do 
##they compare to the infection trajectories for the whole population?

##******************************************************************************

##Model Assumption:
##*****************
##1. The ith person in the whole population has a relative contact rate with
##   other people of beta[i], so that the daily chance of a transmission
##   between infected person i and uninfected person j is beta[i]*beta[j]*lambda,
##   where lambda is an overall viral infectivity parameter. 
##
##2. The average value of beta over the whole population is 1. 
##
##3. Each day an uninfected person, j, has a probability 
##   lambda*beta[j]*sigma(beta[i] for all i in I) of being infected, 
##   and entering the exposed (E) state. I is the set of all currently infected 
##   people. Once exposed, they have a daily probability of 1/3 of entering the 
##   infectious state. 
##
##4. The infectious have a daily probability of 1/5 of leaving the infectious 
##   state (recovery or transition to serious disease). 
##
##5. Over the course of the simulation we do not want to consider re-infection.
##******************************************************************************
##
##Simulation description:
##***********************
##In this code, we simulate the model for 150 days and also we will record the 
##number of new infections each day, and the number of new infections among the 
##10% of the population with the lowest beta[i] values. Also,we will record the 
##number of new infection in a random sample of 0.1% of the population. All of
##the result will be presented in a line graph.
##******************************************************************************
##
##Algorithm:
##1. Set the number of Scotland population size (n), which is 5.5 million.
##
##2. Generate beta using random generation function for log normal distribution. 
##   In R studio, this function is called rlnorm. For this simulation we set the 
##   mean = 0 and standard deviation = 0.5.
##
##3. Standarized the value of all beta by dividing it with its mean.
##
##4. Calculate the overall infectivity parameter (lambda) using this formula:
##   lambda=0.4/n.
##
##5. Set 10 people randomly into the exposed state (E).
##
##6. Find 550 thousand people who have lowest beta value.
##
##7. Select 5.5 thousand people randomly.
##
##8. Calculate the new infections each day for the whole population, 10% of the 
##   population that has lowest beta value, and 0.1% of the people selected 
##   randomly. We can stop counting the new infections if we reach 150 days
##   because we only observed it for 150 days.
##
##9. Plot the result from step 8 in one plot that contain all graph.
##
##10.Run all simulations from step 1 until step 9 for 10 times and plot them in
##   a subplot that contains 10 graph.
##******************************************************************************
##
##States:
##**************
##S=suspectible; E=exposed (infected); I=infectious ; R=recovered
##Over the simulation we use this number to classify each states
##S=0; E=1; I=2; R=3
##******************************************************************************

library(ggplot2)
library(gridExtra)
#Create function to calculate number of new infected in each population
covid_simulation<- function(pop_size,sim_days,beta_value){
  ##function to plot the new infection each days for the three observed 
  ##population.
  ##pop_size = population size; sim_days = observed days during simulation;
  ##beta_value = the contact rate for this simulation (beta).
  
  
  ##initialize simulation data
  first_exposed=10 ##initial number of exposed people
  gamma=1/3 ##daily probability E -> I
  delta=1/5 ##daily probability I -> R
  lamda=0.4/pop_size ##overall infectivity parameters
  x<-rep(0,pop_size)## initialize to susceptible state
  
  
  ##Search 10% people with lowest beta value
  #1beta_sort<-sort(beta_value,decreasing = TRUE)##sort value from big to small
  #1beta_lower <- tail(beta_sort,n*0.1)##take 10% people with lowest beta value
  #1lower_index<-which(beta_value %in% beta_lower)
  lower_index<-order(beta_value, decreasing = FALSE)[1:550000]
  
  
  ##Random sample 0.1% people from the whole population
  sample_beta<-sample(beta_value,pop_size*0.001)##take 0.1% random sample from the beta
  sample_index<-which(beta_value %in% sample_beta)
  #sample_index<-sample(c(1:n),n*0.001)
  
  ##Notation: 
  ##x = whole population; y = 10% of population; z = 0.1% of population
  
  x[1:first_exposed]<-1 #set 10 people to exposed state
  y <- x[lower_index]
  z <- x[sample_index]
  
  #S<-E<-I<-R<-rep(0,nt);A<-W<-U<-P<-rep(0,nt); D<-Q<-G<-K<-rep(0,nt)
  I<-U<-G<-rep(0,sim_days)## set up storage for pop in each new infected each day
  
  ## Details: 
  ##I=new infected each day in whole population;
  ##U=new infected each day in 10% of population with lowest beta value;
  ##G=new infected each day in 0.1% of population;
  
  # S[1]<-n-ne;E[1]<-ne;A[1]<-n-ne;W[1]<-ne;D[1]<-n-ne;Q[1]<-ne
  
  
  ##simulation for 150 days
  for (i in 2:sim_days){
    
    u<-runif(pop_size)##generate random deviates based on the population size
    
    ##total people in S state from each three population
    sum_x<-sum(x==0)
    sum_y<-sum(y==0)
    sum_z<-sum(z==0)
    
    sum_beta_I<-sum(beta_value[which(x==2)])##calculate the sum of beta in I state
    
    x[x==2&u<delta]<-3 ## I -> R with probability delta
    
    x[x==1&u<gamma]<-2 ## E -> I with probability gamma
    
    
    x[x==0&u<beta_value*lamda*sum_beta_I]<-1
    #x[which(x==0)][u[which(x==0)]<sum_beta_I*lamda*beta_value[which(x==0)]]<-1
    ## S -> E with probability beta[j]*lambda*sum(beta[i] for all i in I)
    y <- x[lower_index]
    z <- x[sample_index]
    
    
    newly_infected_x<-sum_x-sum(x==0)## new infected each day for whole population
    I[i]<-newly_infected_x
    
    newly_infected_y<-sum_y-sum(y==0)## new infected each day for 10% population
    U[i]<-newly_infected_y
    
    newly_infected_z<-sum_z-sum(z==0)## new infected each day for 0.1% population
    G[i]<-newly_infected_z
    
    
    
    #x[which(x==0)][u[which(x==0)]<sum(beta[which(x==2)])*lamda*beta[which(x==0)]]<-1
    
    #S[i]<-sum(x==0);E[i]<-sum(x==1);
    #I[i]<-sum(x==2)-sum_x
    #;R[i]<-sum(x==3)
    
    #A[i]<-sum(y==0);W[i]<-sum(y==1);
    #U[i]<-sum(y==2)-sum_y
    #;P[i]<-sum(y==3)
    
    #D[i]<-sum(z==0);Q[i]<-sum(z==1);
    # G[i]<-sum(z==2)-sum_z
    #;K[i]<-sum(z==3)
    
    
    
    
  }
  
  
  #list (S=S,E=E,I=I,R=R,beta=beta);list (A=A,W=W,U=U,P=P,beta=beta);list (D=D,Q=Q,G=G,K=K,beta=beta)
  list (I=I,U=U,G=G,beta=beta_value)
  
  ##standarize and plot each new infected
  #plot(I/n,ylim=c(0,max(I/n,U/(0.1*n),G/(0.001*n))),xlab="day",ylab="N",col='red',type='l')
  #lines(U/(0.1*n),ylim=c(0,max(U/(0.1*n))),xlab="day",ylab="N",col='green',type='l')
  #lines(G/(0.001*n),ylim=c(0,max(G/(0.001*n))),xlab="day",ylab="N",col='blue',type='l')
  
  ###plot(I,ylim=c(0,max(I)),xlab="day",ylab="N",col='red',type='l')
  ###points(U,ylim=c(0,max(U)),xlab="day",ylab="N",col='green',type='l')
  ###lines(G,ylim=c(0,max(G)),xlab="day",ylab="N",col='blue',type='l')
  
  ##Add legend and threshold line at the peak
  #df<-data.frame(I=I/n,I_lower=U/(0.1*n),I_sample=G/(0.001*n),days=1:nt)
  #abline(v = which(I/pop_size==max(I/pop_size)), col = "red", lty = 3)
  #abline(v = which(U/(0.1*pop_size)==max(U/(0.1*pop_size))), col = "green", lty = 3)
  #abline(v = which(G/(0.001*pop_size)==max(G/(0.001*pop_size))), col = "blue", lty = 3)
  #legend(1, 0.015, legend=c("whole", "0.1%", "10%"),col=c("red", "blue","green"), lty=1, cex=0.5, text.font=3,text.col='black',box.lwd = 0,box.co='white')
  
  ##Display horizontal line
  #abline(h = max(I/pop_size), col = "red", lty = 3)
  #abline(h = max(U/(0.1*pop_size)), col = "green", lty = 3)
  #abline(h = max(G/(0.001*pop_size)), col = "blue", lty = 3)
}


##initialize data
n<-5500000 ##Scotland population
nt<-150 ##number of observed days
beta<- rlnorm(n,0,0.5); beta <- beta/mean(beta) ##beta value
epi<-covid_simulation(n,nt,beta)##initialize simulation
pop_infect<-data.frame(I_whole=epi$I,I_lower=epi$U,I_sample=epi$G,days=1:nt)

##standarize and plot each new infected
great_max<-max(max(pop_infect$I_whole/n),max(pop_infect$I_lower/(0.1*n)),max(pop_infect$I_sample/(0.001*n)))
plot(pop_infect$I_whole/n,ylim=c(0,great_max),xlab="day",ylab="N",col='red',type='l')
lines(pop_infect$I_lower/(0.1*n),ylim=c(0,max(pop_infect$I_lower/(0.1*n))),xlab="day",ylab="N",col='green',type='l')
lines(pop_infect$I_sample/(0.001*n),ylim=c(0,max(pop_infect$I_sample/(0.001*n))),xlab="day",ylab="N",col='blue',type='l')

##Display horizontal line
abline(h = max(pop_infect$I_whole/n), col = "red", lty = 3)
abline(h = max(pop_infect$I_lower/(0.1*n)), col = "green", lty = 3)
abline(h = max(pop_infect$I_sample/(0.001*n)), col = "blue", lty = 3)

##Display vertical line
abline(v = pop_infect$days[pop_infect$I_whole==max(pop_infect$I_whole)], col = "red", lty = 3)
abline(v = pop_infect$days[pop_infect$I_lower==max(pop_infect$I_lower)], col = "green", lty = 3)
abline(v = pop_infect$days[pop_infect$I_sample==max(pop_infect$I_sample)], col = "blue", lty = 3)

##Add legend and title to plot
legend(1, 0.013, legend=c("whole", "0.1%", "10%"),col=c("red", "blue","green"), lty=1, cex=0.5, text.font=3,text.col='black',box.lwd = 0,box.co='white')
mtext(text="New Infection Each Day",side=3)
##Running 10 replicate simulations

par(mfcol=c(10,3),mar=c(4,4,1,1))
##R build in function to set graphical parameters
##mar = a numerical vector of the form c(bottom, left, top, right) to modify the 
##graph margin where its element indicates the margin size;
##mfcol = a vector in the form c(nr,nc) where nr = number of row and 
##nc = number of column. 
## *************************************************************************

plot_data <- data.frame(I_whole=numeric(),I_lower=numeric(),I_sample=numeric(),simulation=character(),days=numeric())
for (i in 1:10){
  ##function to running 10 replicate simulations
  beta<- rlnorm(n,0,0.5); beta <- beta/mean(beta)
  epi<-covid_simulation(n,nt,beta)##initialize simulation
  new_data <- data.frame(I_whole=epi$I,I_lower=epi$U,I_sample=epi$G,simulation=rep(as.character(i),nt),days=1:nt)
  plot_data <- rbind(plot_data,new_data)
}

##Standarized all new infected data
plot_data$I_whole<-plot_data$I_whole/n
plot_data$I_lower<-plot_data$I_lower/(0.1*n)
plot_data$I_sample<-plot_data$I_sample/(0.001*n)

##Find the highest new infection per simulation in each population
max_whole <- aggregate(plot_data$I_whole, by = list(plot_data$simulation), max)
colnames(max_whole) <- c("Simulation", "Infected_Maximum")

max_lower <- aggregate(plot_data$I_lower, by = list(plot_data$simulation), max)
colnames(max_lower) <- c("Simulation", "Infected_Maximum")

max_sample <- aggregate(plot_data$I_sample, by = list(plot_data$simulation), max)
colnames(max_sample) <- c("Simulation", "Infected_Maximum")

##Find which day that have the highest new infection per simulation in each population
record_data <- data.frame(day_whole=numeric(),day_lower=numeric(),day_sample=numeric(),simulation=character())
for (i in 1:10){
  whole_day <- plot_data$days[which(plot_data$I_whole==max(plot_data$I_whole[plot_data$simulation==as.character(i)]))]
  lower_day <- plot_data$days[which(plot_data$I_lower==max(plot_data$I_lower[plot_data$simulation==as.character(i)]))]
  sample_day <-plot_data$days[which(plot_data$I_sample==max(plot_data$I_sample[plot_data$simulation==as.character(i)]))]
  input_data <- data.frame(day_whole=whole_day,day_lower=lower_day,day_sample=sample_day,simulation=as.character(i))
  record_data <- rbind(record_data,input_data)
}

##plot the new infected in whole population
plot1<-ggplot(data = plot_data, aes(x = days, y = I_whole, color = simulation))+geom_line()+ ggtitle("New Infected in Whole Population")
plot1<-plot1+labs(y="N", x = "day")+ theme_classic()
plot1<-plot1+geom_hline(yintercept=max(plot_data$I_whole),color='blue',linetype=2)
##plot1<-plot1+geom_vline(xintercept=day_whole,color='blue',linetype=2)

##plot the new infected in 10% population
plot2<-ggplot(data = plot_data, aes(x = days, y = I_lower, color = simulation))+geom_line()+ ggtitle("New Infected in 10% Population")
plot2<-plot2+labs(y="N", x = "day")+ theme_classic()
plot2<-plot2+geom_hline(yintercept=max(plot_data$I_lower),color='blue',linetype=2)
##plot2<-plot2+geom_vline(xintercept=day_lower,color='blue',linetype=2)

##plot the new infected in 0.1% population
plot3<-ggplot(data = plot_data, aes(x = days, y = I_sample, color = simulation))+geom_line()+ ggtitle("New Infected in 0.1% Population")
plot3<-plot3+labs(y="N", x = "day")+theme_classic()
plot3<-plot3+geom_hline(yintercept=max(plot_data$I_sample),color='blue',linetype=2)
##plot3<-plot3+geom_vline(xintercept=day_sample,color='blue',linetype=2)


##plot simulation vs maximum new infection during each simulation
plot4<-ggplot(data = max_whole, aes(x = Simulation, y = Infected_Maximum))+geom_bar(stat="identity", fill="steelblue")
plot4<-plot4+theme_classic()+labs(y="N", x = "simulation")
plot5<-ggplot(data = max_lower, aes(x = Simulation, y = Infected_Maximum))+geom_bar(stat="identity", fill="steelblue")
plot5<-plot5+theme_classic()+labs(y="N", x = "simulation")
plot6<-ggplot(data = max_lower, aes(x = Simulation, y = Infected_Maximum))+geom_bar(stat="identity", fill="steelblue")
plot6<-plot6+theme_classic()+labs(y="N", x = "simulation")

##plot simulation vs maximum new infection during each simulation
plot7<-ggplot(data = record_data, aes(x = simulation, y = day_whole))+geom_bar(position="dodge",stat="identity", fill="steelblue")
plot7<-plot7+theme_classic()+labs(y="day", x = "simulation")
plot8<-ggplot(data = record_data, aes(x = simulation, y = day_lower))+geom_bar(stat="identity", fill="steelblue")
plot8<-plot8+theme_classic()+labs(y="day", x = "simulation")
plot9<-ggplot(data = record_data, aes(x = simulation, y = day_sample))+geom_bar(stat="identity", fill="steelblue")
plot9<-plot9+theme_classic()+labs(y="day", x = "simulation")
## Create the subplot
##grid.arrange(plot1,plot2,plot3,nrow=1)
####From the peak and trend of infected rate of whole population and 10% population with the smallest contact rate,
#####we can find that the simulation for 10% people will underestimate the infection rate.Based on this conclusion,
####the incidence reconstructed from the ZOE data could underestimate the infected population since its study object
####value covid more and they might take more protective measures than more general people.