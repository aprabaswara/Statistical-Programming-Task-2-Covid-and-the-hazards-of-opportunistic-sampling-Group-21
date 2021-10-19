##Group member:
##1. Aditya Prabaswara Mardjikoen(s2264710)
##2. Xiangtian Duan(s2248742)
##3. Huiying Chen(s2264943)

## Github repositories link: 
## https://github.com/aprabaswara/Statistical-Programming-Task-2-Covid-and-the-hazards-of-opportunistic-sampling-Group-21.git



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
  y <- x[lower_index]
  z <- x[sample_index]
  
  #S<-E<-I<-R<-rep(0,nt);A<-W<-U<-P<-rep(0,nt); D<-Q<-G<-K<-rep(0,nt)
  I<-U<-G<-rep(0,nt)
  
  # S[1]<-n-ne;E[1]<-ne;A[1]<-n-ne;W[1]<-ne;D[1]<-n-ne;Q[1]<-ne
  
  
  
  for (i in 2:nt){
    
    u<-runif(n)
    
    sum_x<-sum(x==0)
    sum_y<-sum(y==0)
    sum_z<-sum(z==0)
    
    sum_beta_I<-sum(beta[which(x==2)])
    
    x[x==2&u<delta]<-3## I -> R with prob delta
    
    x[x==1&u<gamma]<-2 ## E -> I with prob gamma
    
    
    
    x[which(x==0)][u[which(x==0)]<sum_beta_I*lamda*beta[which(x==0)]]<-1
    y <- x[lower_index]
    z <- x[sample_index]
    
    
    newly_infected_x<-sum_x-sum(x==0)
    I[i]<-newly_infected_x
    
    newly_infected_y<-sum_y-sum(y==0)
    U[i]<-newly_infected_y
    
    newly_infected_z<-sum_z-sum(z==0)
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
  
  plot(I/n,ylim=c(0,max(I/n,U/(0.1*n),G/(0.001*n))),xlab="day",ylab="N",col='red',type='l')
  points(U/(0.1*n),ylim=c(0,max(U/(0.1*n))),xlab="day",ylab="N",col='green',type='l')
  lines(G/(0.001*n),ylim=c(0,max(G/(0.001*n))),xlab="day",ylab="N",col='blue',type='l')
  
  ###plot(I,ylim=c(0,max(I)),xlab="day",ylab="N",col='red',type='l')
  ###points(U,ylim=c(0,max(U)),xlab="day",ylab="N",col='green',type='l')
  ###lines(G,ylim=c(0,max(G)),xlab="day",ylab="N",col='blue',type='l')
}
n<-5500000
nt<-150
beta<- rlnorm(n,0,0.5); beta <- beta/mean(beta)
covid_simulation(n,nt,beta)

par(mfcol=c(4,3),mar=c(4,4,1,1))
for (i in 1:10){
  beta<- rlnorm(n,0,0.5); beta <- beta/mean(beta)
  covid_simulation(n,nt,beta) 
}