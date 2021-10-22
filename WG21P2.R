##Group member:
##1. Aditya Prabaswara Mardjikoen(s2264710)
##2. Xiangtian Duan(s2248742)
##3. Huiying Chen(s2264943)

## Github repositories link: 
## https://github.com/aprabaswara/Statistical-Programming-Task-2-Covid-and-the-hazards-of-opportunistic-sampling-Group-21.git
## Github repositories invitation link:
## https://github.com/aprabaswara/Statistical-Programming-Task-2-Covid-and-the-hazards-of-opportunistic-sampling-Group-21/invitations



##Overview:
##----------------------------------------------------------------------------------------
##The aims of this project are as follows:
##
##1.record daily infection trajectories of whole population, the "cautious 10%"
##  (with the lowest individual transmission probabilities) and the 0.1% random sample,
##  show them in a graph, find the day on which each trajectory peaks and make comparision
##
##2.make 10 simulations and plot three graphs to visualize the variability in the results 
##  from simulation to simulation
##
##3.use these results to interpret and analyse daily infection trajectories reconstructed 
##  using the ZOE app data.
##----------------------------------------------------------------------------------------
##
##
##Model Assumption:
##----------------------------------------------------------------------------------------
##1. The ith person in the whole population has a relative contact rate with
##   other people of beta[j], so that the daily chance of a transmission
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
##----------------------------------------------------------------------------------------
##
##Simulation description:
##----------------------------------------------------------------------------------------
##1.In this code, we condstruc a model with covid_simulation function. In this model,
##  we simulate for 150 days and record the number of new infections each day among the 
##  whole popeulation, the 10% of the population with the lowest beta[i] values,
##  the 0.1% random sample of the population. 
##
##2.we also presented the results of the simulation
##  in the graph.
##
##3.make 10 simulation useing this model and visualize the variability of simulations.
##----------------------------------------------------------------------------------------
##
##Algorithm:
##----------------------------------------------------------------------------------------
##1. Set the number of Scotland population size (n), which is 5.5 million. 
##   Set 150 model days.
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
##5. Set 10 people randomly into the exposed state (E) to start the epidemic.
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
##9. Plot the result from step 8 in one plot.
##
##10.Run all simulations from step 1 until step 9 for 10 times  
##   and plot them in a subplot that contains 3 graph.
##----------------------------------------------------------------------------------------
##
##States:
##----------------------------------------------------------------------------------------
##S=suspectible; E=exposed and infected; I=infectious ; R=recovered
##Over the simulation we use this number to classify each states
##S=0; E=1; I=2; R=3
##----------------------------------------------------------------------------------------
##
## In this task we use three library: ggplot2, gridExtra, and grid. We use this three 
## library because they are suitable for plotting data based on more than 4 category 
## and suitable for modifying grid and subplot. In addition, we use data frame because
## it is easy to access and we can monitored each new infection. Whats more, data frame
## can be used to access element that we want and make plotting a graph easier and fast.

library(ggplot2)##library for data visualization
library(gridExtra)##library for modifying grid and build subplot based on ggplot2
library(grid) ##library for modifying grid

#Create function to calculate number of new infected in the whole population, 10% of the 
#population that has lowest beta value, and 0.1% of the people selected randomly.
covid_simulation<- function(pop_size,sim_days,beta_value){
  
  ##pop_size = population size; sim_days = observed days during simulation;
  ##beta_value = the contact rate for this simulation (beta).
  
  
  ##initialize simulation data
  
  gamma=1/3 ##daily probability E -> I
  delta=1/5 ##daily probability I -> R
  lambda=0.4/pop_size ##overall infectivity parameters
  first_exposed=10 ##initial number of exposed people
  x<-rep(0,pop_size)## initialize to susceptible state
  
  
  ##Search the index of 10% people in beta_value with the lowest beta values
  lower_index<-order(beta_value, decreasing = FALSE)[1:550000]
  
  
  ##Generate 0.1% random sample from the whole population and extract their index
  sample_beta<-sample(beta_value,pop_size*0.001)##take 0.1% random sample from the beta
  sample_index<-which(beta_value %in% sample_beta)
  
  
  ##Notation: 
  ##x = whole population; y = 10% of population; z = 0.1% of population
  
  #set 10 people to exposed state
  x[1:first_exposed]<-1 
  
  #initialize state of the first day for 10% population group
  y <- x[lower_index]
  
  #initialize state of the first day for 0.1% population group
  z <- x[sample_index]
  
  #S<-E<-I<-R<-rep(0,sim_days)
  I<-U<-G<-rep(0,sim_days)## set up storage for pop in each new infected each day
  
  ## Details: 
  ##I=new infected each day in whole population;
  ##U=new infected each day in 10% of population with lowest beta value;
  ##G=new infected each day in 0.1% of population;
  
  #S[1]<-n-ne;E[1]<-ne;
  
  
  ##simulation for 150 days
  for (i in 2:sim_days){
    
    ##generate random deviates based on the population size
    u<-runif(pop_size)
    
    
    ##Notation: 
    ##x = whole population; y = 10% of population; z = 0.1% of population
    ##all the probabilities are defined and calculated from the (i-1)day to the ith day here
    
    ##number of people in 0 state(susceptible) from each of three groups on the (i-1)th day
    sum_x<-sum(x==0)
    sum_y<-sum(y==0)
    sum_z<-sum(z==0)
    
    ##calculate the sum of beta in I state on the (i-1)th day
    sum_beta_I<-sum(beta_value[which(x==2)])
    
    
    ## I -> R with probability delta
    x[x==2&u<delta]<-3 
    
    ## E -> I with probability gamma
    x[x==1&u<gamma]<-2 
    
    ## S -> E with probability beta_value*lambda*sum_beta_I
    x[x==0&u<beta_value*lambda*sum_beta_I]<-1
   
    
    ##determine state of ith days for 10% population group 
    y <- x[lower_index]
    
    ##determine state of ith days for 0.1% population group 
    z <- x[sample_index]
    
    ## new infected population of ith day for whole population
    newly_infected_x<-sum_x-sum(x==0)
    I[i]<-newly_infected_x
    
    ## new infected population of ith day for 10% population
    newly_infected_y<-sum_y-sum(y==0)
    U[i]<-newly_infected_y
    
    ## new infected population of ith day for 0.1% population
    newly_infected_z<-sum_z-sum(z==0)
    G[i]<-newly_infected_z
    
    
    
    
  }
  
  list (I=I,U=U,G=G,beta=beta_value)
  
}


##initialize data
n<-5500000 ##Scotland population
nt<-150 ##number of observed days
beta<- rlnorm(n,0,0.5); beta <- beta/mean(beta) ##beta value
epi<-covid_simulation(n,nt,beta)##initialize simulation
pop_infect<-data.frame(I_whole=epi$I,I_lower=epi$U,I_sample=epi$G,days=1:nt)

##standarize and plot new infected for the whole population, 10% popoulation, and 0.1% population
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

##Add axis (in order to show the coordinate value more clear, we commented function that shows coordinator valus for 10% and 0.1% population)
axis(1,at=pop_infect$days[pop_infect$I_whole==max(pop_infect$I_whole)],labels = pop_infect$days[pop_infect$I_whole==max(pop_infect$I_whole)], col = "red",lty=3)
axis(2,at=max(pop_infect$I_whole/n),labels = max(pop_infect$I_whole/n), las=1, cex.axis=0.8)
#axis(1,at=pop_infect$days[pop_infect$I_lower==max(pop_infect$I_lower)],labels = pop_infect$days[pop_infect$I_lower==max(pop_infect$I_lower)], col = "black",lty=3)
#axis(2,at=max(pop_infect$I_lower/(0.1*n)),labels = max(pop_infect$I_lower/(0.1*n)), las=1, cex.axis=0.8)
#axis(1,at=pop_infect$days[pop_infect$I_sample==max(pop_infect$I_sample)],labels = pop_infect$days[pop_infect$I_sample==max(pop_infect$I_sample)], col = "blue", lty = 3)
#axis(2,at=max(pop_infect$I_sample/(0.001*n)),labels = max(pop_infect$I_sample/(0.001*n)), las=1, cex.axis=0.8)

##Add legend and title to plot
legend(1, 0.02, legend=c("whole", "0.1%", "10%"),col=c("red", "blue","green"), lty=1, cex=0.5, text.font=3,text.col='black',box.lwd = 0,box.co='white')
mtext(text="New Infection Each Day",side=3)



##----------------------------------------------------------------------------------------
## In this part we will build the subplot for the new infection each day in all three population.
## We plot them in one graph that contain 10 plot of new infection for each population because we
## can monitored how the peak changed during each simulation. In addition, we build a subplot 
## contain the peak of the new infection each days and the day where the peak appears.

##Running 10 replicate simulations
plot_data <- data.frame(I_whole=numeric(),I_lower=numeric(),I_sample=numeric(),simulation=character(),days=numeric())
for (i in 1:10){
  ##function to running 10 replicate simulations
  beta<- rlnorm(n,0,0.5); beta <- beta/mean(beta)
  epi<-covid_simulation(n,nt,beta)##initialize simulation
  new_data <- data.frame(I_whole=epi$I,I_lower=epi$U,I_sample=epi$G,simulation=rep(as.character(i),nt),days=1:nt)
  plot_data <- rbind(plot_data,new_data)
}

##Standarized all new infected data and keep the original data
plot_data_original <- plot_data
plot_data$I_whole<-plot_data$I_whole/n
plot_data$I_lower<-plot_data$I_lower/(0.1*n)
plot_data$I_sample<-plot_data$I_sample/(0.001*n)

##Find the highest new infection per simulation in each group
max_whole <- aggregate(plot_data_original$I_whole, by = list(plot_data_original$simulation), max)
colnames(max_whole) <- c("Simulation", "Infected_Maximum")

max_lower <- aggregate(plot_data_original$I_lower, by = list(plot_data_original$simulation), max)
colnames(max_lower) <- c("Simulation", "Infected_Maximum")

max_sample <- aggregate(plot_data_original$I_sample, by = list(plot_data_original$simulation), max)
colnames(max_sample) <- c("Simulation", "Infected_Maximum")

##Find which day that have the highest new infection per simulation in each group
record_data <- data.frame(day_whole=numeric(),day_lower=numeric(),day_sample=numeric(),simulation=character())
for (i in 1:10){
  whole_day <- plot_data$days[which(plot_data$I_whole==max(plot_data$I_whole[plot_data$simulation==as.character(i)]))]
  lower_day <- plot_data$days[which(plot_data$I_lower==max(plot_data$I_lower[plot_data$simulation==as.character(i)]))]
  sample_day <-plot_data$days[which(plot_data$I_sample==max(plot_data$I_sample[plot_data$simulation==as.character(i)]))]
  input_data <- data.frame(day_whole=whole_day,day_lower=lower_day,day_sample=sample_day,simulation=as.character(i))
  record_data <- rbind(record_data,input_data)
}


##plot the new infected in whole population
plot1<-ggplot(data = plot_data, aes(x = days, y = I_whole, color = simulation))+geom_line()+ ggtitle("Whole Population")
plot1<-plot1+labs(y="N", x = "day")+ theme_classic()+theme(legend.position = 'none')

##plot the new infected in 10% population
plot2<-ggplot(data = plot_data, aes(x = days, y = I_lower, color = simulation))+geom_line()+ ggtitle("Cautious Population")
plot2<-plot2+labs(y="N", x = "day")+ theme_classic()+theme(legend.position = "none")

##plot the new infected in 0.1% population
plot3 <- ggplot(data = plot_data, aes(x = days, y = I_sample, color = simulation))+geom_line()+ ggtitle("0.1% Population")
plot3 <- plot3+labs(y="N", x = "day")+theme_classic()+theme(legend.position = "right")


##plot simulation vs maximum new infection during each simulation
plot4<-ggplot(data = max_whole, aes(x = Simulation, y = Infected_Maximum))+geom_bar(stat="identity", fill="steelblue")+ ggtitle("Whole Population")
plot4<-plot4+theme_classic()+labs(y="N", x = "simulation")+geom_text(aes(label = Infected_Maximum), vjust = 1.5, colour = "white")
plot5<-ggplot(data = max_lower, aes(x = Simulation, y = Infected_Maximum))+geom_bar(stat="identity", fill="steelblue")+ ggtitle("Cautious Population")
plot5<-plot5+theme_classic()+labs(y="N", x = "simulation")+geom_text(aes(label = Infected_Maximum), vjust = 1.5, colour = "white")
plot6<-ggplot(data = max_lower, aes(x = Simulation, y = Infected_Maximum))+geom_bar(stat="identity", fill="steelblue")+ ggtitle("0.1% Population")
plot6<-plot6+theme_classic()+labs(y="N", x = "simulation")+geom_text(aes(label = Infected_Maximum), vjust = 1.5, colour = "white")

##plot simulation vs maximum new infection during each simulation
plot7<-ggplot(data = record_data, aes(x = simulation, y = day_whole))+geom_bar(stat="identity", fill="steelblue")+ ggtitle("Whole Population")
plot7<-plot7+theme_classic()+labs(y="day", x = "simulation")+geom_text(aes(label = day_whole), vjust = 1.5, colour = "white")
plot8<-ggplot(data = record_data, aes(x = simulation, y = day_lower))+geom_bar(stat="identity", fill="steelblue")+ ggtitle("Cautious Population")
plot8<-plot8+theme_classic()+labs(y="day", x = "simulation")+geom_text(aes(label = day_lower), vjust = 1.5, colour = "white")
plot9<-ggplot(data = record_data, aes(x = simulation, y = day_sample))+geom_bar(stat="identity", fill="steelblue")+ ggtitle("0.1% Population")
plot9<-plot9+theme_classic()+labs(y="day", x = "simulation")+geom_text(aes(label = day_sample), vjust = 1.5, colour = "white")

## Create the subplot
grid.arrange(plot1, plot2,plot3, ncol=2, nrow = 2)
grid.arrange(plot4, plot5,plot6, ncol=2, nrow = 2)
grid.arrange(plot7, plot8, plot9, ncol=2, nrow = 2)


####Conclusion:
####From the peak and trend of infected rate of whole population and 10% population with the smallest contact rate,
#####we can find that the simulation for 10% people have the lower infection rate.Based on this conclusion,
####the incidence reconstructed from the ZOE data could underestimate the infected rate of whole population. Its study objects
####value COVID virus more and they might take more protective measures than more general people. Therefore the infected rate of ZOE
####users would be lower than the whole population.