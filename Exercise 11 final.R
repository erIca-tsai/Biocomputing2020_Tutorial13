## Generate a script that simulates growth of the two sub-populations in the tumor to equilibrium followed by drug treatment. Plot your results using a line graph
##Input basic information 

#setworking directory
setwd("/Users/ericatsai/Desktop/untitled folder")

#defining the parameters
#r is growth rate, same for both normal and mutants, and k is carrying capacity 
r=0.1
K=1000000
#rDN is growth rate of normal/wild-type cells with drug, rDM is growth rate of mutant cells with drug
rDN=0.1
rDM=0.05

#set timesteps to stimulate
times<-1:600

#place to store output
output<-matrix(data=NA,nrow=length(times),ncol=3)
output[,1]=times
#because mutation has occured at 100 cells, set one column to start at 1 (mutant)and the next to 99 (normal)
output[1,2]=1
output[1,3]=99

#create a loop to simulate the populations to equilibrium
for(i in times[-1]){ 
  if(times[i]<=175){
    output[i,2]=output[(i-1),2]+r*output[(i-1),2]*(1-(output[(i-1),2]+output[(i-1),3])/K)
    output[i,3]=output[(i-1),3]+r*output[(i-1),3]*(1-(output[(i-1),2]+output[(i-1),3])/K)
    
  }else{
    #drug introduced after 175 timesteps
    output[i,2]=output[(i-1),2]+rDM*output[(i-1),2]*(1-(output[(i-1),2]+output[(i-1),3])/K)
    output[i,3]=output[(i-1),3]+rDN*output[(i-1),3]*(1-(output[(i-1),2]+output[(i-1),3])/K)
    
  }
}
#Output the data
outputData<-data.frame(time=output[,1], Mutants=output[,2], Wildtypes=output[,3])

#checking loop
tail(outputData)

#Plot the data with a line graph
library(ggplot2)
ggplot()+
  geom_line(data = outputData, aes(x=time,y=Mutants),color='steelblue')+ #mutants as steelblue
  geom_line(data = outputData, aes(x=time,y=Wildtypes),color='maroon')+ #wildtype cells as maroon
  xlab('Time')+
  ylab('Number of cells')

#done



