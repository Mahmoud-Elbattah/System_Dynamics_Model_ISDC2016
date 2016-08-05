#Include deSolve
library(deSolve)

#Initialise simulation start and finish times, and time step
START<-2016;FINISH=2027;STEP<-1

#Create a simulation time vector, needed by deSolve
simTime <- seq(START, FINISH, by=STEP)
malePop <-  c( 422965.0,437907.0,453122.0,468454.0,484698.0,501711.0,518177.0,535249.0,552724.0,570513.0,588297.0,588297.0)
femalePop <-  c( 465222.0,479079.0,493389.0,507616.0,523315.0,539759.0,555410.0,571876.0,588662.0,606098.0,622986.0,622986.0)

popLookup<- data.frame(Time=simTime,MalePopulation=malePop,FemalePopulation=femalePop)

chos<-c("CHO1","CHO2","CHO3","CHO4","CHO5","CHO6","CHO7","CHO8","CHO9")
popPercent<- c(9.5,10.6,8.9,15.5,11.7,8.8,12.0,11.7,11.3)

choLookup <- data.frame(CHO=chos,Population=popPercent)


model<- function(time,stocks,auxs)
{
  with(as.list(c(stocks,auxs)),{
    #Calculate the flows
    #Looking up the corrsponding inflow and outflow at the current simulation time
    df<- popLookup[popLookup$Time==time,]
    fMalePatients<-(140.0 / 100000.0 ) * df$MalePopulation
    fFemalePatients<- (407.0 / 100000.0) * df$FemalePopulation
    
    sDiscahrgedPatients <- round ( (fMalePatients+ fFemalePatients),0 )
    #CHO1 calculations
    cho1Percent<-(choLookup[choLookup$CHO=="CHO1",]$Population / 100)
    fCHO1_Males<-round((cho1Percent*fMalePatients),0)
    fCHO1_Females<-round((cho1Percent*fFemalePatients),0)  
    
    #CHO2 calculations 
    cho2Percent<-(choLookup[choLookup$CHO=="CHO2",]$Population / 100)
    fCHO2_Males<-round((cho2Percent*fMalePatients),0)
    fCHO2_Females<-round((cho2Percent*fFemalePatients),0)  
    
    #CHO3 calculations
    cho3Percent<-(choLookup[choLookup$CHO=="CHO3",]$Population / 100)
    fCHO3_Males<-round((cho3Percent*fMalePatients),0)
    fCHO3_Females<-round((cho3Percent*fFemalePatients),0) 
    
    #CHO4 calculations
    cho4Percent<-(choLookup[choLookup$CHO=="CHO4",]$Population / 100)
    fCHO4_Males<-round((cho4Percent*fMalePatients),0)
    fCHO4_Females<-round((cho4Percent*fFemalePatients),0) 
    
    #CHO5 calculations
    cho5Percent<-(choLookup[choLookup$CHO=="CHO5",]$Population / 100)
    fCHO5_Males<-round((cho5Percent*fMalePatients),0)
    fCHO5_Females<-round((cho5Percent*fFemalePatients),0) 
    
    #CHO6
    cho6Percent<-(choLookup[choLookup$CHO=="CHO6",]$Population / 100)
    fCHO6_Males<-round((cho6Percent*fMalePatients),0)
    fCHO6_Females<-round((cho6Percent*fFemalePatients),0) 
    
    #CHO7
    cho7Percent<-(choLookup[choLookup$CHO=="CHO7",]$Population / 100)
    fCHO7_Males<-round((cho7Percent*fMalePatients),0)
    fCHO7_Females<-round((cho7Percent*fFemalePatients),0) 
    
    #CHO8
    cho8Percent<-(choLookup[choLookup$CHO=="CHO8",]$Population / 100)
    fCHO8_Males<-round((cho8Percent*fMalePatients),0)
    fCHO8_Females<-round((cho8Percent*fFemalePatients),0)
    
    #CHO9
    cho9Percent<-(choLookup[choLookup$CHO=="CHO9",]$Population / 100)
    fCHO9_Males<-round((cho9Percent*fMalePatients),0)
    fCHO9_Females<-round((cho9Percent*fFemalePatients),0) 
    
    #return calculations as a list,
    return (list(c(sDiscahrgedPatients),
                CHO1_Males=fCHO1_Males, CHO1_Females=fCHO1_Females,CHO2_Males=fCHO2_Males,CHO2_Females=fCHO2_Females,CHO3_Males=fCHO3_Males,CHO3_Females=fCHO3_Females,CHO4_Males=fCHO4_Males,CHO4_Females=fCHO4_Females,CHO5_Males=fCHO5_Males,CHO5_Females=fCHO5_Females,CHO6_Males=fCHO6_Males,fCHO6_Females=fCHO6_Females,CHO7_Males=fCHO7_Males,CHO7_Females=fCHO7_Females,CHO8_Males=fCHO8_Males,CHO8_Females=fCHO8_Females,fCHO9_Males=fCHO9_Males,CHO9_Females=fCHO9_Females
                 
    ))
  })
  
}

#Create the vector of stocks, and its initial value
stocks<-c(sDiscahrgedPatients=0)

#Run simulation
simOutput<- data.frame(ode(y=stocks,simTime,func = model,parms = NULL,method = "euler"))
simOutput