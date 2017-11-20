# simaR version 1.0 (2-10-2017)
# Copyright (C) 2017 Jacinto Benhadi Marín
  
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details at http://www.gnu.org/licenses/.
#
# Citation: 
# "Benhadi-Marín, J., 2017. simaR: an R library to simulate functional responses. DOI:10.5281/zenodo.1030152
# Available at: https://github.com/jbenma/simaR/edit/Simulation-functions. Version of access: Date:"

################### Simulation function ######################
simData<-function(x,y,z) {  
  # x: an object (table) of two columns, the first one containing the number of prey offered in each treatment and the second one containing the mean of the experimental data i.e. the mean number of consumed prey items for each corresponding initial prey density.
  # y: number of samples per N to be simulated.
  # z: number of curves to be simulated.
  
  # Creating an intermediate data.frame to allocate columns of simulated data; 1 column = 1 initial prey density #
  New_Data<-as.data.frame(matrix(nrow=y,ncol=nrow(x)))
  colnames(New_Data)<-x[,1]
  New_Curves<-list()
  
  for(j in 1:z){  #j counts each new curve#
    
    #Simulating new data; i counts each new sample#
    for (i in 1 : nrow(x)) {
      New_Data[,i]<-rpois(y, lambda=x[i,2])
    }
    #Correcting new data - if the simulated value (Ncons) > Noffered, force Ncons = Noffered#
    for (i in 1 : nrow(x)) {
      New_Data[,i][New_Data[,i] > as.integer(colnames(New_Data))[i]] <- as.integer(colnames(New_Data))[i]
    }
    
    # Reorganizing the new data in a list of data.frames #  
    New_Curves[[j]]<- data.frame("Offered"=sort(as.integer(rep(colnames(New_Data),y))),"Consumed"=stack(New_Data)[,1])
  }
  return(New_Curves)
}

######################## Test function #######################
newTests<-function(x){ 
  # x: an object containing the output of simData()
  
  library(frair)
  New_Test<-list()
  for(i in 1:length(x)){
    New_Test[[i]]<-frair_test(x[[1]][,2]~x[[1]][,1], data=x[[1]])
  }
  return(New_Test)
}

###################### Fitting function ######################
getFitData<-function(x,y){
  # x: an object containing the output of simData()
  # y: the time of the experiment (T)
  
    library(frair)
    options(warn=-1)
    New_Params<-list()
  for(i in 1:length(x)){
    New_Params[[i]]<-frair_fit(Consumed~Offered, data=x[[i]], response="rogersII",start=list(a = 0.001, h = 0.001), fixed=list(T=y))
  }
  return(New_Params)
}

###################### Plotting function ######################
plotCurves<-function(x,y,z){
  # x: an object containing the output of getFitData()
  # y: upper limit for the X-axis
  # z: upper limit for the Y-axis
  
  for(i in 1:length(x)){
    plot(x[[i]],lty=i,pch=i,xlim=c(0,y),ylim=c(0,z),ylab=c(""),xlab=c(""),axes=F)
    lines(x[[i]],lty=i) 
    par(new=T)
  }
  plot(x[[1]],xlim=c(0,y),ylim=c(0,z),ylab=c("Number of prey consumed (N)"),xlab=c("Initial prey density (N)"),col=0)
}

################# Maximum attack rate function ################
Max_attackRates<-function(x,y){
  # x: an object containing the output of getFitData()
  # y: the time of the experiment (T)
  
  #new data.frame for each simulated attack rate#
  simulated_attack_rates<-as.data.frame(matrix(nrow=length(x),ncol=1))
  simulated_attack_rates<-data.frame(Attack_rate=simulated_attack_rates[,1])
  
  #new data.frame for allocate the calculated maximum attack rate#
  Max_attackRates<-as.data.frame(matrix(nrow=length(x),ncol=1))
  Max_attackRates<-data.frame(Max_attack_rate=Max_attackRates[,1])
  
  for(i in 1:length(x)){
    simulated_attack_rates[i,]<-round(x[[i]][["fit"]]@details$par[2],digits=2) #Extracting the attack rate from the frair_fit output#
  }
  #Calculating the maximum attack rate for each curve (T/Th)#
  Max_attackRates<- round(y/simulated_attack_rates,digits=2) #Calculating the maximum attack rate for each curve (T/Th)#
  #Replacing infinites by NAs, supressing them and allocating the results on the its own data.frame#
  Max_attackRates<-as.vector(na.omit(replace(Max_attackRates[,1],is.infinite(Max_attackRates[,1]),NA)))
  
  return(Max_attackRates)
}

############# Own_mean function for boot package ##############
own_mean<-function(x,y) { 
  # x: a vector with the means that will be used by MARbootstrapping()
  # y: a second argument required by the boot function from boot package.
  
  ### Remember, the second argument of the function "boot" needs two parameters being the second one a vector of indices.
  mean1 = mean(x[y])
}

######### Bootstrapping function from boot package ############
MARbootstrapping<-function(x,y,z,w){ #x=output of Max_attackRates(); y=number of bootstrapping replicates; z=level of confidence; w=type of CI#
  # x: an object containing the output of Max_attackRates()
  # y: the number of bootstrap replicates.
  # z: the confidence level of the required interval.
  # w: A vector of character strings representing the type of intervals asked by the function boot.ci from boot package: "norm","basic", "stud", "perc", "bca".
  
  library(plotrix)
  library(boot)
  boot_res<-as.vector(matrix(nrow=length(x),ncol=1))
  
  #bootstrapping the mean of the maximum attack rate#
  boot_res<-boot(x, own_mean, R = y)
  
  #extracting the lower and upper limit of the confidence interval#
  return(data.frame(li=as.vector(boot.ci(boot_res, conf=z, type=w)[[4]])[2],ui=as.vector(boot.ci(boot_res, conf=0.95, type="norm")[[4]])[3]))
}
