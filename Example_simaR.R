library(plotrix) # Useful to plot CI´s

#### Fitting type-II ####
source("simaR_v06.R") #Loading the functions#

Means<-data.frame(N = c(3,5,7,20,40),Means=c(1.25,2.5,3,5,6.5)) #Loading fake data at 5 initial prey densities#
plot(Means,pch=16)
Means[,2]<-Means[,2]/Means[,1] #Calculating the average proportion of prey killed at each initial prey density#
New_data<-simData(Means,20,50) #Simulating 50 curves with 20 samples per treatment (N)#
newTests(New_data) #Performing the discrimination between type-II and type III responses#
New_curves<-getFitData(New_data,24,"rogersII",6.5)  #Fitting the new data to a type-II response#
plotCurves(New_curves,40,30) #Plotting the curves#
New_rates<-Max_attackRates(New_curves,24,"rogersII") #Calculating the simulated maximum attak rates#
Results<-MARbootstrapping(New_rates,999,0.95,"norm") #Bootstrapping the simulated maximum attak rates#
par(family="serif",font=1,mar=c(5,5,1,2)) #Plotting the simulated-MAR and its confidence intervals#
plotCI(1, mean(New_rates), ui=Results$ui,li=Results$li,
       xaxt = "n",xlab="",ylab=c("Simulated maximum attack rate mean (T/Th)"),
       gap=T,pt.bg=par("bg"),pch=16)
axis(labels=c("Treatment"),side=1,tck=-0.03,at=1,las=2)


#### Fitting type-III ####
source("simaR_v06.R") #Loading the functions#

Means<-data.frame(N = c(3,5,7,10,15,20,30,40,50,70),Means=c(2.25,2.0,2.1,5,4.5,15,28,30,29,30)) #Loading fake data at 10 initial prey densities#
plot(Means, pch=16)
Means[,2]<-Means[,2]/Means[,1] #Calculating the average proportion of prey killed at each initial prey density#
New_Curves_real77r<-simData(Means,20,100) #Simulating 100 curves with 10 samples per treatment (N) #
newTests(New_Curves_real77r) #Testing the type of functional response #

# the frair_test function will say type_II, 
# so let´s check the "q" exponent given by 
# the flexp model type.

#####################################################
## Investigating the exponent (q) of attack rate ####
######## Is q > 0? If so, then type-III #############
#####################################################

nuevos_datos_real77r<-getFitData(New_Curves_real77r,24,"real77r",30)  #Fitting the new data with real77r#
#Depending on the version of "frair" real77 and real77r will not work, if so, try flexp and flepxnr that are exactly the same#

# Extracting the fitting stats of each simulated curve
summaries<-list()
for(i in 1:length(nuevos_datos_real77r)){ 
  summaries[[i]]<-summary(nuevos_datos_real77r[[i]]$fit)
}
summaries

# Extracting the q-parameter of each simulated curve
qs<-as.data.frame(matrix(nrow=length(nuevos_datos_real77r),ncol=1))
for(i in 1:length(nuevos_datos_real77r)){ 
  qs[[i,1]]<-summaries[[i]]@coef[2]
}
qs
plot(qs,xlim=c(-1,2),pch=16) ###Yeap, type-III, remember to check for outliers###

rates_real77r<-Max_attackRates(nuevos_datos_real77r,24,"real77r")   #Extracting the simulated maximum attack rates#
match(boxplot.stats(rates_real77r)$out,rates_real77r)
plot(rates_real77r)
plotCurves(nuevos_datos_real77r,70,50)
resultados_real77r<-MARbootstrapping(rates_real77r,999,0.95,"norm")    #Bootstrapping the simulated maximum attack rates#

par(family="serif",font=1,cex=1.1,mar=c(3,5,1,4))
plotCI(c(1),mean(rates_real77r),
       ui=c(resultados_real77r$ui),
       li=c(resultados_real77r$li),
       xaxt = "n",yaxt = "n",xlab="",ylab=c("Simulated maximum attack rate mean (T/Th)"),gap=T,pt.bg=par("bg"),pch=16,ylim=c(34,36))
axis(labels=c("34","36"),side=2,tck=-0.03,at=c(34,36),las=2)
axis(labels=c("Treatment"),side=1,tck=-0.03,at=1,las=2)
