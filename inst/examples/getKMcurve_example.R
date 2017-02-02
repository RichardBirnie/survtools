library(survival)
library(survminer)
library(flexsurv)
library(dplyr)
library(survtools)

#load some example data from the flexsurv package
#Survival times of 686 patients with primary node positive breast cancer
#censrec = censoring, 1 = dead, 0 = censored
#rectime = time of death or censoring in days
#recyrs = time of death or censoring in years
#group = prognostic group
data(bc)

#create a survival object
surv.obj <- Surv(recyrs, censrec) ~ group

#simple Kaplan-Meier estimate
surv.km <- survfit(surv.obj, data = bc)

#Get the Kaplan-Meier data
km.curve <- getKMcurve(km = surv.km, time.col = 'recyrs', event.col = 'censrec', data = bc)

#plot KM
plot(km.curve$Time[km.curve$group == 'Good'], km.curve$Survival [km.curve$group == 'Good'], type="l", ylim=c(0,1),
     xlab = 'Time', ylab = 'Survival', col = 'red')
lines(km.curve$Time[km.curve$group == 'Medium'], km.curve$Survival [km.curve$group == 'Medium'], col='green')
lines(km.curve$Time[km.curve$group == 'Poor'], km.curve$Survival [km.curve$group == 'Poor'], col='blue')
