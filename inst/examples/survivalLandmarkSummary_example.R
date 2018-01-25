library(flexsurv)
library(survtools)

# Load data ---------------------------------------------------------------
#load some example data from the flexsurv package
#Survival times of 686 patients with primary node positive breast cancer
#censrec = censoring, 1 = dead, 0 = censored
#rectime = time of death or censoring in days
#recyrs = time of death or censoring in years
#group = prognostic group
data(bc)

# Fit parametric survival models -------------------------------------------
#fit 6 standard distributions
#this fits the distributions and collects the model fit objects in a list
#Simple model estimating survival for each prognostic group
psm.list <- list(
  exp = flexsurvreg(Surv(rectime, censrec) ~ group, data = bc, dist = 'exp'),
  weibull = flexsurvreg(Surv(rectime, censrec) ~ group, data = bc, dist = 'weibull'),
  gompertz = flexsurvreg(Surv(rectime, censrec) ~ group, data = bc, dist = 'gompertz'),
  llog = flexsurvreg(Surv(rectime, censrec) ~ group, data = bc, dist = 'llogis'),
  lnorm = flexsurvreg(Surv(rectime, censrec) ~ group, data = bc, dist = 'lnorm'),
  gengamma = flexsurvreg(Surv(rectime, censrec) ~ group, data = bc, dist = 'gengamma')
)

#Get summaries at 1, 3, 5, 10 and 15 years. Note that time was modelled above in days
landmark.summ <- survivalLandmarkSummary(survival.models = psm.list, time.points = c(1, 3, 5, 10, 15),
                                         time.unit = 365.25, dp = 3)

#Simpler model estimating survival for the whole cohort
psm.single <- list(
  exp = flexsurvreg(Surv(rectime, censrec) ~ 1, data = bc, dist = 'exp'),
  weibull = flexsurvreg(Surv(rectime, censrec) ~ 1, data = bc, dist = 'weibull'),
  gompertz = flexsurvreg(Surv(rectime, censrec) ~ 1, data = bc, dist = 'gompertz'),
  llog = flexsurvreg(Surv(rectime, censrec) ~ 1, data = bc, dist = 'llogis'),
  lnorm = flexsurvreg(Surv(rectime, censrec) ~ 1, data = bc, dist = 'lnorm'),
  gengamma = flexsurvreg(Surv(rectime, censrec) ~ 1, data = bc, dist = 'gengamma')
)

#Get summaries an 1, 3, 5, 10 and 15 years. Note that time was modelled above in days
landmark.summ <- survivalLandmarkSummary(survival.models = psm.single, time.points = c(1, 3, 5, 10, 15),
                                         time.unit = 365.25, dp = 3)
