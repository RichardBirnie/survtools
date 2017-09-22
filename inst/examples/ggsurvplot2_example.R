library(survival)
library(survminer)
library(flexsurv)
library(dplyr)
library(survtools)

#create custom colours
BM.blue = rgb(69, 185, 209, max=255)
BM.red = rgb(225, 55, 60, max=255)
BM.yellow = rgb(238, 224, 30, max=255)
BM.pink=rgb(211,78,147,max=255)
BM.Dblue=rgb(0,45,92,max=255)
BM.Dyellow = rgb(214, 200, 16, max=255)

#load some example data from the flexsurv package
#Survival times of 686 patients with primary node positive breast cancer
#censrec = censoring, 1 = dead, 0 = censored
#rectime = time of death or censoring in days
#recyrs = time of death or censoring in years
#group = prognostic group
data(bc)

# Basic Kaplan-Meier curves -----------------------------------------------
#simple Kaplan-Meier estimate
surv.km <- survfit(Surv(recyrs, censrec) ~ group, data = bc)

#edit the strata labels. These are used in the plot legend
names(surv.km$strata) <- gsub('group=', '', names(surv.km$strata))

#Plot basic Kaplan-Meier curves without any parametric curves
#This should be identical to ggsurvplot. Only shown as an example
km.fig <- ggsurvplot2(fit = surv.km, censor = FALSE, risk.table = TRUE, risk.table.height = 0.2, risk.table.y.text.col = FALSE,
                      title = 'Breast Cancer Prognosis', xlim = c(0, 7.5),
                      legend.title = 'Prognosis', legend.labs =  c('KM Good', 'KM Medium', 'KM Poor'),
                      palette = c(BM.red, BM.blue, BM.Dblue) #use this to control colour of lines
                      )

# Fit parametric survival models -------------------------------------------
#fit 6 standard distributions
#this fits the distributions and collects the model fit objects in a list
psm.list <- list(
  exp = flexsurvreg(Surv(recyrs, censrec) ~ group, data = bc, dist = 'exp'),
  weibull = flexsurvreg(Surv(recyrs, censrec) ~ group, data = bc, dist = 'weibull'),
  gompertz = flexsurvreg(Surv(recyrs, censrec) ~ group, data = bc, dist = 'gompertz'),
  llog = flexsurvreg(Surv(recyrs, censrec) ~ group, data = bc, dist = 'llogis'),
  lnorm = flexsurvreg(Surv(recyrs, censrec) ~ group, data = bc, dist = 'lnorm'),
  gengamma = flexsurvreg(Surv(recyrs, censrec) ~ group, data = bc, dist = 'gengamma')
)

#get the fitted survival curve for each distribution and store the results in a list
#this is equivalent to 'summary(model.fit.obj, type = 'survival', tidy = TRUE)' for each distribution
#Each element of the list a data frame with the results of the corresponding model
psm.summ <- lapply(psm.list, summary, type = 'survival', tidy = TRUE)

#Edit the group column to label all the permutations of group and model
for(i in 1:length(psm.summ)){
  psm.summ[[i]]$group <- paste0(names(psm.summ)[i], ' ', psm.summ[[i]]$group)
}

# Overlay parametric curves on Kaplan-Meier -------------------------------
#plot the results
#The base layer of the plot is the Kaplan Meier curve. The parametric curves are
#added as extra layers on top.
#The following arguments are specific to ggsurvplot2: psm.curves, xval, yval, groups
#All other arguments are passed directly to ggsurvplot
surv.fig <- ggsurvplot2(fit = surv.km, psm.curves = psm.summ, data = bc, xval = 'time', yval = 'est', groups = 'group',
                        censor = FALSE, risk.table = TRUE, risk.table.height = 0.2, risk.table.y.text.col = FALSE,
                        title = 'Breast Cancer Prognosis', xlim = c(0, 7.5),
                        legend.title = 'Prognosis', legend.labs =  c('KM Good', 'KM Medium', 'KM Poor'),
                        palette = rep(c(BM.red, BM.blue, BM.Dblue), length(psm.summ)+1) #use this to control colour of lines
                       )
surv.fig

# Example with a single group ---------------------------------------------
#basic KM
single.km <- survfit(Surv(recyrs, censrec) ~ 1, data = bc)

#Plot basic Kaplan-Meier curves without any parametric curves
#This should be identical to ggsurvplot. Only shown as an example
km.fig <- ggsurvplot2(fit = single.km, censor = FALSE, risk.table = TRUE, risk.table.height = 0.2, risk.table.y.text.col = FALSE,
                      title = 'Breast Cancer Prognosis', xlim = c(0, 7.5), conf.int = FALSE,
                      legend.title = '', legend.labs =  'All', palette = 'black'
)

km.fig

# Fit parametric survival models -------------------------------------------
#fit 6 standard distributions
#this fits the distributions and collects the model fit objects in a list
psm.list.single <- list(
  exp = flexsurvreg(Surv(recyrs, censrec) ~ 1, data = bc, dist = 'exp'),
  weibull = flexsurvreg(Surv(recyrs, censrec) ~ 1, data = bc, dist = 'weibull'),
  gompertz = flexsurvreg(Surv(recyrs, censrec) ~ 1, data = bc, dist = 'gompertz'),
  llog = flexsurvreg(Surv(recyrs, censrec) ~ 1, data = bc, dist = 'llogis'),
  lnorm = flexsurvreg(Surv(recyrs, censrec) ~ 1, data = bc, dist = 'lnorm'),
  gengamma = flexsurvreg(Surv(recyrs, censrec) ~ 1, data = bc, dist = 'gengamma')
)

#get the fitted survival curve for each distribution and store the results in a list
#this is equivalent to 'summary(model.fit.obj, type = 'survival', tidy = TRUE)' for each distribution
#Each element of the list a data frame with the results of the corresponding model
psm.summ.single <- lapply(psm.list.single, summary, type = 'survival', tidy = TRUE)

#Add a dummy group column
for(i in 1:length(psm.summ.single)){
  psm.summ.single[[i]]$group <- names(psm.summ.single)[i]
}

# Overlay parametric curves on Kaplan-Meier -------------------------------
#plot the results
#The base layer of the plot is the Kaplan Meier curve. The parametric curves are
#added as extra layers on top.
#The following arguments are specific to ggsurvplot2: psm.curves, xval, yval, groups
#All other arguments are passed directly to ggsurvplot
single.fig <- ggsurvplot2(fit = single.km, psm.curves = psm.summ.single, data = bc, xval = 'time', yval = 'est', groups = 'group',
                          break.time.by = 1, censor = FALSE, conf.int = FALSE, risk.table = TRUE, risk.table.height = 0.2,
                          risk.table.y.text.col = FALSE, title = 'Breast Cancer Prognosis', xlim = c(0, 8),
                          legend.title = 'Prognosis', legend.labs =  c('KM'),
                          palette = c(BM.red, BM.blue, BM.Dblue, 'black', BM.Dyellow, BM.pink, BM.yellow) #use this to control colour of lines
)
single.fig

