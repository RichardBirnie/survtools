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

#create a survival object
surv.obj <- Surv(recyrs, censrec) ~ group

#simple Kaplan-Meier estimate
surv.km <- survfit(surv.obj, data = bc)

#fit a Weibull model and get the fitted survival curve
surv.wb <- flexsurvreg(surv.obj, data = bc, dist = 'weibull')
surv.wb.summ <- summary(surv.wb, type = 'survival')

#rearrange the Weibull survival results into a format suitable for ggplot
names(surv.wb.summ) <- gsub('group=', '', names(surv.wb.summ))
n.temp <- names(surv.wb.summ)
wb.surv <- lapply(names(surv.wb.summ), function(name) {
  surv.wb.summ[[name]]$prognosis <- paste(name, 'Weibull')
  surv.wb.summ[[name]]
})
names(wb.surv) <- n.temp
wb.surv <- bind_rows(wb.surv)

#fit an exponential model and get the fitted survival curve
surv.exp <- flexsurvreg(surv.obj, data = bc, dist = 'exp')
surv.exp.summ <- summary(surv.exp, type = 'survival')

#rearrange the exponential survival results into a format suitable for ggplot
names(surv.exp.summ) <- gsub('group=', '', names(surv.exp.summ))
n.temp <- names(surv.exp.summ)
exp.surv <- lapply(names(surv.exp.summ), function(name) {
  surv.exp.summ[[name]]$prognosis <- paste(name, 'Exponential')
  surv.exp.summ[[name]]
})
names(exp.surv) <- n.temp
exp.surv <- bind_rows(exp.surv)

#put all the parametric survival results in a list
#Each element of the list a data frame with the results of the corresponding model
psm = list('Weibull' = wb.surv, 'Exponential' = exp.surv)

#plot the results
#The base layer of the plot is the Kaplan Meier curve. The parametric curves are
#added as extra layers on top. This should work of any number of additional curves
#Pay attention to the l.type, cols and labs.legend arguments. These need to be specified in
#the order that you expect them to appear in the figure legend
surv.fig <- ggsurvplot2(km = surv.km, psm.curves = psm, xval = 'time', yval = 'est',
                        groups = 'prognosis', plot.title = 'Breast Cancer Prognosis', x.limit = c(0, 7.5),
                        l.type = rep(c(2, 1, 3), length(psm)+1), title.legend = 'Prognosis',
                        labs.legend =  c('Good KM', 'Medium KM', 'Poor KM'),
                        cols = c(rep(BM.red, length(psm)+1), rep(BM.blue, length(psm)+1), rep(BM.Dblue, length(psm)+1))
                       )
surv.fig
