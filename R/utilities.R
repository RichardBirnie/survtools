#' Extract data describing the Kaplan-Meier curve from a \code{\link[survival]{survfit}} object
#'
#' @param km A survfit object containing the Kaplan-Meier estimate of the
#'   survival function as returned by \code{\link[survival]{survfit}}
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{Time}{The time of the events}
#'     \item{Survival}{The probability of survival at each event time}
#'     \item{stderr}{The standard error of the survival estimate}
#'     \item{lower}{The lower limit of the confidence interval for the survival curve}
#'     \item{upper}{The upper limit of the confidence interval for the survival curve}
#'     \item{Nrisk}{The number of patients at risk at each event time}
#'     \item{group}{(Optional) The group that each observation belongs to if
#'     there are multiple patient groups. This will be absent if there is only
#'     one group}
#'     }
#'
#' @example inst/examples/getKMcurve_example.R
#'
#' @export
getKMcurve <- function(km, time.col, event.col, group.col, data){
  #get summary of KM object
  KMSum <- summary(km)

  #check if there is more than one group of patients and proceed accordingly
  if(!'strata' %in% names(KMSum)){
    #extract Kaplan-Meier data from a single group of patients
    km.curve <- .extractKM(KMSum = KMSum, time.col = time.col, event.col = event.col, data = data)
  } else {
    #Tidy up the strata names. Remove everything before the equals
    KMSum$strata <- gsub('^.*=', '', KMSum$strata, perl = TRUE)
    #convert the necessary parts of KM summary into a data frame
    KMSum <- dplyr::bind_cols(KMSum[c('time', 'surv', 'std.err', 'lower', 'upper', 'n.risk', 'n.event', 'strata')])

    km.curve <- lapply(unique(KMSum$strata), function(group){
      km.group <- dplyr::filter(KMSum, strata == group)
      data.group <- plyr::dlply(data, .variables = group.col, .fun = function(x)return(x))

      km.curve.g <- .extractKM(KMSum = km.group, time.col = time.col, event.col = event.col, data.group = data.group[[group]])
      km.curve.g$group <- group
      km.curve.g
    })

    km.curve <- dplyr::bind_rows(km.curve)
  }

}

#' Internal helper function to extract Kaplan-Meier data from a single group of patients
.extractKM <- function(KMSum, time.col, event.col, data.group){
  #extract survival estimates immediately  after event occurs (bottom right corner of km). Data also contains survival at time 0
  KMdataA <- data.frame("Time" = c(0, min(data.group[data.group[event.col] == 1, time.col],na.rm = TRUE), KMSum$time, max(data.group[,time.col], na.rm = TRUE)),
                        "Survival" = c(1, 1, KMSum$surv, min(KMSum$surv)),
                        "stderr" = c(1, 1, KMSum$std.err, min(KMSum$std.err)),
                        "lower" = c(1, 1, KMSum$lower, min(KMSum$lower)),
                        "upper" = c(1, 1, KMSum$upper, min(KMSum$upper))
  )
  #extract survival estimates immediately before event occurs (top right corner of KM)
  KMdataB <- data.frame("Time" = KMSum$time[-1],
                        "Survival" = KMSum$surv[-length(KMSum$surv)],
                        "stderr" = KMSum$std.err[-length(KMSum$std.err)],
                        "lower" = KMSum$lower[-length(KMSum$lower)],
                        "upper" = KMSum$upper[-length(KMSum$upper)])
  #combine datasets
  KMdataAB <- rbind(KMdataA, KMdataB)

  #correction for sorting start of KM
  KMdataAB$Time <- ifelse(KMdataAB$Time > 0 & KMdataAB$Survival == 1, KMdataAB$Time - 0.000001, KMdataAB$Time)
  #correction for sorting end of KM
  KMdataAB$Time <- ifelse(KMdataAB$Time > 0 & KMdataAB$Survival == 0, KMdataAB$Time + 0.000001, KMdataAB$Time)

  #Reorder by Time and then by survival
  KMdataAB2 <- KMdataAB[order(KMdataAB$Time, -KMdataAB$Survival), ]

  #calculate number of right censored patients
  NRcens <-  dim(data.group[(data.group[,time.col] == max(data.group[,time.col], na.rm = TRUE) & data.group[,event.col] == 0), ])[1]
  #calculate and order number of patients at risk
  Nrisk <- sort(c(length(data.group[,time.col]), KMSum$n.risk, KMSum$n.risk - KMSum$n.event, NRcens), decreasing = TRUE)
  KMdata <- cbind(KMdataAB2, Nrisk)
}

#' Produce a summary of survival probabilities at selected time points from parametric survival models
#'
#' @param survival.models A named list of survival models as returned by
#'   \code{\link[flexsurv]{flexsurvreg}}. Note that names are essential. Even if
#'   models are just named 'model 1', 'model 2' etc
#' @param time.points A vector of time points to produce summaries. Defaults are 1, 3, 5 and 10 years
#' @param time.unit Preferred time units. This sets a multiplier to calculate
#'   landmark time points. Default is 365.25 to give time points in years.
#'   Survival at 5 years would calculated as 5 x time.unit = 5 x 365.25 =
#'   1826.25 days. It is assumed that time in the survival model was in days.
#'   For months set this to 30.4. For days set this to 1. Only days, months or years are recognised
#' @param dp Number of decimal places in the estimates. Default = 3
#' @param ... Additional arguments to be passed to summary.flexsurvreg NOT TESTED
#'
#' @details Note that this has only been tested for simple models with a single covariate, e.g treatment
#'
#' @return A data frame with survival probabilities (including 95% CI) at each time point
#'
#' @example inst/examples/survivalLandmarkSummary_example.R
#'
#' @export
survivalLandmarkSummary <- function(survival.models, time.points = c(1, 3, 5, 10), time.unit = 365.25, dp = 3, ...){

  #set a time label based on units
  if(time.unit == 365.25){t.lab <- 'year'}
  if(time.unit == 30.4){t.lab <- 'month'}
  if(time.unit == 1){t.lab <- 'day'}

  #get survival at each time point for each model
  survival.lmark <- lapply(1:length(survival.models), function(i){
    if(length(attr(survival.models[[i]]$data$m, "covnames.orig")) > 0){ #models with covariates
      #get summary for current model
      for(j in 1:length(time.points)){
        #get summary for the current time point
        surv.lmark  <-  summary(survival.models[[i]], type = 'survival',
                                t = time.points[j] * time.unit, tidy = TRUE, ...)
        #re-order the columns and amend names
        surv.lmark <- dplyr::select(surv.lmark, attr(survival.models[[i]]$data$m, "covnames.orig"), time, est, lcl, ucl)
        colnames(surv.lmark)[2:5] <- paste0(t.lab, time.points[j], '_', colnames(surv.lmark)[2:5])

        if(j == 1){
          df.tmp <- surv.lmark
        } else {
          df.tmp <- dplyr::left_join(df.tmp, surv.lmark, by = attr(survival.models[[i]]$data$m, "covnames.orig"))
        }
      }
      surv.lmark <- dplyr::bind_cols(data.frame('name' = rep(names(survival.models)[i], nrow(df.tmp))),
                                     df.tmp)
    } else { #models without covariates
      #get summary for current model
      for(j in 1:length(time.points)){
        #get summary for the current time point
        surv.lmark  <-  summary(survival.models[[i]], type = 'survival',
                                t = time.points[j] * time.unit, tidy = TRUE, ...)
        #re-order the columns and amend names
        colnames(surv.lmark) <- paste0(t.lab, time.points[j], '_', colnames(surv.lmark))

        if(j == 1){
          df.tmp <- surv.lmark
        } else {
          df.tmp <- dplyr::bind_cols(df.tmp, surv.lmark)
        }
      }
      surv.lmark <- dplyr::bind_cols(data.frame('name' = rep(names(survival.models)[i], nrow(df.tmp))),
                                     df.tmp)
    }
  })
  #convert all factors to characters to avoid warnings from bind_rows
  survival.lmark <- purrr::map(.x = survival.lmark, .f = ~purrr::modify_if(., .p = is.factor, .f = as.character))
  #combine results from different models
  survival.lmark <- dplyr::bind_rows(survival.lmark)
  survival.lmark <- dplyr::select(survival.lmark, -dplyr::ends_with('time'))
  #round all estimates
  survival.lmark <- purrr::modify_if(.x = survival.lmark, .p = is.numeric, .f = round, digits = dp)
}
