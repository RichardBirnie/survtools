#' Extract data describing the Kaplan-Meier curve from a \code{\link[survival]{survfit}} object
#'
#' @param km A survfit object containing the Kaplan-Meier estimate of the
#'   survival function as returned by \code{\link[survival]{survfit}}
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{Time}{The time of the events}
#'     \item{Survival}{The probability of survival at each event time}
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
    # KMSum <- dplyr::bind_cols(KMSum[c(2:4, 6, 8)])
    KMSum <- dplyr::bind_cols(KMSum[c(2:4, 6, 8:9, 11:10)])

    km.curve <- lapply(unique(KMSum$strata), function(group){
      km.group <- dplyr::filter(KMSum, strata == group)
      fil <- lazyeval::interp(~y == x, .values=list(y = as.name(group.col), x = group))
      data.group <- dplyr::filter_(data, fil)
      km.curve.g <- .extractKM(KMSum = km.group, time.col = time.col, event.col = event.col, data = data.group)
      km.curve.g$group <- group
      km.curve.g
    })
    km.curve <- dplyr::bind_rows(km.curve)
  }

}

#' Internal helper function to extract Kaplan-Meier data from a single group of patients
.extractKM <- function(KMSum, time.col, event.col, data){
  #extract survival estimates immediately  after event occurs (bottom right corner of km). Data also contains survival at time 0
  KMdataA <- data.frame("Time" = c(0, min(data[data[event.col] == 1, time.col],na.rm = TRUE), KMSum$time, max(data[,time.col], na.rm = TRUE)),
                        "Survival" = c(1, 1, KMSum$surv, min(KMSum$surv)),
                        "lower" = c(1, 1, KMSum$lower, min(KMSum$lower)),
                        "upper" = c(1, 1, KMSum$upper, min(KMSum$upper))
  )
  #extract survival estimates immediately before event occurs (top right corner of KM)
  KMdataB <- data.frame("Time" = KMSum$time[-1],
                        "Survival" = KMSum$surv[-length(KMSum$surv)],
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
  NRcens <-  dim(data[(data[,time.col] == max(data[,time.col], na.rm = TRUE) & data[,event.col] == 0), ])[1]
  #calculate and order number of patients at risk
  Nrisk <- sort(c(length(data[,time.col]), KMSum$n.risk, KMSum$n.risk - KMSum$n.event, NRcens), decreasing = TRUE)
  KMdata <- cbind(KMdataAB2, Nrisk)
}
