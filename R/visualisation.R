#' Plot Kaplan-Meier curves with a corresponding table of numbers at risk and
#' overlay parametric survival curves if required
#'
#' This function extends \code{\link[survminer]{ggsurvplot}} from the survminer
#' package to allow parametric survival curves to be overlaid on the
#' Kaplan-Meier curve. If only the Kaplan-Meier curve is required it may be
#' preferable to use ggsurvplot directly
#'
#' @param fit A survfit object containing the Kaplan-Meier estimate of the
#'   survival function as returned by \code{\link[survival]{survfit}}
#' @param psm.curves List. Each element of the list is a data frame containing
#'   survival estimates from a parametric survival model as returned by
#'   summary.flexsurv. The list must be named. Names should be the distribution
#'   used for the survival analysis, e.g. weibull. See example below
#' @param xval column name that contains x values (time)
#' @param yval column name that contains y values (survival probability)
#' @param groups column name that contains groups in your data. e.g. treatment arm.
#' @param ... Additional arguments to be passed to
#'   \code{\link[survminer]{ggsurvplot}}
#'
#' @return An object of class "ggsurvplot" "list". This is essentially a list
#'   with two elements. The first element is the plot the second element is the
#'   table of number at risk. Both can be extracted and modified using ggplot2
#'   if required.
#'
#' @example inst/examples/ggsurvplot2_example.R
#'
#' @seealso \code{\link[survminer]{ggsurvplot}}
#'
#' @export
ggsurvplot2 <- function(fit, psm.curves = NULL, xval = 'time', yval = 'est', groups = 'group', ...) {

  fig <- survminer::ggsurvplot(fit, ...)

  if(!is.null(psm.curves)){
    #combine psm curves into a single dataframe to satisfy ggplot
    #add a variable to indicate the survival model
    for(i in 1:length(psm.curves)){
      psm.curves[[i]] <- mutate(psm.curves[[i]], model = names(psm.curves)[i])
    }
    psm.curves <- bind_rows(psm.curves)

    #extract the plot component for modification
    #add the parametric curves and put the plot component back into the figure
    fig$plot <- fig$plot + ggplot2::geom_line(data = psm.curves, aes_string(x = xval, y = yval, colour = groups)) +
      scale_y_continuous(expand = c(0, 0)) +
      ggplot2::theme(plot.title = element_text(hjust = 0.5))
  }

  #tidy up the table a bit and insert it back into the figure
  fig$table <- fig$table +
    ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(), axis.title = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = 12)
                   )
  fig
}
