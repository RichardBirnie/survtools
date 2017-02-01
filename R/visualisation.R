#' Plot Kaplan-Meier curves with a corresponding table of numbers at risk and
#' overlay parametric survival curves if required
#'
#' This function extends \code{\link[survminer]{ggsurvplot}} from the survminer
#' package to allow parametric survival curves to be overlaid on the
#' Kaplan-Meier curve. If only the Kaplan-Meier curve is required it may be
#' preferable to use ggsurvplot directly
#'
#' @param km A survfit object containing the Kaplan-Meier estimate of the
#'   survival function as returned by \code{\link[survival]{survfit}}
#' @param psm.curves List. Each element of the list is a data frame containing
#'   results from a parametric survival model. See example below
#' @param fun This is passed directly to the fun argument of
#'   \code{\link[survminer]{ggsurvplot}}. The default is to plot the survival
#'   curve. Set this to 'cumhaz' if you want the cumulative hazard function.
#' @param cen logical TRUE/FALSE. Should censoring points be shown on the curve. Default = FALSE
#' @param xval column name that contains x values (time)
#' @param yval column name that contains y values (survival probability)
#' @param groups column name that contains groups in your data.
#' @param plot.title Character. Text to be used as the main plot title
#' @param time.interval Integer. A numbe specifying the preferred time intervals
#'   on the x-axis. Default = 1 year
#' @param time.scale Character. This defines the scale of the time axis. Default
#'   = 'linear'. The alternative is 'log' to plot on a log time scale
#' @param x.label Character. Text to be used as the label on the x-axis. Default =
#'   Time (Years)
#' @param show.table Logical TRUE/FALSE. If the table of numbers at risk is
#'   required set this to TRUE, if not set this to FALSE. Default = TRUE
#' @param table.height Numeric. This controls the height of the number at risk table
#' @param table.title Character. Text to be used as the table title
#' @param title.legend Character Text to be used as the legend title. This is blank by default
#' @param labs.legend Character vector to be used as labels for each item in the
#'   legend. The length of the vector shoud match the numbe of groups in your
#'   data
#' @param linetype A vector of linetypes for each curve in the plot
#' @param x.limit A numeric vector of length 2 that specifies the range of the
#'   x-axis.
#' @param ... Additional arguments to be passed to \code{\link[survminer]{ggsurvplot}}
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
ggsurvplot2 <- function(km, psm.curves, cen = FALSE, xval, yval, groups, plot.title = '', time.interval = 1,
                        time.scale = 'linear', show.table = TRUE, table.height = 0.2, table.title = 'Number at risk',
                        title.legend = '', labs.legend, l.type, x.limit, x.label = 'Time (Years)',
                        cols = 'hue', ...) {
  fig <- survminer::ggsurvplot(km, break.time.by = time.interval,
                               palette = cols, xlab = x.label, risk.table = show.table, risk.table.y.text.col = FALSE,
                               risk.table.height = table.height, risk.table.title = table.title, legend.title = title.legend,
                               legend.labs = labs.legend, censor = cen, linetype = l.type, xlim = x.limit, ...
                               )

  #extract the plot component for modification
  #add the parametric curves and put the plot component back into the figure
  for(i in 1:length(psm.curves)){
  fig$plot <- fig$plot + ggplot2::geom_line(data = psm.curves[[i]],
                                   aes_string(x = xval, y = yval, colour = groups, linetype = groups)) +
    ggplot2::scale_colour_manual(values = cols) +
    ggplot2::labs(title = plot.title) +
    ggplot2::theme(plot.title = element_text(hjust = 0.5))
  }

  if(time.scale == 'log'){
    low.time <- round(x.limit[1], 1)
    hi.time <- round(x.limit[2], 1)
    fig$plot <- fig$plot + scale_x_continuous(trans = 'log')
  }

  #tidy up the table a bit and insert it back into the figure
  fig$table <- fig$table + ggplot2::theme(axis.line = element_blank(), axis.ticks = element_blank(),
                                          axis.text.x = element_blank(), axis.title = element_blank(),
                                          plot.title = element_text(size = 12)
                                          )
  fig
}
