#' Error bars (lines) for barplots
#'
#' This function adds symmetrical error bars to barplots, or returns the x-axis positions of the bars
#' @param variable Vector of values that were plotted in the barplot
#' @param errors Vector of single-sided errors (eg. standard errors)
#' @param errplot Logical indicating whether error bars need to be plot. If FALSE then the x coordinates of the bars are returned
#' @param lwd Linewidth of the error bars (default 1)
#' @param col Colour of the error bars (default black)
#' @param screen Screen number of a multi screen figure (default is current screen)
#' @keywords error bars
#' @export
#' @examples
#' tdata1 <- c(4.3, 3.9, 5.6, 4.9)
#' terr1 <- c(0.25, 0.19, 0.34, 0.23)
#'
#' barplot(tdata1)
#' barErrBars(tdata1, terr1) # simple case
#' barErrBars(tdata1, terr1, lwd=2, col="blue") # custom linewidth and Colour
#' xpos <- barErrBars(tdata1, NA, FALSE) # assigns x coodinates of the bars to xpos
#'
#' ################################################################################
#'
#' tdata2 <- c(1.2, 1.9, 0.9, 3.2, 2.6)
#' terr2 <- c(0.12, 0.09, 0.19, 0.27, 0.14, 0.22)
#'
#' close.screen(all.screens = TRUE)
#' split.screen(c(1,2))
#' screen(1)
#' barplot(tdata1)
#' screen(2)
#' barplot(tdata2)
#' 
#' barErrBars(tdata1, terr1, screen=1) # add error bars toa specific screen later
  
barErrBars <- function(variable, errors, errplot=TRUE, lwd=1, col="black", screen=screen()) {
  xpos <- barplot(variable, plot=FALSE)
  if (errplot == TRUE) {
    for (x in 1:length(xpos)) {
      lines(rep(xpos[x], 2), c(variable[x]-errors[x], variable[x]+errors[x]), lwd=lwd, col=col)
    }
  } else {
    return(xpos)
  }
}

