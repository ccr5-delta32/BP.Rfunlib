#' Daylength for any day of the year at any latitude on earth. B. Pieper, MPIPZ Cologne, November 2015.
#'
#' @param latitude Latitude in degrees. Northern hemisphere are positive, Southern hemisphere negative.
#' @param J Day of the year (1-365) where 1 is Jan-1
#' @param dayl_coeff Daylength coefficient
#' @return Daylength in hours
#' @keywords daylength
#' @section Details: Daylength coefficient determines the definition of sunset and sunrise used to calculate the daylength. The following are some examples of standard definitions and extensions: \cr
#'  0.0     - Sunrise and sunset when the center of the sun is even with horizon \cr
#'  0.26667 - Sunrise and sunset when the top of the sun is even with horizon \cr 
#'  0.83333 - This value is the summation of the radius of the sun (in  degrees  as  seen  from  Earth) plus the adopted value for the refraction of the light through the atmosphere of 34 minutes (Astronomical  Almanac  1992)(Forsythe et al., 1995). \cr
#'  6.0     - including civil twilight \cr
#' 12.0     - including nautical twilight \cr
#' 16.0     - including astronomical twilight \cr
#' @references This funtion calculates daylength according to the CBM model from W.C. Forsythe et al., 1995. Ecological Modelling 80: 87-95. 
#' @examples
#' ### The following plots the daylength in Cologne, Germany for each day of the year from Jan-1 to Dec-31
#' # create an empty plotting window with the desired coordinate system.
#' plot(1:365, type='n', axes=FALSE, xaxs='i', yaxs='i', xlim=c(1,365), ylim=c(0,24), xlab='', ylab='')
#' for (x in 1:365) {
#'   points(x, dayl(51.7519, x, 0.8333), col='red', pch=15, cex=0.4)
#' }
#' @export

dayl <- function(latitude, J, dayl_coeff=0.0) {
  theta <- 0.2163108 + 2 * atan(0.9671396 * tan(0.00860 * (J - 186)))
  phi <- asin(0.39795 * cos(theta))
  pD <- 24 - (24/pi)*acos((sin((dayl_coeff * pi)/180) + sin((latitude * pi) / 180) * phi) / (cos((latitude * pi) / 180) * cos(phi)))
  return(pD)
}
