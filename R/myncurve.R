#' Normal curve plotter and probability finder
#'
#' @param mu The mean of the normal distribution
#' @param sigma The standard deviation of the normal distribution
#' @param a The number we will find the probability P(x <= a)
#'
#' @return The probability P(x <= a)
#' @export
#'
#' @importFrom graphics curve polygon
#' @importFrom stats dnorm pnorm
#'
#' @examples
#' \dontrun{myncurve(mu=0, sigma=1, a=0)}
myncurve = function(mu, sigma, a) {
  x <- NULL # global variable issue
  curve(dnorm(x, mean=mu, sd=sigma), xlim=c(mu-3*sigma, mu+3*sigma))
  xcurve <- seq(mu-3*sigma, a, length=1000)
  ycurve <- dnorm(xcurve, mean=mu, sd=sigma)
  polygon(c(mu-3*sigma, xcurve, a), c(0, ycurve, 0), col="Red")
  pnorm(a, mean=mu, sd=sigma)
}
