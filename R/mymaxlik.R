#' My Maximum Likelihood Function
#'
#' Uses the method of maximum likelihood to estimate the max likelihood
#' of a distribution, given some prior outcomes and a function that
#' returns the log likelihood given a parameter.
#'
#' @param lfun - a function that will find the log likelihood given a value-parameter pair
#' @param x - a vector of prior outcomes
#' @param param - the parameter to be passed to the distribution (for instance, p in a binomial or lambda in a Poisson)
#' @param ... - other parameters that will be passed to `plot()`
#'
#' @return maxima and also a neat graph
#' @export
#'
#' @importFrom graphics abline points axis
#'
#' @examples
#' \dontrun{
#' mymaxlik(function (x, param) log(dbinom(x, prob=param, size=20)), c(1,2,3,4), seq(0,1,length=1000))
#' }
mymaxlik=function(lfun,x,param,...){
  # how many param values are there?
  np=length(param)
  # outer -- notice the order, x then param
  # this produces a matrix -- try outer(1:4,5:10,function(x,y) paste(x,y,sep=" "))   to understand
  z=outer(x,param,lfun) #A
  # z is a matrix where each x,param is replaced with the function evaluated at those values
  y=apply(z,2,sum)

  # y is a vector made up of the column sums
  # Each y is the log lik for a new parameter value
  plot(param,y,col="Blue",type="l",lwd=2,...)
  # which gives the index for the value of y == max.
  # there could be a max between two values of the parameter, therefore 2 indices
  # the first max will take the larger indice
  i=max(which(y==max(y))) #B
  abline(v=param[i],lwd=2,col="Red")

  # plots a nice point where the max lik is
  points(param[i],y[i],pch=19,cex=1.5,col="Black")
  axis(3,param[i],round(param[i],2))
  #check slopes. If it is a max the slope shoud change sign from + to
  # We should get three + and two -vs
  ifelse(i-3>=1 & i+2<=np, slope<-(y[(i-2):(i+2)]-y[(i-3):(i+1)])/(param[(i-2):(i+2)]-param[(i-3):(i+1)]),slope<-"NA")
  return(list(i=i,parami=param[i],yi=y[i],slope=slope))
}
