#' @title Finding for the five number of summary, standard deviation, variance for age-adjusted prevalence data
#'
#' @description Compute the five number of summary for the age of the data
#'
#'
#' @param x_range A \code{vector} of dimension 2 used to denote the integration
#' region of interest, i.e. [a, b].
#' @return A \code{list} containing the following attributes:
#' \describe{
#'      \item{summary}{Computing five number of summary which is mean, median, min, max, 1st Quartile, and 3rd Quartile}
#'      \item{sd}{Estimated standard deviation of the estimator}
#'      \item{var}{Estimated variance of the estimator}
#' }
#' @author Group 5
#' @importFrom data.rda
#' @export Summary_Age


age=data[data$`Data Type`=="Age-adjusted Prevalence","Data Value"]

y=age

GroupSummary <- function(y,na.rm=TRUE){
  a=summary(y)
  b=sd(y)
  c=var(y)


  d=c(a,b,c)
  return (d)
}


GroupSummary(age$`Data Value`)
