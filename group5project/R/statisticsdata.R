#' divide and combine datas accordingly into two groups; crude and age
#' this is for the crude data
#'
#' @title Finding for the five number of summary, standard deviation, variance for crude prevalence data
#'
#' @description Compute the five number summary of the data value of the dataset
#'
#'
#' @param x A \code{vector} of dimension 1 that is used to be calculated its summary
#' @return A \code{list} containing the following attributes:
#' \describe{
#'      \item{sd}{Estimated standard deviation of the estimator}
#'      \item{var}{Estimated variance of the estimator}
#' }
#'
#' @author Group 5
#' @importFrom summary sd var
#' @export
statisticsdata <- function(x){

      result=c(summary(x),sd(x),var(x))

return (result)
}
