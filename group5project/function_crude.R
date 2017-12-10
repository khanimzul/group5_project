#divide and combine datas accordingly into two groups; crude and age
#this is for the crude data
#' @title Finding for the five number of summary, standard deviation, variance for crude prevalence data
#'
#' @description Compute the five number of summary for the age and crude of the data
#'
#'
#' @param x_range A \code{vector} of dimension 2 used to denote the integration
#' region of interest, i.e. [a, b].
#' @return A \code{list} containing the following attributes:
#' \describe{
#'      \item{sd}{Estimated standard deviation of the estimator}
#'      \item{var}{Estimated variance of the estimator}
#' }
#' @author Group 5
#' @importFrom data.rda
#' @export Summary_Crude


crude=data[data$ `Data Type` == "Crude Prevalence","Data Value"]
age=data[data$`Data Type`=="Age-adjusted Prevalence","Data Value"]

x=crude


GroupSummary <- function(x,na.rm=TRUE){
      a=summary(x)
      b=sd(x)
      c=var(x)


d=c(a,b,c)
return (d)
}

#run
GroupSummary(crude$`Data Value`)



