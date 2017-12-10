    GroupSummary <- function(x){
      for (i in x) {
        if(i>0){
          p <-c(summary(x))
          r <- c(p)
        } else {if(i<0){
          n <-c(summary(x))
          r <- c(n)
        } else {stop}
        }
        return(r)
      }
    }

#call


GroupSummary(x)


write.csv(DentalVisitHispanic14_Age, file="DentalVisitHispanic14_Age.csv",row.names = FALSE)


crude=data[data$`Data Type`=="Crude Prevalence",]
age=data[data$`Data Type`=="Age-adjusted Prevalence",]

mm_sd=function(){
  x=crude
  
  a=mean(x)
  b=median(x)
  c=sd
  
}d=c(a,b,c)
return(d)