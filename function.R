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
