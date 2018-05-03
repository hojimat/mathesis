crrautil <- function(C, rave=param_gamma){
  Death <- 110
  delta <- 1
  crra <- 0
  for(t in 1:Death){
    sum_comp <- ( delta^(t-1) ) * surv[t] * (C^(1-rave)) / (1-rave)
    crra <- crra + sum_comp
  }
  return(crra)
}

cec <- function(C, rave=param_gamma){
  BLAH BLAH BLAH
  
}