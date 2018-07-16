crrautil <- function(C, rave=param_gamma){
  delta <- 0.89
  crra <- 0
  for(t in 58:100){
    sum_comp <- ( delta^(t-1-57) ) * surv[t] * (C[t-57]^(1-rave)) / (1-rave)
    crra <- crra + sum_comp
  }
  return(crra)
}
