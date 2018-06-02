crrautil <- function(C, rave=param_gamma){
  delta <- 0.89
  crra <- 0
  for(t in 58:100){
    sum_comp <- ( delta^(t-1-57) ) * surv[t] * (C[t-57]^(1-rave)) / (1-rave)
    crra <- crra + sum_comp
  }
  return(crra)
}


basket <- rep(1.084, 43)
basket[1] <- 100
basket <- cumprod(basket)

for(j in fc[30,]){
  kv <- rep(j/disc,43)
  kvv <- kv/basket
  print(crrautil(kvv,rave=param_gamma))
}