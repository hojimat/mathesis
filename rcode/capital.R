L_steep <- rep(0,T)
L_moderate <- rep(0,T)
L_flat <- rep(0,T)
for(i in 1:param_T){
  ans <- 0
  anm <- 0
  anf <- 0
  for(j in i:param_T){
    tt <- j-i
    ans <- ans + LL_steep[j]/((1+param_r_f)^tt)
    anm <- anm + LL_moderate[j]/((1+param_r_f)^tt)
    anf <- anf + LL_flat[j]/((1+param_r_f)^tt)
  }
  L_steep[i] <- ans
  L_moderate[i] <- anm
  L_flat[i] <- anf
}

