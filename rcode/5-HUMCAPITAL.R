LL_steep <- cumprod(c(100, rep(1.022,14), rep(1.012,15), rep(1.015,10)))
LL_moderate <- cumprod(c(50, rep(1.038,14), rep(1.014,15), rep(1,10)))
LL_flat <- cumprod(c(50, rep(1,14), rep(1,15), rep(1,10)))


L_steep <- rep(0,T)
L_moderate <- rep(0,T)
L_flat <- rep(0,T)

for(i in 1:40){
  ans <- 0
  anm <- 0
  anf <- 0
  for(j in i:40){
    tt <- j-i-1
    ans <- ans + LL_steep[j]/((1+param_r_f)^tt)
    anm <- anm + LL_moderate[j]/((1+param_r_f)^tt)
    anf <- anf + LL_flat[j]/((1+param_r_f)^tt)
  }
  L_steep[i] <- ans
  L_moderate[i] <- anm
  L_flat[i] <- anf
}













# x <- c(rep(1.065,40), rep(1.02,15), rep(1,10))
# x[1] <- 100
# x[1:40] <- cumprod(x[1:40]);x[40:55] <- cumprod(x[40:55]);x[55:65] <- cumprod(x[55:65])
# param_mu_w_steep <- x
# 
# x <- c(rep(1.035,40), rep(1.03,15), rep(1,10))
# x[1] <- 50
# x[1:40] <- cumprod(x[1:40]);x[40:55] <- cumprod(x[40:55]);x[55:65] <- cumprod(x[55:65])
# param_mu_w_moderate <- x
# 
# x <- c(rep(1,40), rep(1,15), rep(1,10))
# x[1] <- 50
# x[1:40] <- cumprod(x[1:40]);x[40:55] <- cumprod(x[40:55]);x[55:65] <- cumprod(x[55:65])
# param_mu_w_flat <- x
