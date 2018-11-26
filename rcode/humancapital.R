set.seed(1)
HH <- rep(0,param_T); HH[1] <- 200000
HH[2:30] <- 1+rnorm(29,param_mu_h,param_sig_h)
HH <- cumprod(HH)
plot(c(26:65),HH)


#individualized portfolios
#LL <- rep(0,param_T); LL[1] <- w2[12,]$income*1.3
pdf(file = "Dropbox/research/mathesis/tex/figs/heterwageless.pdf")
LL_steep <- cumprod(c(100, rep(1.022,14), rep(1.012,15), rep(1.015,10)))
LL_moderate <- cumprod(c(50, rep(1.038,14), rep(1.014,15), rep(1,10)))
LL_flat <- cumprod(c(50, rep(1,14), rep(1,15), rep(1,10)))
plot(LL_steep, type="l", col="red", xlab = "age", ylab = "wage", ylim = c(50,200))
lines(LL_moderate, type="l", col="orange")
lines(LL_flat, type="l", col="green")
legend(x="topleft", y=0,legend = c("steep", "moderate", "flat"), fill = c("red", "orange", "green"), lty = c(1,1), cex = 1.5)
dev.off()
# x[1] <- 100
# x[1:35] <- cumprod(x[1:35])
# x[35:45] <- cumprod(x[35:45])
# x[45:65] <- cumprod(x[45:65])
# x <- x[26:65]
#LL_steep <- x
# x[1] <- 50
# x[1:35] <- cumprod(x[1:35])
# x[35:45] <- cumprod(x[35:45])
# x[45:65] <- cumprod(x[45:65])
# x <- x[26:65]
#LL_moderate <- x
# x[1] <- 50
# x[1:35] <- cumprod(x[1:35])
# x[35:45] <- cumprod(x[35:45])
# x[45:65] <- cumprod(x[45:65])
#x <- x[26:65]
#LL_flat <- x




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


pdf(file="~/Dropbox/research/mathesis/tex/figs/humancapital.pdf")
plot(c(25:64), L_steep, type="l", xlab="age", ylab="human capital", ylim=c(50, 3300),col="red3")
lines(c(25:64), L_moderate, col="orange2")
lines(c(25:64), L_flat, col="green3")
legend(x = "topright", y =0, legend = c("steep","moderate","flat"), fill=c("red3","orange2","green3"), cex=1.4)
dev.off()
