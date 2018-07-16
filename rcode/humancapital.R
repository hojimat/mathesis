set.seed(1)
HH <- rep(0,param_T); HH[1] <- 200000
HH[2:30] <- 1+rnorm(29,param_mu_h,param_sig_h)
HH <- cumprod(HH)
plot(c(28:57),HH)


#individualized portfolios
LL <- rep(0,param_T); LL[1] <- w2[12,]$income*1.3
pdf(file = "Dropbox/research/mathesis/tex/figs/heterwageless.pdf")
x <- c(rep(1.065,35), rep(1.02,10), rep(1,15))
x[1] <- LL[1]
x[1:35] <- cumprod(x[1:35])
x[35:45] <- cumprod(x[35:45])
x[45:60] <- cumprod(x[45:60])
x <- x[28:57]
LL_steep <- x
plot(x, type="l", col="red", xlab = "age", ylab = "wage", ylim = c(5000,31000))
x <- c(rep(1.035,35), rep(1.03,10), rep(1,15))
x[1] <- LL[1]
x[1:35] <- cumprod(x[1:35])
x[35:45] <- cumprod(x[35:45])
x[45:60] <- cumprod(x[45:60])
x <- x[28:57]
LL_moderate <- x
lines(x, type="l", col="orange")
x <- c(rep(1,35), rep(1,10), rep(1,15))
x[1] <- LL[1]
x[1:35] <- cumprod(x[1:35])
x[35:45] <- cumprod(x[35:45])
x[45:60] <- cumprod(x[45:60])
x <- x[28:57]
LL_flat <- x
lines(x, type="l", col="green")
legend(x="topleft", y=0,legend = c("steep", "moderate", "flat"), fill = c("red", "orange", "green"), lty = c(1,1), cex = 1.5)
dev.off()



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



pdf(file="Dropbox/research/tex/figs/humancapital.pdf")
plot(c(28:57), L_steep, type="l", xlab="age", ylab="human capital", ylim=c(30000,1200000),col="red3")
lines(c(28:57), L_moderate, col="orange2")
lines(c(28:57), L_flat, col="green3")
legend(x = "topright", y =0, legend = c("steep","moderate","flat"), fill=c("red3","orange2","green3"), cex=1.5)
dev.off()
