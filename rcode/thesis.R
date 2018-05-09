rm(list=ls())
library("zoo")
source("/home/ravshan/Dropbox/research/rcode/dfim.R")
source("/home/ravshan/Dropbox/research/rcode/crrautil.R")
source("/home/ravshan/Dropbox/research/rcode/munk.R")
source("/home/ravshan/Dropbox/research/rcode/merton.R")
source("/home/ravshan/Dropbox/research/rcode/names.R")
source("/home/ravshan/Dropbox/research/rcode/param.R")
#source("/home/ravshan/Dropbox/research/rcode/graphs.R")





#default portfolios
stoage <- c(100:1)
bodie  <- rep(100,100)
bodie[40:60] <- 200-2.5*(40:60)
bodie[61:100] <- 50
stoage <- stoage[param_Y:param_R]
bodie <- bodie[param_Y:param_R]
stoage <- stoage/100
bodie <- bodie/100
age <- c(param_Y:param_R)
ahl <- rep(0.5, param_T)
ah0 <- rep(0.3, param_T)
markowitz <- rep(1, param_T)

pdf(file = "Dropbox/research/tex/figs/defaults.pdf")
plot(age, stoage, ylim = c(0.2,1), xlim = c(param_Y,param_R), type="l", col="red", ylab = "stock share")
lines(age, bodie, type="l", col="orange")
lines(age, ahl, type="l", col="green")
lines(age, ah0, type="l", col="purple")
legend(x = "topright", y=0, legend = c("(100-t)%", "(200-2.5t)%", "50%", "30%"), fill = c("red", "orange", "green", "purple"))
dev.off()






#individualized portfolios
LL <- rep(0,param_T); LL[1] <- w2[12,]$income*1.3
pdf(file = "Dropbox/research/tex/figs/heterwageless.pdf")
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











#Series

#financial capital
FF <- rep(0,param_T); FF[1] <- 25000

#human capital
source("/home/ravshan/Dropbox/research/rcode/capital.R")



set.seed(1)
HH <- rep(0,param_T); HH[1] <- 200000
HH[2:30] <- 1+rnorm(29,param_mu_h,param_sig_h)
HH <- cumprod(HH)
plot(c(28:57),HH)