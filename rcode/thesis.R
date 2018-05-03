rm(list=ls())
library("zoo")
source("/home/ravshan/Dropbox/research/rcode/crrautil.R")
source("/home/ravshan/Dropbox/research/rcode/munk.R")
source("/home/ravshan/Dropbox/research/rcode/merton.R")
source("/home/ravshan/Dropbox/research/rcode/dfim.R")
source("/home/ravshan/Dropbox/research/rcode/names.R")
source("/home/ravshan/Dropbox/research/rcode/param.R")
source("/home/ravshan/Dropbox/research/rcode/graphs.R")

#series
FF <- rep(0,T); FF[1] <- 27000
LL <- w2[11:41,]$income
HH <- rep(0,T); HH[1] <- 235000




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

pdf(file = "Dropbox/research/tex/figs/defaults.pdf")
plot(age, stoage, ylim = c(0.2,1), xlim = c(param_Y,param_R), type="l", col="red", ylab = "stock share")
lines(age, bodie, type="l", col="orange")
lines(age, ahl, type="l", col="green")
lines(age, ah0, type="l", col="purple")
legend(x = "topright", y=0, legend = c("(100-t)%", "(200-2.5t)%", "50%", "30%"), fill = c("red", "orange", "green", "purple"))
dev.off()