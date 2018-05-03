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
stoage <- stoage[Y:R]
bodie <- bodie[Y:R]