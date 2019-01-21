#set.seed(1334);param_stock <- rnorm(n = 30, mean = param_mu_s, sd = param_sig_s)
#plot(param_stock, type="l")

#set.seed(45345);param_house <- rnorm(n=30, mean=param_mu_h, sd=param_sig_h)
#plot(param_house, type="l")

#param_stock <- rep(param_mu_s,30)
#param_house <- rep(param_mu_h,30)




param_gamma <- 1.5
source("/home/ravshan/Dropbox/research/mathesis/rcode/fincapital.R")
source("/home/ravshan/Dropbox/research/mathesis/rcode/munkhouse_remake.R")
kvn <- fc[40,]

param_gamma <- 3
source("/home/ravshan/Dropbox/research/mathesis/rcode/fincapital.R")
source("/home/ravshan/Dropbox/research/mathesis/rcode/munkhouse_remake.R")
kvn[2,] <- fc[40,]

param_gamma <- 5
source("/home/ravshan/Dropbox/research/mathesis/rcode/fincapital.R")
source("/home/ravshan/Dropbox/research/mathesis/rcode/munkhouse_remake.R")
kvn[3,] <- fc[40,]


param_gamma <- 10
source("/home/ravshan/Dropbox/research/mathesis/rcode/fincapital.R")
source("/home/ravshan/Dropbox/research/mathesis/rcode/munkhouse_remake.R")
kvn[4,] <- fc[40,]

kvn$init[1] <- 1.5
kvn$init[2] <- 3
kvn$init[3] <- 5
kvn$init[4] <- 10

write.csv(x=round(kvn, digits = 0), file = "~/Desktop/fincapital.csv")




# utils

#cpi
basket <- rep(1.084, 35)
basket[1] <- 100
basket <- cumprod(basket)

#util
kkvn <- kvn / 205.29
for(j in kkvn[1,]){
  kv <- rep(j,43)
  kvv <- kv/basket
  print(crrautil(kvv,rave=5))
}

kkvn <- kvn / 205.29
for(j in kkvn[2,]){
  kv <- rep(j,43)
  kvv <- kv/basket
  print(crrautil(kvv,rave=1.5))
}

kkvn <- kvn / 205.29
for(j in kkvn[3,]){
  kv <- rep(j,43)
  kvv <- kv/basket
  print(crrautil(kvv,rave=3))
}

kkvn <- kvn / 205.29
for(j in kkvn[4,]){
  kv <- rep(j,43)
  kvv <- kv/basket
  print(crrautil(kvv,rave=10))
}






# graph fins
pdf(file="~/Dropbox/research/mathesis/tex/figs/fincapital.pdf")
param_stock <- rep(param_mu_s, 39)
source("/home/ravshan/Dropbox/research/mathesis/rcode/fincapital.R")
plot(c(26:65), fc$markowitz, col="red3", type="l", lty=1, xlab="age", ylab="financial capital")
param_stock <- rep(param_mu_s - param_sig_s, 39)
source("/home/ravshan/Dropbox/research/mathesis/rcode/fincapital.R")
lines(c(26:65), fc$markowitz, col="blue", lty=5)
param_stock <- rep(param_mu_s + param_sig_s, 39)
source("/home/ravshan/Dropbox/research/mathesis/rcode/fincapital.R")
lines(c(26:65), fc$markowitz, col="blue", lty=5)
legend(x = "topleft", y = 0, legend = c("average", "confidence intervals"), fill=c("red3","blue"), cex=1.5)
dev.off()