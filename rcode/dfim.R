#import raw income data from aktug and yigit
aktug <- read.csv(file="/home/ravshan/Dropbox/research/mathesis/data/aktug/aktug.csv", header = TRUE)
yigit <- read.csv(file="/home/ravshan/Dropbox/research/mathesis/data/yigit.csv", header=TRUE)
cpi   <- read.csv(file="/home/ravshan/Dropbox/research/mathesis/data/cpi.csv", header=FALSE)
cpiann <- read.csv(file="/home/ravshan/Dropbox/research/mathesis/data/cpiann.csv", header=FALSE)
surv  <- read.csv(file="/home/ravshan/Dropbox/research/mathesis/data/survival.csv", header=TRUE)[,2]
surv  <- cumprod(surv)


yigit$date <- as.yearmon(yigit$date)
wage <- aggregate(x = aktug, FUN = mean, by = list(aktug$year))[c("year", "income")]
w2 <- aggregate(x = aktug, FUN = median, by = list(aktug$age))[c("age", "income")]


wage <-na.spline(object = wage$income, x=wage$year, xout = yigit$date)
#wage <- na.locf(object = wage$income, x=wage$year, xout = yigit$date)



df <- yigit
df$wage <- wage
df$cpi <- cpi$V3

#df$dhouse <- append(diff(df$house,12)/df$house[1:132],rep(NA,12),0)
#df$dstock <- append(diff(df$stock,12)/df$stock[1:132],rep(NA,12),0)
#df$dwage <- append(diff(df$wage,12)/df$wage[1:132],rep(NA,12),0)
df$dstock <- append(diff(log(df$stock)),NA,0)
df$dhouse <- append(diff(log(df$house)),NA,0)
df$dwage <- append(diff(log(df$wage)),NA,0)
df$inf <- append(diff(df$cpi, 12)/df$cpi[1:132],rep(NA,12),0)
df <- df[-c(1:12),]

realstock <- df$stock/df$cpi
drealstock <- append(diff(log(realstock)),NA,0)
adf.test(drealstock[-1])
wewe <- arima(drealstock[-1],c(2,0,2))
param_mu_ss <- as.data.frame(forecast(wewe, 200))[200,1]
param_sig_ss <- as.data.frame(forecast(wewe, 200))[200,3] - param_mu_ss
#pdf(file="~/Dropbox/research/mathesis/tex/figs/bistdiff.pdf")
plot(df$date[-1], drealstock[-1], type="l", col='red3', xlab="months", ylab="real stock returns")
#dev.off()


realwage <- df$wage/df$cpi
drealwage <- append(diff(log(realwage)),NA,0)
adf.test(drealwage[-1])
dfdf <- arima(drealwage[-1], c(5,0,2) )
param_sig_yy <- as.data.frame(forecast(dfdf,200))[200,1] - as.data.frame(forecast(dfdf,200))[200,2]
#pdf(file="~/Dropbox/research/mathesis/tex/figs/wagediff.pdf")
plot(df$date[-1], drealwage[-1], type="l", col='blue3', xlab="months", ylab="real wage growth")
#dev.off()


realhouse <- df$house/df$cpi
drealhouse <- append(diff(log(realhouse)),NA,0)
adf.test(drealhouse[-1], k = 3)
asas <- arima(drealhouse[-1],c(1,0,1))
plot(df$date[-1], drealhouse[-1], type='l', col='red3', xlab='months', ylab='real housing returns ')
param_mu_hh <- as.data.frame(forecast(asas, 200))[200,1]
param_sig_hh <- as.data.frame(forecast(asas, 200))[200,3] - param_mu_hh
#pdf(file="~/Dropbox/research/mathesis/tex/figs/reidindiff.pdf")
plot(df$date[-1], drealhouse[-1], type="l", col='green3', xlab="months", ylab="real housing returns")
#dev.off()

plot(drealstock, type='l', col='blue')
lines(drealhouse, type='l', col='red')
lines(drealwage, type='l', col='green')