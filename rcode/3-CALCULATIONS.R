# ARIMA COEFFICIENT DETERMINATION
for(i in 1:6){
  for(j in 1:6){
    erer <- arima(drealstock[-1], order = c(i,0,j))
    cat("ARMA(",i,j,") = ", erer$aic, "\n")
  }
}



#########################################################################################






# FLAT

tmpsec <- subset(aktug, educ == 2)
for(satir in 1:nrow(tmpsec)){tmpsec$income[satir] <- tmpsec$income[satir]/cpiann$V2[tmpsec$year[satir]-2002]}
tmpsec <- subset(tmpsec, age<=65 & age>25)
tmpsec <- aggregate(tmpsec, FUN = median, by = list(tmpsec$age))
tmpsec <- tmpsec[c("age", "income")]
tmpsec$d40 <- ifelse(tmpsec$age<=40, 1, 0)
tmpsec$d55 <- ifelse(tmpsec$age>40&tmpsec$age<=55, 1, 0)
tmpsec$dincome <- c(NA,diff(log(tmpsec$income)))
#pdf(file = "~/Dropbox/research/mathesis/tex/figs/heterwage.pdf")
plot(tmpsec$income, ylim=c(30,200), col="green", xlab="age", ylab="real wages")


xxx <- lm(dincome ~ d40 + d55, data=tmpsec)
summary(xxx)
#lines(cumprod(c(tmpsec[1,2],xxx$fitted.values + 1)))
lines(rep(tmpsec[1,2],40))

# MODERATE

tmpsec <- subset(aktug, educ == 7)
for(satir in 1:nrow(tmpsec)){tmpsec$income[satir] <- tmpsec$income[satir]/cpiann$V2[tmpsec$year[satir]-2002]}
tmpsec <- subset(tmpsec, age<=65 & age>=25)
tmpsec <- aggregate(tmpsec, FUN = median, by = list(tmpsec$age))
tmpsec <- tmpsec[c("age", "income")]
tmpsec$d40 <- ifelse(tmpsec$age<=40, 1, 0)
tmpsec$d55 <- ifelse(tmpsec$age>40&tmpsec$age<=55, 1, 0)
tmpsec$dincome <- c(NA,diff(log(tmpsec$income)))
points(tmpsec$income, col="blue")


xxx <- lm(dincome ~ d40 + d55, data=tmpsec)
summary(xxx)
lines(cumprod(c(tmpsec[1,2],xxx$fitted.values + 1)))

# STEEP
tmpsec <- subset(aktug, educ == 10)
for(satir in 1:nrow(tmpsec)){tmpsec$income[satir] <- tmpsec$income[satir]/cpiann$V2[tmpsec$year[satir]-2002]}
tmpsec <- subset(tmpsec, age<=63 & age>=25)
tmpsec <- aggregate(tmpsec, FUN= median, by = list(tmpsec$age))
tmpsec <- tmpsec[c("age", "income")]
tmpsec$d40 <- ifelse(tmpsec$age<=40, 1, 0)
tmpsec$d55 <- ifelse(tmpsec$age>40&tmpsec$age<=55, 1, 0)
tmpsec$dincome <- c(NA,diff(log(tmpsec$income)))
points(tmpsec$income, col="red")
#plot(tmpsec$income)

xxx <- lm(dincome ~ d40 + d55, data=tmpsec)
summary(xxx)
lines(cumprod(c(tmpsec[1,2],xxx$fitted.values + 1)))
#dev.off()
legend(x="topleft", y=0,legend = c("steep", "moderate", "high"), fill = c("red", "blue", "green"), lty = c(1,1), cex=1.5)




#########################################################################################

#dw/dt determine:
pdf(file = "Dropbox/research/tex/figs/heterwage.pdf")
tmpeduc <- subset(aktug, aktug$educ==11 & aktug$age<61)
tmpeduc <- aggregate(x=tmpeduc, FUN=median, by=list(tmpeduc$age))[c("age","income")]
plot(tmpeduc, type="l", col="red")
x <- c(rep(1.065,35), rep(1.02,10), rep(1,15))
x[1] <- tmpeduc$income[1]
x[1:35] <- cumprod(x[1:35])
x[35:45] <- cumprod(x[35:45])
x[45:60] <- cumprod(x[45:60])
param_mu_w_steep <- x
lines(x, type="l", col="blue")


tmpeduc <- subset(aktug, aktug$educ==7 & aktug$age<61)
tmpeduc <- aggregate(x=tmpeduc, FUN=median, by=list(tmpeduc$age))[c("age","income")]
lines(tmpeduc, type="l", col="red")
x <- c(rep(1.035,35), rep(1.03,10), rep(1,15))
x[1] <- tmpeduc$income[1]
x[1:35] <- cumprod(x[1:35])
x[35:45] <- cumprod(x[35:45])
x[45:60] <- cumprod(x[45:60])
param_mu_w_moderate <- x
lines(x, type="l", col="blue")


tmpeduc <- subset(aktug, aktug$educ==2 & aktug$age<61)
tmpeduc <- aggregate(x=tmpeduc, FUN=median, by=list(tmpeduc$age))[c("age","income")]
lines(tmpeduc, type="l", col="red")
x <- c(rep(1,35), rep(1,10), rep(1,15))
x[1] <- mean(tmpeduc$income)
x[1:35] <- cumprod(x[1:35])
x[35:45] <- cumprod(x[35:45])
x[45:60] <- cumprod(x[45:60])
param_mu_w_flat <- x
lines(x, type="l", col="blue")
legend(x="topleft", y=0,legend = c("actual", "parameterized"), fill = c("red", "blue"), lty = c(1,1), cex = 1.5)
dev.off()

pdf(file = "Dropbox/research/tex/figs/heterwageless.pdf")
x <- c(rep(1.065,35), rep(1.02,10), rep(1,15))
x[1] <- tmpeduc$income[1]
x[1:35] <- cumprod(x[1:35])
x[35:45] <- cumprod(x[35:45])
x[45:60] <- cumprod(x[45:60])
plot(x, type="l", col="red", xlab = "age", ylab = "wage")
x <- c(rep(1.035,35), rep(1.03,10), rep(1,15))
x[1] <- tmpeduc$income[1]
x[1:35] <- cumprod(x[1:35])
x[35:45] <- cumprod(x[35:45])
x[45:60] <- cumprod(x[45:60])
lines(x, type="l", col="orange")
x <- c(rep(1,35), rep(1,10), rep(1,15))
x[1] <- mean(tmpeduc$income)
x[1:35] <- cumprod(x[1:35])
x[35:45] <- cumprod(x[35:45])
x[45:60] <- cumprod(x[45:60])
lines(x, type="l", col="green")
legend(x="topleft", y=0,legend = c("steep", "moderate", "flat"), fill = c("red", "orange", "green"), lty = c(1,1), cex = 1.5)
dev.off()

pdf(file = "Dropbox/research/tex/figs/survival.pdf")
plot(surv, xlab = "age", ylab = "survival probability")
dev.off()

#finding rho_ws
tmpindex <- c("1","6","7","8","9","10","11","12","14")
tmpsec <- subset(aktug, aktug$sector==tmpindex[7] | aktug$sector==tmpindex[8] | aktug$sector==tmpindex[9])
tmpsec <- aggregate(x=tmpsec, FUN=mean, by=list(tmpsec$year))[c("year","income")]
tmpsec <- na.spline(object = tmpsec$income, x=tmpsec$year, xout = yigit$date)
tmpsec <- diff(tmpsec,12)
print(cor(tmpsec, df$dstock))
tmpsec <- subset(aktug, aktug$sector==tmpindex[5] | aktug$sector==tmpindex[6]  | aktug$sector==tmpindex[4])
tmpsec <- aggregate(x=tmpsec, FUN=mean, by=list(tmpsec$year))[c("year","income")]
tmpsec <- na.spline(object = tmpsec$income, x=tmpsec$year, xout = yigit$date)
tmpsec <- diff(tmpsec,12)
print(cor(tmpsec, df$dstock))


