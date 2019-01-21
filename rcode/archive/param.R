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




# Params:

#common params
#param_mu_s <- 0.232 #0.1058 #mean(df$dstock)
param_mu_s <- (1 + param_mu_ss)^12 - 1
param_mu_h <- (1 + param_mu_hh)^12 - 1
param_sig_s <- param_sig_ss*sqrt(12)
param_sig_h <- param_sig_hh*sqrt(12)
param_sig_w <- param_sig_yy*sqrt(12)
#param_mu_h <- 0.113 #mean(subset(df$dhouse, df$dhouse>0))
#param_sig_s <- 0.36 #0.017 #0.13 #sd(df$dstock)
#param_sig_s <- 0.297
#param_sig_h <- 0.052 #0.01 #sd(subset(df$dhouse]))
#param_sig_w <- 0.056 #sd(df$dwage)
param_rho_hs <- 0.27 #cor(df$dhouse, df$dstock)
param_rho_hw <- 0.35 #cor(df$dhouse, df$dwage)
param_beta  <- 0.89
param_r_f <- 0.12 - 0.09
param_inf <- 0.084
param_gamma <- 1.5
param_R <- 65 #retirement age
param_Y <- 25 #life cycle period
param_T <- param_R-param_Y
param_kappa <- 1

#heter params
param_mu_w_flat
param_mu_w_moderate
param_mu_w_steep

#param_sig_eps_low <- 0.03
#param_sig_eps_medium <- 0.05
#param_sig_eps_high <- 0.07

param_rho_ws_low <- 0
param_rho_ws_medium <- 0.2
param_rho_ws_high <- 0.4


