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


#finding rho_ws
tmpsec <- subset(aktug, aktug$sector==tmpindex[7] | aktug$sector==tmpindex[8] | aktug$sector==tmpindex[9])
tmpsec <- aggregate(x=tmpsec, FUN=mean, by=list(tmpsec$year))[c("year","income")]
tmpsec <- na.spline(object = tmpsec$income, x=tmpsec$year, xout = yigit$date)
tmpsec <- diff(tmpsec)
print(cor(tmpsec/df$cpi, df$stock))
tmpsec <- subset(aktug, aktug$sector==tmpindex[5] | aktug$sector==tmpindex[6]  | aktug$sector==tmpindex[4])
tmpsec <- aggregate(x=tmpsec, FUN=mean, by=list(tmpsec$year))[c("year","income")]
tmpsec <- na.spline(object = tmpsec$income, x=tmpsec$year, xout = yigit$date)
tmpsec <- diff(tmpsec)
print(cor(tmpsec/df$cpi, df$stock))




# Params:

#common params
param_mu_s <- 0.07 #10.58% 
param_mu_h <- 0.085
param_sig_s <- 0.13
param_sig_h <- 0.096 #0.01
param_sig_w <- 0.047
param_rho_hs <- cor(df$dhouse/df$cpi, df$dstock/df$cpi)
param_rho_hw <- cor(df$dhouse/df$cpi, df$dwage/df$cpi)
param_beta  <- 0.89
param_r_f <- 0.035
param_gamma <- 1.5
param_R <- 57 #retirement age
param_Y <- 28 #life cycle period
param_T <- param_R-param_Y+1
param_kappa <- 1

#heter params
param_mu_w_flat
param_mu_w_moderate
param_mu_w_steep

param_sig_eps_low <- 0.03
param_sig_eps_medium <- 0.05
param_sig_eps_high <- 0.07








