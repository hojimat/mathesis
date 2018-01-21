rm(list=ls()) #clear all variables

require(broom)
require(zoo)
library("ctsem")


# Define parameters

r_f <- 0.035 #risk-free interest rate
pi_t <- 0.019 #inflation

mu_s <- 0.07 #long-term expected stock appreciation rate
mu_h <- 0.0113 #long-term expected house appreciation rate
mu_v <- 0.025 #long-term expected excess wage return

sig_s <- 0.2 #volatility of stock
sig_h <- 0.1372 #volatility of house
sig_v <- 0.0527 #volatility of labor

rho_hs <- 0.069 #instantaneous corr btw stock and house price
rho_hy <- 0.305 #instantaneous corr btw wage and house price
rho_sy <- 0 #instantaneous corr btw stock price and wage = 0
rho_hat <- rho_hy/(sqrt(1-rho_hs^2)) #Cholesky decomposed corr

lambda <- 5 #risk aversion coef
R <- 67 #retirement age
F_0 <- 27000 #initial fin. wealth
Y_1 <- 44900 #salary at age 37
H_1 <- 235000 #purchase price of house
kappa <- 1 #loan to value of reverse mortgage
T <- 30 #lifecycle investment period
sig_eps <- 0.05 #idiosyncratic labor income risk



#Characteristic equations:
# how to write continuous equations on R?

S <- rep(0,T) #stock market index MSCI World Index
H <- rep(0,T) #house price index Bank of International Settlements BIS
Y <- Y #labor wage index IMF average aggregate wage growth rate

dW_s <- cumsum(rnorm(T), mean=0, sd=1:T) #brownian motion ~ N(0,t)
dW_h <- cumsum(rnorm(T), mean=0, sd=1:T) #brownian motion ~ N(0,t)
dW_v <- cumsum(rnorm(T), mean=0, sd=1:T) #brownian motion ~ N(0,t)



##########################
## Labor income Process ##
##########################

w = matrix(20,T,N) #career path #I used average wage in thousands of TL
v = rnorm(T) #aggregate shock
eps = matrix(rnorm(T*N),T,N) #idiosyncratic shock
Y = matrix(20,T,N)
for(n in 1:N){
  for(t in 2:T){
    Y[t,n] = Y[t-1,n]*(1+w[t,n]+v[t]+eps[t,n])
  }
}





#notes:
#cumsum makes brownian motion;
#stocks are not affected by correlations;
#what is Advies Commission Parameters
