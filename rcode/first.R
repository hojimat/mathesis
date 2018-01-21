rm(list=ls()) #clear all variables

require(ggplot2) #include graphs
require(broom)
require(zoo)
library("ctsem")

N=10 #numberd individual profiles
T = 30 #investment period from age 37 to 66


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



################################
## House, Stock, Wage returns ##
################################

sim_num = 2000 #number of simulated scenarios
r_f = 0.1 #risk-free interest rate
mu_s = 0.1 #long-term expected stock appreciation rate
mu_h = 0.1 #long-term expected house appreciation rate
mu_v = 0.1 #long-term expected excess wage return
S = rep(0,T) #stock market index MSCI World Index
H = rep(0,T) #house price index Bank of INternational Settlements BIS
Y = Y #labor wage index IMF average aggregate wage growth rate
sig_s = 0 #volatility of stock
sig_h = 0 #volatility of house
sig_v = 0 #volatility of labor
dW_s = cumsum(rnorm(T), mean=0, sd=1:T) #brownian motion ~ N(0,t)
dW_h = cumsum(rnorm(T), mean=0, sd=1:T) #brownian motion ~ N(0,t)
dW_v = cumsum(rnorm(T), mean=0, sd=1:T) #brownian motion ~ N(0,t)
rho_hs = cor(H,S) #instantaneous corr btw stock and house price
rho_hy = cor(H,Y) #instantaneous corr btw wage and house price
rho_sy = cor(S,Y) #instantaneous corr btw stock price and wage = 0
rho_hy_hat = (rho_hy-rho_hs*rho_sy)/(sqrt(1-rho_hs^2)) #Cholesky decomposed corr
#Characteristic equations:
# how to write continuous equations on R?




#############################
## Individual total wealth ##
#############################











#notes:
#cumsum makes brownian motion;
#stocks are not affected by correlations;
#what is Advies Commission Parameters
