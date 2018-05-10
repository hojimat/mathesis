munk <- function(t, rhowsi, dwi){
  ans <- 0
  ans <- ans + ( (param_mu_s - param_r_f) / (param_gamma * (sig_s^2) ) ) * (LL[t-1]+HH[t-1]) / FF[t-1]
  ans <- ans - ( rhows / (sig_s^2) ) * LL[t-1] / FF[t-1]
  ans <- ans - ( rho_hs / (sig_s^2) ) * HH[t-1] / FF[t-1]
  return(ans)
}
