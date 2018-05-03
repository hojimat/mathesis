munk <- function(t, rave=5, rhows=0){
  ans <- 0
  ans <- ans + ( (mu_s - r_f) / (rave * (sig_s^2) ) ) * (LL[t-1]+HH[t-1]) / FF[t-1]
  ans <- ans - ( rhows / (sig_s^2) ) * LL[t-1] / FF[t-1]
  ans <- ans - ( rho_hs / (sig_s^2) ) * HH[t-1] / FF[t-1]
  return(ans)
}
