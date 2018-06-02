merton <- function(t, dwi, dfi, rave=param_gamma){
  ans <- 0
  ans <- param_mu_s - param_r_f
  ans <- ans / rave
  ans <- ans / param_sig_s^2
  ans <- ans * (1+dwi[t]/dfi[t])
  return(ans)
}


