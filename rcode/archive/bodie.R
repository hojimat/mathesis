bodie <- function(t, dwi, dfi){
  ans <- (param_mu_s - param_r_f)/(param_gamma*param_sig_s^2)
  ans <- ans * (1+dwi[t]/dfi[t])
  return(ans)
}


