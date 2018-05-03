merton <- function(t, rave=5){
  ans <- 0
  ans <- ans + ( (mu_s - r_f) / (rave * (sig_s^2) ) ) * (LL[t]+HH[t]) / FF[t]
  return(ans)
}
