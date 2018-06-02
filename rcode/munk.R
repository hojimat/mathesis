munk <- function(t, dwi, dfi, rhowsi, rhowhi, rave=param_gamma, house=TRUE){
  pismunk <- 0
  pihmunk <- 0
  
  stocksh <- (param_mu_s - param_r_f) / param_sig_s
  housesh <- (param_mu_h - param_r_f) / param_sig_h
  firstcoef <- rave * (1-param_rho_hs^2)
  secondcoef <- (dwi[t]/dfi[t]) * (param_sig_w / param_sig_s) * (rhowsi - param_rho_hs * rhowhi) / (1-param_rho_hs^2)
  thirdcoef <- (dwi[t]/dfi[t]) * (param_sig_w / param_sig_h) * (rhowhi - param_rho_hs * rhowsi) / (1-param_rho_hs^2)
  fourthcoef <- rave * param_sig_s
  fifthcoef <- (dwi[t]/dfi[t]) * rhowsi * param_sig_w / param_sig_s

  if(house==TRUE){
    pismunk <- stocksh - param_rho_hs * housesh
    pihmunk <- housesh - param_rho_hs * stocksh
  
    pismunk <- pismunk / firstcoef
    pihmunk <- pismunk / firstcoef
  
    pismunk <- pismunk * (1 + dwi[t]/dfi[t]) / param_sig_s
    pihmunk <- pihmunk * (1 + dwi[t]/dfi[t]) / param_sig_h
  
    pismunk <- pismunk - secondcoef
    pihmunk <- pismunk - thirdcoef
  }else if(house==FALSE){
    pismunk <- stocksh / fourthcoef
    pismunk <- pismunk * (1 + dwi[t]/dfi[t])
    pismunk <- pismunk - fifthcoef
  }
  
  results <- list()
  results$pis <- pismunk
  results$pih <- pihmunk
  return(results)
}
