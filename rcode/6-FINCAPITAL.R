# stock shares for every option
latmrk <- data.frame(matrix(0.17, param_T, 3*3))
latsto <- data.frame(matrix(c(75:36)/100, param_T, 3*3))
latcoc <- data.frame(matrix(c(rep(100,15), seq(100, 52.5, -2.5),rep(50,5))/100, param_T, 3*3))
latbnk <- data.frame(matrix(0.6, param_T, 3*3))

latbod <- data.frame(matrix(0, param_T, 3*3))
latmnk <- data.frame(matrix(0, param_T, 3*3))
latmnks<- data.frame(matrix(0, param_T, 3*3))
latmnkh<- data.frame(matrix(0, param_T, 3*3))

# accrued capital for every option
vosmrk <- data.frame(matrix(0, param_T, 3*3))
vossto <- data.frame(matrix(0, param_T, 3*3))
voscoc <- data.frame(matrix(0, param_T, 3*3))
vosbnk <- data.frame(matrix(0, param_T, 3*3))
vosbod <- data.frame(matrix(0, param_T, 3*3))
vosmnk <- data.frame(matrix(0, param_T, 3*3))
vosmnkh<- data.frame(matrix(0, param_T, 3*3))

init_capital <- c(rep(LL_steep[1]*0.03,3),rep(LL_moderate[1]*0.03,3),rep(LL_flat[1]*0.03,3))
vosmrk[1,] <- init_capital
vossto[1,] <- init_capital
voscoc[1,] <- init_capital
vosbnk[1,] <- init_capital
vosbod[1,] <- init_capital
vosmnk[1,] <- init_capital
vosmnkh[1,]<- init_capital

MCNO <- 10000
mcmrk <- data.frame(matrix(0, MCNO, 3*3))
mcsto <- data.frame(matrix(0, MCNO, 3*3))
mccoc <- data.frame(matrix(0, MCNO, 3*3))
mcbnk <- data.frame(matrix(0, MCNO, 3*3))
mcbod <- data.frame(matrix(0, MCNO, 3*3))
mcmnk <- data.frame(matrix(0, MCNO, 3*3))
mcmnkh<- data.frame(matrix(0, MCNO, 3*3))

for(realization in 1:MCNO){
  set.seed(realization); sex <- rnorm(n=param_T,mean=param_mu_s,sd=param_sig_s)
  set.seed(realization+MCNO); hex <- rnorm(n=param_T,mean=param_mu_h,sd=param_sig_h)
  for(dydt in 1:3){#wage growth STEEP-MOD-FLAT
    for(rhows in 1:3){#stock-wage correlation HIGH-MOD-LOW
      lex <- 0; if(dydt==1){lex<-LL_steep}else if(dydt==2){lex<-LL_moderate}else if(dydt==3){lex<-LL_flat}
      flex <- 0; if(dydt==1){flex<-L_steep}else if(dydt==2){flex<-L_moderate}else if(dydt==3){flex<-L_flat}
      rex <- 0; if(rhows==1){rex<-param_rho_ws_high}else if(rhows==2){rex<-param_rho_ws_medium}else if(rhows==3){rex<-param_rho_ws_low}

      # set index
      ii <- 3*dydt+rhows-3
      
      for(tt in 2:param_T){#time period  
        # set ratios
        latbod[tt-1,ii] <- min(bodie(tt-1,flex,vosbod[,ii]),1)
        latmnk[tt-1,ii] <- max(min(munk(t=tt-1,dwi=flex,dfi=vosmnk[,ii],rhowsi=rex,house = FALSE)$pis,1),0)
        
        latmnks[tt-1,ii] <- munk(t=tt-1,dwi=flex,dfi=vosmnkh[,ii],rhowsi=rex,house = TRUE)$pis
        latmnkh[tt-1,ii] <- munk(t=tt-1,dwi=flex,dfi=vosmnkh[,ii],rhowsi=rex,house = TRUE)$pih
        sumlat <- latmnkh[tt-1,ii]+latmnks[tt-1,ii]
        if(latmnkh[tt-1,ii] <= 0){
          latmnks[tt-1,ii] <- min(1,latmnks[tt-1,ii])
          latmnkh[tt-1,ii] <- 0
        }else if(latmnkh[tt-1,ii]>0 & sumlat>1){
          latmnks[tt-1,ii]<-latmnks[tt-1,ii]/sumlat
          latmnkh[tt-1,ii]<-latmnkh[tt-1,ii]/sumlat
        }
        sumlat <- latmnkh[tt-1,ii]+latmnks[tt-1,ii]
        
        
        
        # use law of motion
        vosmrk[tt,ii] <- vosmrk[tt-1,ii]*(1+sex[tt-1]*latmrk[tt-1,ii] + param_r_f*(1-latmrk[tt-1,ii])) + lex[tt-1]*0.03
        vossto[tt,ii] <- vossto[tt-1,ii]*(1+sex[tt-1]*latsto[tt-1,ii] + param_r_f*(1-latsto[tt-1,ii])) + lex[tt-1]*0.03
        voscoc[tt,ii] <- voscoc[tt-1,ii]*(1+sex[tt-1]*latcoc[tt-1,ii] + param_r_f*(1-latcoc[tt-1,ii])) + lex[tt-1]*0.03
        vosbnk[tt,ii] <- vosbnk[tt-1,ii]*(1+sex[tt-1]*latbnk[tt-1,ii] + param_r_f*(1-latbnk[tt-1,ii])) + lex[tt-1]*0.03
        vosbod[tt,ii] <- vosbod[tt-1,ii]*(1+sex[tt-1]*latbod[tt-1,ii] + param_r_f*(1-latbod[tt-1,ii])) + lex[tt-1]*0.03
        vosmnk[tt,ii] <- vosmnk[tt-1,ii]*(1+sex[tt-1]*latmnk[tt-1,ii] + param_r_f*(1-latmnk[tt-1,ii])) + lex[tt-1]*0.03
        vosmnkh[tt,ii]<- vosmnkh[tt-1,ii]*(1+sex[tt-1]*latmnks[tt-1,ii] + hex[tt-1]*latmnkh[tt-1,ii] + param_r_f*(1-sumlat)) + lex[tt-1]*0.03
      }
    }
  }
  
  mcmrk[realization,] <- vosmrk[40,]
  mcsto[realization,] <- vossto[40,]
  mccoc[realization,] <- voscoc[40,]
  mcbnk[realization,] <- vosbnk[40,]
  mcbod[realization,] <- vosbod[40,]
  mcmnk[realization,] <- vosmnk[40,]
  mcmnkh[realization,] <- vosmnkh[40,]
  print(realization)
}

write.csv(mcmrk, "~/Dropbox/research/mathesis/misc/mcmrk10.csv")
write.csv(mcsto, "~/Dropbox/research/mathesis/misc/mcsto10.csv")
write.csv(mccoc, "~/Dropbox/research/mathesis/misc/mccoc10.csv")
write.csv(mcbnk, "~/Dropbox/research/mathesis/misc/mcbnk10.csv")
write.csv(mcbod, "~/Dropbox/research/mathesis/misc/mcbod10.csv")
write.csv(mcmnk, "~/Dropbox/research/mathesis/misc/mcmnk10.csv")
write.csv(mcmnkh, "~/Dropbox/research/mathesis/misc/mcmnkh10.csv")