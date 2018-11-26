#param_house <- rnorm(39, param_mu_h, param_sig_h)
#param_stock <- rnorm(39, param_mu_s, param_sig_s)
#plot(param_stock, type="l")
#param_house <- rep(param_mu_h, 39)
#param_gamma <- 1.5


# Munk solution with housing

# kfc: fin capital
kfc <- data.frame(matrix(0, param_T, 9))
kfc[1,c(1,2,3)] <- LL_steep[1]*0.03
kfc[1,c(4,5,6)] <- LL_moderate[1]*0.03
kfc[1,c(7,8,9)] <- LL_flat[1]*0.03

# md: stock share
md <- data.frame(matrix(0, 39, 9))
mdf <- data.frame(matrix(0, 39, 9))

# bk: house share
bk <- data.frame(matrix(0, 39, 9))
bkf <- data.frame(matrix(0, 39, 9))
#sl sm sh ml mm mh fl fm fh

for(i in 2:40){
  for(j in 1:9){
    lx <- 0;if(j <= 3){lx <- L_steep}else if(j>3&j<=6){lx <- L_moderate}else if(j>6&j<=9){lx <- L_flat}
    rx <- 0;if(j%%3 == 1){rx <- param_rho_ws_low}else if(j%%3==2){rx <- param_rho_ws_medium}else if(j%%3==0){rx <- param_rho_ws_high}
    md[i-1,j] <- munk(t = i-1, dwi = lx, dfi = kfc[,j], rhowsi = rx, rhowhi = param_rho_hw, house=TRUE)$pis
    bk[i-1,j] <- munk(t = i-1, dwi = lx, dfi = kfc[,j], rhowsi = rx, rhowhi = param_rho_hw, house=TRUE)$pih
    
    if(bk[i-1,j] <= 0){
      mdf[i-1,j] <- min(1,md[i-1,j])
      bkf[i-1,j] <- 0
    }else if(bk[i-1,j] > 0 & md[i-1,j] + bk[i-1,j] > 1){
      mdf[i-1,j] <- md[i-1,j] / (md[i-1,j] + bk[i-1,j])
      bkf[i-1,j] <- bk[i-1,j] / (md[i-1,j] + bk[i-1,j])
    }else if(bk[i-1,j] > 0 & md[i-1,j] + bk[i-1,j] < 1){
      mdf[i-1,j] <- md[i-1,j]
      bkf[i-1,j] <- bk[i-1,j]
    }
    
    kfc[i,j] <- (kfc[i-1,j] + lx[i-1]*0.03)*(1 + param_stock[i-1]*mdf[i-1,j] + param_house[i-1]*bkf[i-1,j] + param_r_f*(1-mdf[i-1,j] - bkf[i-1,j]))        
  }
}

#############################################
fc$munkhouse_steep_low <- kfc[,1]
fc$munkhouse_steep_mod <- kfc[,2]
fc$munkhouse_steep_hi <- kfc[,3]
j
fc$munkhouse_moderate_low <- kfc[,4]
fc$munkhouse_moderate_mod <- kfc[,5]
fc$munkhouse_moderate_hi <- kfc[,6]

fc$munkhouse_flat_low <- kfc[,7]
fc$munkhouse_flat_mod <- kfc[,8]
fc$munkhouse_flat_hi <- kfc[,9]
############################################





















param_gamma <- 1.5
# stock shares for every option
latmrk <- data.frame(matrix(0.17, param_T, 3*3*3))
latsto <- data.frame(matrix(c(75:36)/100, param_T, 3*3*3))
latcoc <- data.frame(matrix(c(rep(100,15), seq(100, 52.5, -2.5),rep(50,5))/100, param_T, 3*3*3))
latbnk <- data.frame(matrix(0.6, param_T, 3*3*3))

latbod <- data.frame(matrix(0, param_T, 3*3*3))
latmnk <- data.frame(matrix(0, param_T, 3*3*3))
latmnks<- data.frame(matrix(0, param_T, 3*3*3*3))
latmnkh<- data.frame(matrix(0, param_T, 3*3*3*3))

# accrued capital for every option
vosmrk <- data.frame(matrix(0, param_T, 3*3*3))
vossto <- data.frame(matrix(0, param_T, 3*3*3))
voscoc <- data.frame(matrix(0, param_T, 3*3*3))
vosbnk <- data.frame(matrix(0, param_T, 3*3*3))
vosbod <- data.frame(matrix(0, param_T, 3*3*3))
vosmnk <- data.frame(matrix(0, param_T, 3*3*3))
vosmnkh<- data.frame(matrix(0, param_T, 3*3*3*3))

init_capital <- c(rep(LL_steep[1]*0.03,9),rep(LL_moderate[1]*0.03,9),rep(LL_flat[1]*0.03,9))
vosmrk[1,] <- init_capital
vossto[1,] <- init_capital
voscoc[1,] <- init_capital
vosbnk[1,] <- init_capital
vosbod[1,] <- init_capital
vosmnk[1,] <- init_capital
vosmnkh[1,]<- c(rep(LL_steep[1]*0.03,27),rep(LL_moderate[1]*0.03,27),rep(LL_flat[1]*0.03,27))

for(dydt in 1:3){#wage growth STEEP-MOD-FLAT
  for(rhows in 1:3){#stock-wage correlation HIGH-MOD-LOW
    for(confid in 1:3){#confidence interval UP-STD-DOWN
      lex <- 0; if(dydt==1){lex<-LL_steep}else if(dydt==2){lex<-LL_moderate}else if(dydt==3){lex<-LL_flat}
      flex <- 0; if(dydt==1){flex<-L_steep}else if(dydt==2){flex<-L_moderate}else if(dydt==3){flex<-L_flat}
      rex <- 0; if(rhows==1){rex<-param_rho_ws_high}else if(rhows==2){rex<-param_rho_ws_medium}else if(rhows==3){rex<-param_rho_ws_low}
      sex <- 0; if(confid==1){sex<-param_mu_s+param_sig_s}else if(confid==2){sex<-param_mu_s}else if(confid==3){sex<-param_mu_s-param_sig_s}
      
      # set index
      ii <- 9*dydt+3*rhows+confid-12
      
      for(tt in 2:param_T){#time period  
        # set ratios
        latbod[tt-1,ii] <- min(bodie(tt-1,flex,vosbod[,ii]),1)
        latmnk[tt-1,ii] <- max(min(munk(t=tt-1,dwi=flex,dfi=vosmnk[,ii],rhowsi=rex,house = FALSE)$pis,1),0)
        
        # use law of motion
        vosmrk[tt,ii] <- vosmrk[tt-1,ii]*(1+sex*latmrk[tt-1,ii] + param_r_f*(1-latmrk[tt-1,ii])) + lex[tt-1]*0.03
        vossto[tt,ii] <- vossto[tt-1,ii]*(1+sex*latsto[tt-1,ii] + param_r_f*(1-latsto[tt-1,ii])) + lex[tt-1]*0.03
        voscoc[tt,ii] <- voscoc[tt-1,ii]*(1+sex*latcoc[tt-1,ii] + param_r_f*(1-latcoc[tt-1,ii])) + lex[tt-1]*0.03
        vosbnk[tt,ii] <- vosbnk[tt-1,ii]*(1+sex*latbnk[tt-1,ii] + param_r_f*(1-latbnk[tt-1,ii])) + lex[tt-1]*0.03
        vosbod[tt,ii] <- vosbod[tt-1,ii]*(1+sex*latbod[tt-1,ii] + param_r_f*(1-latbod[tt-1,ii])) + lex[tt-1]*0.03
        vosmnk[tt,ii] <- vosmnk[tt-1,ii]*(1+sex*latmnk[tt-1,ii] + param_r_f*(1-latmnk[tt-1,ii])) + lex[tt-1]*0.03
      }
      
      for(hconf in 1:3){
        hex <- 0; if(hconf==1){hex<-param_mu_h+param_sig_h}else if(hconf==2){hex<-param_mu_h}else if(hconf==3){hex<-param_mu_h-param_sig_h}
        # set index
        jj <- 27*dydt+9*rhows+3*confid + hconf - 39
        for(tt in 2:param_T){#time period  
          latmnks[tt-1,jj] <- munk(t=tt-1,dwi=flex,dfi=vosmnkh[,jj],rhowsi=rex,rhowhi=hex,house = TRUE)$pis
          latmnkh[tt-1,jj] <- munk(t=tt-1,dwi=flex,dfi=vosmnkh[,jj],rhowsi=rex,rhowhi=hex,house = TRUE)$pih
          sumlat <- latmnkh[tt-1,jj]+latmnks[tt-1,jj]
          if(latmnkh[tt-1,jj] <= 0){
            latmnks[tt-1,jj] <- min(1,latmnks[tt-1,jj])
            latmnkh[tt-1,jj] <- 0
          }else if(latmnkh[tt-1,jj]>0 & sumlat>1){
            latmnks[tt-1,jj]<-latmnks[tt-1,jj]/sumlat
            latmnkh[tt-1,jj]<-latmnkh[tt-1,jj]/sumlat
          }
          sumlat <- latmnkh[tt-1,jj]+latmnks[tt-1,jj]
          vosmnkh[tt,jj]<- vosmnkh[tt-1,jj]*(1+sex*latmnks[tt-1,jj] + hex*latmnkh[tt-1,jj] + param_r_f*(1-sumlat)) + lex[tt-1]*0.03
        }        
      }
    }
  }
}

pdf("~/Dropbox/research/mathesis/tex/figs/individs.pdf")
plot(0, type="n", ylim=c(0,1), xlim=c(25,63), ylab="fin capital", xlab="age")
for(i in 1:27){
  ltype<-2;if(i%%3==2){ltype<-1}
  coltype1<-"gray";if(i%%3==2){coltype1<-"red"}
  coltype2<-"gray";if(i%%3==2){coltype2<-"blue"}
  lines(c(25:63), latmnk[1:39,i], lty=ltype, col=coltype1)
  lines(c(25:63), latbod[1:39,i], lty=ltype, col=coltype2)
}
legend(x = "bottomleft", y=0, legend = c("Bodie et al.", "Munk"), fill=c("blue", "red3"), cex=1.5)
dev.off()



plot(0, type="n", ylim=c(0,1), xlim=c(25,63), ylab="fin capital", xlab="age")
for(i in 1:81){
  ltype<-3;if(i%%8==5){ltype<-1}
  coltype1<-"gray";if(i%%8==5){coltype1<-"red"}
  lines(c(25:63), latmnkh[1:39,i], lty=ltype, col=coltype1)
}





fc <- data.frame(
  mrk = as.numeric(vosmrk[40,]),
  sto = as.numeric(vossto[40,]),
  coc = as.numeric(voscoc[40,]),
  bnk = as.numeric(vosbnk[40,]),
  bod = as.numeric(vosbod[40,]),
  mnk = as.numeric(vosmnk[40,]),
  mnkh=as.numeric(vosmnkh[40,])
)

#write.csv(fc, "~/Dropbox/research/mathesis/misc/fins.csv")