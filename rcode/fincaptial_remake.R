param_gamma <- 10
# stock shares for every option
latmrk <- data.frame(matrix(0.17, param_T, 3*3*3))
latsto <- data.frame(matrix(c(75:36)/100, param_T, 3*3*3))
latcoc <- data.frame(matrix(c(rep(100,15), seq(100, 52.5, -2.5),rep(50,5))/100, param_T, 3*3*3))
latbnk <- data.frame(matrix(0.6, param_T, 3*3*3))

latbod <- data.frame(matrix(0, param_T, 3*3*3))
latmnk <- data.frame(matrix(0, param_T, 3*3*3))
latmnks<- data.frame(matrix(0, param_T, 3*3*3))
latmnkh<- data.frame(matrix(0, param_T, 3*3*3))

# accrued capital for every option
vosmrk <- data.frame(matrix(0, param_T, 3*3*3))
vossto <- data.frame(matrix(0, param_T, 3*3*3))
voscoc <- data.frame(matrix(0, param_T, 3*3*3))
vosbnk <- data.frame(matrix(0, param_T, 3*3*3))
vosbod <- data.frame(matrix(0, param_T, 3*3*3))
vosmnk <- data.frame(matrix(0, param_T, 3*3*3))
vosmnkh<- data.frame(matrix(0, param_T, 3*3*3))

init_capital <- c(rep(LL_steep[1]*0.03,9),rep(LL_moderate[1]*0.03,9),rep(LL_flat[1]*0.03,9))
vosmrk[1,] <- init_capital
vossto[1,] <- init_capital
voscoc[1,] <- init_capital
vosbnk[1,] <- init_capital
vosbod[1,] <- init_capital
vosmnk[1,] <- init_capital
vosmnkh[1,]<- init_capital

for(dydt in 1:3){#wage growth STEEP-MOD-FLAT
  for(rhows in 1:3){#stock-wage correlation HIGH-MOD-LOW
    for(confid in 1:3){#confidence interval UP-STD-DOWN
      lex <- 0; if(dydt==1){lex<-LL_steep}else if(dydt==2){lex<-LL_moderate}else if(dydt==3){lex<-LL_flat}
      flex <- 0; if(dydt==1){flex<-L_steep}else if(dydt==2){flex<-L_moderate}else if(dydt==3){flex<-L_flat}
      rex <- 0; if(rhows==1){rex<-param_rho_ws_high}else if(rhows==2){rex<-param_rho_ws_medium}else if(rhows==3){rex<-param_rho_ws_low}
      sex <- 0; if(confid==1){sex<-param_mu_s+param_sig_s}else if(confid==2){sex<-param_mu_s}else if(confid==3){sex<-param_mu_s-param_sig_s}
      hex <- 0; if(confid==1){hex<-param_mu_h+param_sig_h}else if(confid==2){hex<-param_mu_h}else if(confid==3){hex<-param_mu_h-param_sig_h}
      
      # set index
      ii <- 9*dydt+3*rhows+confid-12
      
      for(tt in 2:param_T){#time period  
        # set ratios
        latbod[tt-1,ii] <- min(bodie(tt-1,flex,vosbod[,ii]),1)
        latmnk[tt-1,ii] <- max(min(munk(t=tt-1,dwi=flex,dfi=vosmnk[,ii],rhowsi=rex,house = FALSE)$pis,1),0)

        latmnks[tt-1,ii] <- munk(t=tt-1,dwi=flex,dfi=vosmnkh[,ii],rhowsi=rex,rhowhi=hex,house = TRUE)$pis
        latmnkh[tt-1,ii] <- munk(t=tt-1,dwi=flex,dfi=vosmnkh[,ii],rhowsi=rex,rhowhi=hex,house = TRUE)$pih
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
        vosmrk[tt,ii] <- vosmrk[tt-1,ii]*(1+sex*latmrk[tt-1,ii] + param_r_f*(1-latmrk[tt-1,ii])) + lex[tt-1]*0.03
        vossto[tt,ii] <- vossto[tt-1,ii]*(1+sex*latsto[tt-1,ii] + param_r_f*(1-latsto[tt-1,ii])) + lex[tt-1]*0.03
        voscoc[tt,ii] <- voscoc[tt-1,ii]*(1+sex*latcoc[tt-1,ii] + param_r_f*(1-latcoc[tt-1,ii])) + lex[tt-1]*0.03
        vosbnk[tt,ii] <- vosbnk[tt-1,ii]*(1+sex*latbnk[tt-1,ii] + param_r_f*(1-latbnk[tt-1,ii])) + lex[tt-1]*0.03
        vosbod[tt,ii] <- vosbod[tt-1,ii]*(1+sex*latbod[tt-1,ii] + param_r_f*(1-latbod[tt-1,ii])) + lex[tt-1]*0.03
        vosmnk[tt,ii] <- vosmnk[tt-1,ii]*(1+sex*latmnk[tt-1,ii] + param_r_f*(1-latmnk[tt-1,ii])) + lex[tt-1]*0.03
        vosmnkh[tt,ii]<- vosmnkh[tt-1,ii]*(1+sex*latmnks[tt-1,ii] + hex*latmnkh[tt-1,ii] + param_r_f*(1-sumlat)) + lex[tt-1]*0.03
      }
    }
  }
}
# 
# pdf("~/Dropbox/research/mathesis/tex/figs/individuals10.pdf")
# plot(0, type="n", ylim=c(0,1), xlim=c(25,63), ylab="fin capital", xlab="age")
# for(i in 1:27){
#   ltype<-2;if(i%%3==2){ltype<-1}
#   coltype1<-"gray";if(i%%3==2){coltype1<-"red"}
#   coltype2<-"gray";if(i%%3==2){coltype2<-"blue"}
#   lines(c(25:63), latmnk[1:39,i], lty=ltype, col=coltype1)
#   lines(c(25:63), latbod[1:39,i], lty=ltype, col=coltype2)
# }
# legend(x = "bottomleft", y=0, legend = c("Bodie et al.", "Munk"), fill=c("blue", "red3"), cex=1.5)
# dev.off()


pdf("~/Dropbox/research/mathesis/tex/figs/hmunkhouse10.pdf")
plot(0, type="n", ylim=c(0,1), xlim=c(25,63), ylab="fin capital", xlab="age")
for(i in 1:27){
  ltype<-3;if(i%%3==2){ltype<-1}
  coltype1<-"gray";if(i%%3==2){coltype1<-"red"}
  lines(c(25:63), latmnkh[1:39,i], lty=ltype, col=coltype1)
}
dev.off()

pdf("~/Dropbox/research/mathesis/tex/figs/smunkhouse10.pdf")
plot(0, type="n", ylim=c(0,1), xlim=c(25,63), ylab="fin capital", xlab="age")
for(i in 1:27){
  ltype<-3;if(i%%3==2){ltype<-1}
  coltype1<-"gray";if(i%%3==2){coltype1<-"blue"}
  lines(c(25:63), latmnks[1:39,i], lty=ltype, col=coltype1)
}
dev.off()




fc <- data.frame(
  mrk = as.numeric(vosmrk[40,seq(2,27,3)]),
  mrku = as.numeric(vosmrk[40,seq(1,27,3)]),
  mrkd = as.numeric(vosmrk[40,seq(3,27,3)]),
  sto = as.numeric(vossto[40,seq(2,27,3)]),
  stou = as.numeric(vossto[40,seq(1,27,3)]),
  stod = as.numeric(vossto[40,seq(3,27,3)]),
  coc = as.numeric(voscoc[40,seq(2,27,3)]),
  cocu = as.numeric(voscoc[40,seq(1,27,3)]),
  cocd = as.numeric(voscoc[40,seq(3,27,3)]),
  bnk = as.numeric(vosbnk[40,seq(2,27,3)]),
  bnku = as.numeric(vosbnk[40,seq(1,27,3)]),
  bnkd = as.numeric(vosbnk[40,seq(3,27,3)]),
  bod = as.numeric(vosbod[40,seq(2,27,3)]),
  bodu = as.numeric(vosbod[40,seq(1,27,3)]),
  bodd = as.numeric(vosbod[40,seq(3,27,3)]),
  mnk = as.numeric(vosmnk[40,seq(2,27,3)]),
  mnku = as.numeric(vosmnk[40,seq(1,27,3)]),
  mnkd = as.numeric(vosmnk[40,seq(3,27,3)]),
  mnkh = as.numeric(vosmnkh[40,seq(2,27,3)]),
  mnkhu = as.numeric(vosmnkh[40,seq(1,27,3)]),
  mnkhd = as.numeric(vosmnkh[40,seq(3,27,3)])
)

#write.table(format(t(round(fc,0)),big.mark=","), "~/Dropbox/research/mathesis/misc/fins10.csv",sep = "&", eol = "\\\\\n")