monaco15 <- cbind(mcmrk15, mcsto15, mccoc15, mcbnk15, mcbod15, mcmnk15, mcmnkh15)
monaco3 <- cbind(mcmrk3, mcsto3, mccoc3, mcbnk3, mcbod3, mcmnk3, mcmnkh3)
monaco5 <- cbind(mcmrk5, mcsto5, mccoc5, mcbnk5, mcbod5, mcmnk5, mcmnkh5)
monaco10 <- cbind(mcmrk10, mcsto10, mccoc10, mcbnk10, mcbod10, mcmnk10, mcmnkh10)



strategies <- c("mrk","sto","coc","bnk","bod","mnk","mnkh")
wages <- c("Lst", "Lmd", "Lft")
corrs <- c("rho hi", "rho mod", "rho lo")

for(i in 1:6){
  for(j in (i+1):7){
    for(k in 1:9){
      cat(strategies[i],">",strategies[j],"| ", wages[ceiling(k/3)], ": ")
      print(mean(crrautil(monaco10[,9*(i-1) + k]/13.73,10) >= crrautil(monaco10[,9*(j-1)+k]/13.73,10),na.rm = T))
      #print(mean( ( crrautil( (monaco3[,(9*(i-1) + k)]/13.73) , 3 ) - crrautil( (monaco3[,(9*(j-1)+k)]/13.73) , 3 ) ) / crrautil((monaco3[,(9*(j-1)+k)]/13.73) , 3 ), na.rm = T))
    }
  }
}


# 
# crrautil(mcmrk15[,1]/13.73,1.5)
# crrautil(mcmrk3[,1]/13.73,3)
# crrautil(mcmrk5[,1]/13.73,5)
# crrautil(mcmrk10[,1]/13.73,10)
# 
# crrautil(mcsto15[,1]/13.73,1.5)
# crrautil(mcsto3[,1]/13.73,3)
# crrautil(mcsto5[,1]/13.73,5)
# crrautil(mcsto10[,1]/13.73,10)
# 
# crrautil(mccoc15[,1]/13.73,1.5)
# crrautil(mccoc3[,1]/13.73,3)
# crrautil(mccoc5[,1]/13.73,5)
# crrautil(mccoc10[,1]/13.73,10)
# 
# crrautil(mcbnk15[,1]/13.73,1.5)
# crrautil(mcbnk3[,1]/13.73,3)
# crrautil(mcbnk5[,1]/13.73,5)
# crrautil(mcbnk10[,1]/13.73,10)
# 
# crrautil(mcbod15[,1]/13.73,1.5)
# crrautil(mcbod3[,1]/13.73,3)
# crrautil(mcbod5[,1]/13.73,5)
# crrautil(mcbod10[,1]/13.73,10)
# 
# crrautil(mcmnk15[,1]/13.73,1.5)
# crrautil(mcmnk3[,1]/13.73,3)
# crrautil(mcmnk5[,1]/13.73,5)
# crrautil(mcmnk10[,1]/13.73,10)
# 
# crrautil(mcmnkh15[,1]/13.73,1.5)
# crrautil(mcmnkh3[,1]/13.73,3)
# crrautil(mcmnkh5[,1]/13.73,5)
# crrautil(mcmnkh10[,1]/13.73,10)
# 
# 
# 
# plot(density(mcmnk3[,1]),ylim=c(0,0.009),col='red')
# lines(density(mcmrk3[,1]),col='green')
# lines(density(mcmnkh3[,1]),col='blue')
# lines(density(mcbnk3[,1]),col='grey')
# lines(density(mcbod3[,1]),col='orange')
# lines(density(mccoc3[,1]),col='black')
# lines(density(mcsto3[,1]),col='purple')


umean15 <- matrix(NA,7,9)
usd15 <- matrix(NA,7,9)

#plot(0,ylim=c(0,170),xlim=c(-0.4,0.001),type='n')
for(i in 1:7){
  for(k in 1:9){
    #lines(density(crrautil(monaco3[,(9*i-9+k)]/13.73,3)),col=sample(colours(),1))
    #cat(strategies[i], "| ", wages[ceiling(k/3)], ": ")
    umean15[i,k] <- round(mean(crrautil(monaco15[,(9*i - 9 + k)]/13.73, 1.5), na.rm=T),3)
    usd15[i,k] <- round(sd(crrautil(monaco15[,(9*i - 9 + k)]/13.73, 1.5), na.rm=T),3)
  }
}

write.csv(umean15,"~/Desktop/umean15.txt")
write.csv(usd15,"~/Desktop/usd15.txt")





umean3 <- matrix(NA,7,9)
usd3 <- matrix(NA,7,9)

for(i in 1:7){
  for(k in 1:9){
    #lines(density(crrautil(monaco3[,(9*i-9+k)]/13.73,3)),col=sample(colours(),1))
    #cat(strategies[i], "| ", wages[ceiling(k/3)], ": ")
    umean3[i,k] <- round(mean(crrautil(monaco3[,(9*i - 9 + k)]/13.73, 3), na.rm=T),3)
    usd3[i,k] <- round(sd(crrautil(monaco3[,(9*i - 9 + k)]/13.73, 3), na.rm=T),3)
  }
}

write.csv(umean3,"~/Desktop/umean3.txt")
write.csv(usd3,"~/Desktop/usd3.txt")


umean5 <- matrix(NA,7,9)
usd5 <- matrix(NA,7,9)

for(i in 1:7){
  for(k in 1:9){
    #lines(density(crrautil(monaco3[,(9*i-9+k)]/13.73,3)),col=sample(colours(),1))
    #cat(strategies[i], "| ", wages[ceiling(k/3)], ": ")
    umean5[i,k] <- round(mean(crrautil(monaco5[,(9*i - 9 + k)]/13.73, 5), na.rm=T),3)
    usd5[i,k] <- round(sd(crrautil(monaco5[,(9*i - 9 + k)]/13.73, 5), na.rm=T),3)
  }
}

write.csv(umean5,"~/Desktop/umean5.txt")
write.csv(usd5,"~/Desktop/usd5.txt")



umean10 <- matrix(NA,7,9)
usd10 <- matrix(NA,7,9)

for(i in 1:7){
  for(k in 1:9){
    #lines(density(crrautil(monaco3[,(9*i-9+k)]/13.73,3)),col=sample(colours(),1))
    #cat(strategies[i], "| ", wages[ceiling(k/3)], ": ")
    umean10[i,k] <- round(mean(crrautil(monaco10[,(9*i - 9 + k)]/13.73, 10), na.rm=T),3)
    usd10[i,k] <- round(sd(crrautil(monaco10[,(9*i - 9 + k)]/13.73, 10), na.rm=T),3)
  }
}

write.csv(umean10,"~/Desktop/umean10.txt")
write.csv(usd10,"~/Desktop/usd10.txt")
