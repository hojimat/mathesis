mcmrk15 <- read.csv("../misc/mcmrk15.csv")[,-1]
mcmrk3 <- read.csv("../misc/mcmrk3.csv")[,-1]
mcmrk5 <- read.csv("../misc/mcmrk5.csv")[,-1]
mcmrk10 <- read.csv("../misc/mcmrk10.csv")[,-1]

mcsto15 <- read.csv("../misc/mcsto15.csv")[,-1]
mcsto3 <- read.csv("../misc/mcsto3.csv")[,-1]
mcsto5 <- read.csv("../misc/mcsto5.csv")[,-1]
mcsto10 <- read.csv("../misc/mcsto10.csv")[,-1]

mccoc15 <- read.csv("../misc/mccoc15.csv")[,-1]
mccoc3 <- read.csv("../misc/mccoc3.csv")[,-1]
mccoc5 <- read.csv("../misc/mccoc5.csv")[,-1]
mccoc10 <- read.csv("../misc/mccoc10.csv")[,-1]

mcbnk15 <- read.csv("../misc/mcbnk15.csv")[,-1]
mcbnk3 <- read.csv("../misc/mcbnk3.csv")[,-1]
mcbnk5 <- read.csv("../misc/mcbnk5.csv")[,-1]
mcbnk10 <- read.csv("../misc/mcbnk10.csv")[,-1]

mcbod15 <- read.csv("../misc/mcbod15.csv")[,-1]
mcbod3 <- read.csv("../misc/mcbod3.csv")[,-1]
mcbod5 <- read.csv("../misc/mcbod5.csv")[,-1]
mcbod10 <- read.csv("../misc/mcbod10.csv")[,-1]

mcmnk15 <- read.csv("../misc/mcmnk15.csv")[,-1]
mcmnk3 <- read.csv("../misc/mcmnk3.csv")[,-1]
mcmnk5 <- read.csv("../misc/mcmnk5.csv")[,-1]
mcmnk10 <- read.csv("../misc/mcmnk10.csv")[,-1]

mcmnkh15 <- read.csv("../misc/mcmnkh15.csv")[,-1]
mcmnkh3 <- read.csv("../misc/mcmnkh3.csv")[,-1]
mcmnkh5 <- read.csv("../misc/mcmnkh5.csv")[,-1]
mcmnkh10 <- read.csv("../misc/mcmnkh10.csv")[,-1]

monaco15 <- cbind(mcmrk15, mcsto15, mccoc15, mcbnk15, mcbod15, mcmnk15, mcmnkh15)
monaco3 <- cbind(mcmrk3, mcsto3, mccoc3, mcbnk3, mcbod3, mcmnk3, mcmnkh3)
monaco5 <- cbind(mcmrk5, mcsto5, mccoc5, mcbnk5, mcbod5, mcmnk5, mcmnkh5)
monaco10 <- cbind(mcmrk10, mcsto10, mccoc10, mcbnk10, mcbod10, mcmnk10, mcmnkh10)


round(matrix(colMeans(monaco15),7,9,byrow=T),0)
round(matrix(colMeans(monaco3),7,9,byrow=T),0)
round(matrix(colMeans(monaco5),7,9,byrow=T),0)
round(matrix(colMeans(monaco10),7,9,byrow=T),0)

round(crrautil(round(matrix(colMeans(monaco15),7,9,byrow=T),0)/13.73,1.5),6)
round(crrautil(round(matrix(colMeans(monaco3),7,9,byrow=T),0)/13.73,3),6)
round(crrautil(round(matrix(colMeans(monaco5),7,9,byrow=T),0)/13.73,5),6)
round(crrautil(round(matrix(colMeans(monaco10),7,9,byrow=T),0)/13.73,10),15)





# strategies <- c("mrk","sto","coc","bnk","bod","mnk","mnkh")
# wages <- c("Lst", "Lmd", "Lft")
# corrs <- c("rho hi", "rho mod", "rho lo")
# 
# for(i in 1:6){
#   for(j in (i+1):7){
#     for(k in 1:9){
#       cat(strategies[i],">",strategies[j],"| ", wages[ceiling(k/3)], ": ")
#       print(mean(crrautil(monaco15[,9*(i-1) + k]/13.73,10) >= crrautil(monaco15[,9*(j-1)+k]/13.73,10),na.rm = T))
#       #print(mean( ( crrautil( (monaco3[,(9*(i-1) + k)]/13.73) , 3 ) - crrautil( (monaco3[,(9*(j-1)+k)]/13.73) , 3 ) ) / crrautil((monaco3[,(9*(j-1)+k)]/13.73) , 3 ), na.rm = T))
#     }
#   }
# }