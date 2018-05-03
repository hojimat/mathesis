#graphs

#house price index reidin
pdf(file = "Dropbox/research/tex/figs/reidin.pdf")
plot(y = df$house, x = df$date, type="l", xlab = "months", ylab = "house price index",
     ann = 1, col="blue")
dev.off()

#stock price index bist30
pdf(file = "Dropbox/research/tex/figs/bist.pdf")
plot(y = df$stock, x = df$date, type="l", xlab = "months", ylab = "stock price index",
     ann = 1, col="red")
dev.off()

#wage series by age
pdf(file = "Dropbox/research/tex/figs/wage2median.pdf")
plot(w2)
dev.off()

#WAGE BY SEC AND YEAR
pdf(file = "Dropbox/research/tex/figs/wage2sec.pdf")
tmpcol <- c("springgreen4","green1","blue","plum1","blueviolet","orange","grey","red1","steelblue")
tmpindex <- c("1","6","7","8","9","10","11","12","14")
for(i in 1:9){
  tmpsec <- subset(aktug, aktug$sector==tmpindex[i])
  tmpsec <- aggregate(x=tmpsec, FUN=median, by=list(tmpsec$year))[c("year","income")]
  if(i==1){
    plot(tmpsec, type="l", ylim=c(0,median(aktug$income)*4), col=tmpcol[i])
  }else{
    lines(tmpsec, type="l", col=tmpcol[i])
  }
}
legend(x="topleft", y=0,legend = seclist, fill = tmpcol, lty = c(1,1), cex = 1)
dev.off()

#WAGE BY EDUC AND AGE
pdf(file = "Dropbox/research/tex/figs/wage2educ.pdf")
tmpindex <- c("1","2","3","5","7","8","9","10","11")
tmpcol <- c("springgreen4", "green1", "blue", "plum1", "violet", "orange", "grey", "red1", "steelblue")
for(i in 1:9){
  tmpsec <- subset(aktug, aktug$educ==tmpindex[i])
  tmpsec <- aggregate(x=tmpsec, FUN=median, by=list(tmpsec$age))[c("age","income")]
  if(i==1){
    plot(tmpsec, type="l", ylim=c(0,max(aktug$income)/10), col=tmpcol[i])
  }else{
    lines(tmpsec, type="l", col=tmpcol[i])
  }
}
legend(x="topleft", y=0,legend = educlist, fill = tmpcol, lty = c(1,1))
dev.off()

#WAGE BY EDUC AND YEAR
pdf(file = "Dropbox/research/tex/figs/wage3educ.pdf")
tmpcol <- c("springgreen4", "green1", "blue", "plum1", "violet", "orange", "grey", "red1", "steelblue")
tmpindex <- c("1","2","3","5","7","8","9","10","11")
for(i in 1:9){
  tmpsec <- subset(aktug, aktug$educ==tmpindex[i])
  tmpsec <- aggregate(x=tmpsec, FUN=median, by=list(tmpsec$year))[c("year","income")]
  if(i==1){
    plot(tmpsec, type="l", ylim=c(0,median(aktug$income)*5), col=tmpcol[i])
  }else{
    lines(tmpsec, type="l", col=tmpcol[i])
  }
}
legend(x="topleft", y=0,legend = educlist, fill = tmpcol, lty = c(1,1))
dev.off()

#ak1 <- subset(aktug, aktug$educ>7 & aktug$educ<11)
#w3 <- aggregate(x=ak1, FUN=mean, by=list(ak1$age))[c("age","income")]
#plot(w3, type="l")

#ak1 <- subset(aktug, aktug$sector==9)
#w3 <- aggregate(x=ak1, FUN=mean, by=list(ak1$year))[c("year","income")]
#lines(w3, type="l")
