# FLAT

tmpsec <- subset(aktug, educ == 2)
for(satir in 1:nrow(tmpsec)){tmpsec$income[satir] <- tmpsec$income[satir]/cpiann$V2[tmpsec$year[satir]-2002]}
tmpsec <- subset(tmpsec, age<=65 & age>25)
tmpsec <- aggregate(tmpsec, FUN = median, by = list(tmpsec$age))
tmpsec <- tmpsec[c("age", "income")]
tmpsec$d40 <- ifelse(tmpsec$age<=40, 1, 0)
tmpsec$d55 <- ifelse(tmpsec$age>40&tmpsec$age<=55, 1, 0)
tmpsec$dincome <- c(NA,diff(log(tmpsec$income)))
#pdf(file = "~/Dropbox/research/mathesis/tex/figs/heterwage.pdf")
plot(tmpsec$income, ylim=c(30,200), col="green", xlab="age", ylab="real wages")


xxx <- lm(dincome ~ d40 + d55, data=tmpsec)
summary(xxx)
#lines(cumprod(c(tmpsec[1,2],xxx$fitted.values + 1)))
lines(rep(tmpsec[1,2],40))

# MODERATE

tmpsec <- subset(aktug, educ == 7)
for(satir in 1:nrow(tmpsec)){tmpsec$income[satir] <- tmpsec$income[satir]/cpiann$V2[tmpsec$year[satir]-2002]}
tmpsec <- subset(tmpsec, age<=65 & age>=25)
tmpsec <- aggregate(tmpsec, FUN = median, by = list(tmpsec$age))
tmpsec <- tmpsec[c("age", "income")]
tmpsec$d40 <- ifelse(tmpsec$age<=40, 1, 0)
tmpsec$d55 <- ifelse(tmpsec$age>40&tmpsec$age<=55, 1, 0)
tmpsec$dincome <- c(NA,diff(log(tmpsec$income)))
points(tmpsec$income, col="blue")


xxx <- lm(dincome ~ d40 + d55, data=tmpsec)
summary(xxx)
lines(cumprod(c(tmpsec[1,2],xxx$fitted.values + 1)))

# STEEP
tmpsec <- subset(aktug, educ == 10)
for(satir in 1:nrow(tmpsec)){tmpsec$income[satir] <- tmpsec$income[satir]/cpiann$V2[tmpsec$year[satir]-2002]}
tmpsec <- subset(tmpsec, age<=63 & age>=25)
tmpsec <- aggregate(tmpsec, FUN= median, by = list(tmpsec$age))
tmpsec <- tmpsec[c("age", "income")]
tmpsec$d40 <- ifelse(tmpsec$age<=40, 1, 0)
tmpsec$d55 <- ifelse(tmpsec$age>40&tmpsec$age<=55, 1, 0)
tmpsec$dincome <- c(NA,diff(log(tmpsec$income)))
points(tmpsec$income, col="red")
#plot(tmpsec$income)

xxx <- lm(dincome ~ d40 + d55, data=tmpsec)
summary(xxx)
lines(cumprod(c(tmpsec[1,2],xxx$fitted.values + 1)))
#dev.off()
legend(x="topleft", y=0,legend = c("steep", "moderate", "high"), fill = c("red", "blue", "green"), lty = c(1,1), cex=1.5)