#import raw income data from aktug and yigit
aktug <- read.csv(file="/home/ravshan/Dropbox/research/mathesis/data/aktug/aktug.csv", header = TRUE)
yigit <- read.csv(file="/home/ravshan/Dropbox/research/mathesis/data/yigit.csv", header=TRUE)
cpi   <- read.csv(file="/home/ravshan/Dropbox/research/mathesis/data/cpi.csv", header=FALSE)
surv  <- read.csv(file="/home/ravshan/Dropbox/research/mathesis/data/survival.csv", header=TRUE)[,2]
surv  <- cumprod(surv)


yigit$date <- as.yearmon(yigit$date)
wage <- aggregate(x = aktug, FUN = mean, by = list(aktug$year))[c("year", "income")]
w2 <- aggregate(x = aktug, FUN = median, by = list(aktug$age))[c("age", "income")]


wage <-na.spline(object = wage$income, x=wage$year, xout = yigit$date)
#wage <- na.locf(object = wage$income, x=wage$year, xout = yigit$date)



df <- yigit
df$wage <- wage
df$cpi <- cpi$V3

df$dhouse <- append(diff(df$house,12)/df$house[1:132],rep(NA,12),0)
df$dstock <- append(diff(df$stock,12)/df$stock[1:132],rep(NA,12),0)
df$dwage <- append(diff(df$wage,12)/df$wage[1:132],rep(NA,12),0)
df$inf <- append(diff(df$cpi, 12)/df$cpi[1:132],rep(NA,12),0)
df <- df[-c(1:12),]