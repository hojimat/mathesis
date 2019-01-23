# Import Turkey Household Budget Survey
aktug <- read.csv(file="/home/ravshan/Dropbox/research/mathesis/data/aktug/aktug.csv", header = TRUE)

# Import monthly house prices for Istanbul
yigit <- read.csv(file="/home/ravshan/Dropbox/research/mathesis/data/yigit.csv", header=TRUE)

# Import monthly and yearly CPI data
cpi   <- read.csv(file="/home/ravshan/Dropbox/research/mathesis/data/cpi.csv", header=FALSE)
cpiann <- read.csv(file="/home/ravshan/Dropbox/research/mathesis/data/cpiann.csv", header=FALSE)

# Import survival rates for every age
surv  <- read.csv(file="/home/ravshan/Dropbox/research/mathesis/data/survival.csv", header=TRUE)[,2]
surv  <- cumprod(surv)

# Set date variable
yigit$date <- as.yearmon(yigit$date)

# Aggregate wage data by year and age
wage <- aggregate(x = aktug, FUN = mean, by = list(aktug$year))[c("year", "income")]
w2 <- aggregate(x = aktug, FUN = median, by = list(aktug$age))[c("age", "income")]

# Intrapolate wage to monthly data
wage <-na.spline(object = wage$income, x=wage$year, xout = yigit$date)

# Declare main dataframe df
df <- yigit
df$wage <- wage
df$cpi <- cpi$V3

# Define first differences of stock and house prices, and wages
df$dstock <- append(diff(log(df$stock)),NA,0)
df$dhouse <- append(diff(log(df$house)),NA,0)
df$dwage <- append(diff(log(df$wage)),NA,0)
df$inf <- append(diff(df$cpi, 12)/df$cpi[1:132],rep(NA,12),0)
df <- df[-c(1:12),]

# Divide nominal values by CPI to obtain real values
realstock <- df$stock/df$cpi
drealstock <- append(diff(log(realstock)),NA,0)

realwage <- df$wage/df$cpi
drealwage <- append(diff(log(realwage)),NA,0)

realhouse <- df$house/df$cpi
drealhouse <- append(diff(log(realhouse)),NA,0)