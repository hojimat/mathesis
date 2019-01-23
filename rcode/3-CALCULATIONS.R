# ARIMA COEFFICIENT DETERMINATION USING AKAIKE INFORMATION CRITERION
for(i in 1:6){
  for(j in 1:6){
    erer <- arima(drealstock[-1], order = c(i,0,j))
    cat("ARMA(",i,j,") = ", erer$aic, "\n")
  }
}
arima(drealstock[-1], order=c(2,0,2))
#########################################################################################
# APPENDIX B: CALIBRATION OF PARAMETERS

# FLAT
tmpeduc <- subset(aktug, educ == 2)
for(satir in 1:nrow(tmpeduc)){tmpeduc$income[satir] <- tmpeduc$income[satir]/cpiann$V2[tmpeduc$year[satir]-2002]}
tmpeduc <- subset(tmpeduc, age<=65 & age>25)
tmpeduc <- aggregate(tmpeduc, FUN = median, by = list(tmpeduc$age))
tmpeduc <- tmpeduc[c("age", "income")]
tmpeduc$d40 <- ifelse(tmpeduc$age<=40, 1, 0)
tmpeduc$d55 <- ifelse(tmpeduc$age>40&tmpeduc$age<=55, 1, 0)
tmpeduc$dincome <- c(NA,diff(log(tmpeduc$income)))
Ftmpeduc <- tmpeduc
# MODERATE
tmpeduc <- subset(aktug, educ == 7)
for(satir in 1:nrow(tmpeduc)){tmpeduc$income[satir] <- tmpeduc$income[satir]/cpiann$V2[tmpeduc$year[satir]-2002]}
tmpeduc <- subset(tmpeduc, age<=65 & age>=25)
tmpeduc <- aggregate(tmpeduc, FUN = median, by = list(tmpeduc$age))
tmpeduc <- tmpeduc[c("age", "income")]
tmpeduc$d40 <- ifelse(tmpeduc$age<=40, 1, 0)
tmpeduc$d55 <- ifelse(tmpeduc$age>40&tmpeduc$age<=55, 1, 0)
tmpeduc$dincome <- c(NA,diff(log(tmpeduc$income)))
Mtmpeduc <- tmpeduc
# STEEP
tmpeduc <- subset(aktug, educ == 10)
for(satir in 1:nrow(tmpeduc)){tmpeduc$income[satir] <- tmpeduc$income[satir]/cpiann$V2[tmpeduc$year[satir]-2002]}
tmpeduc <- subset(tmpeduc, age<=63 & age>=25)
tmpeduc <- aggregate(tmpeduc, FUN= median, by = list(tmpeduc$age))
tmpeduc <- tmpeduc[c("age", "income")]
tmpeduc$d40 <- ifelse(tmpeduc$age<=40, 1, 0)
tmpeduc$d55 <- ifelse(tmpeduc$age>40&tmpeduc$age<=55, 1, 0)
tmpeduc$dincome <- c(NA,diff(log(tmpeduc$income)))
Stmpeduc <- tmpeduc


Fxx <- lm(dincome ~ d40 + d55, data=Ftmpeduc)
summary(Fxx)
Mxx <- lm(dincome ~ d40 + d55, data=Mtmpeduc)
summary(Mxx)
Sxx <- lm(dincome ~ d40 + d55, data=Stmpeduc)
summary(Sxx)