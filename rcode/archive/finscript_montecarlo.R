###########################################################################################
param_gamma <- 1.5
source("/home/ravshan/Dropbox/research/mathesis/rcode/fincapital_montecarlo.R")

write.csv(mcmrk, "~/Dropbox/research/mathesis/misc/mcmrk15.csv")
write.csv(mcsto, "~/Dropbox/research/mathesis/misc/mcsto15.csv")
write.csv(mccoc, "~/Dropbox/research/mathesis/misc/mccoc15.csv")
write.csv(mcbnk, "~/Dropbox/research/mathesis/misc/mcbnk15.csv")
write.csv(mcbod, "~/Dropbox/research/mathesis/misc/mcbod15.csv")
write.csv(mcmnk, "~/Dropbox/research/mathesis/misc/mcmnk15.csv")
write.csv(mcmnkh, "~/Dropbox/research/mathesis/misc/mcmnkh15.csv")

fc15 <- t(data.frame(
  mrk = colMeans(mcmrk),
  sto = colMeans(mcsto),
  coc = colMeans(mccoc),
  bnk = colMeans(mcbnk),
  bod = colMeans(mcbod),
  mnk = colMeans(mcmnk),
  mnkh= colMeans(mcmnkh)
))

