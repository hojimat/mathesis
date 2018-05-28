# Munk solution with housing

fc$munkhouse_steep_low <- rep(0,param_T)
fc$munkhouse_steep_low[1] <- LL_steep[1]*0.03
fc$munkhouse_steep_mod <- rep(0,param_T)
fc$munkhouse_steep_mod[1] <- LL_steep[1]*0.03
fc$munkhouse_steep_hi <- rep(0,param_T)
fc$munkhouse_steep_hi[1] <- LL_steep[1]*0.03

fc$munkhouse_moderate_low <- rep(0,param_T)
fc$munkhouse_moderate_low[1] <- LL_moderate[1]*0.03
fc$munkhouse_moderate_mod <- rep(0,param_T)
fc$munkhouse_moderate_low[1] <- LL_moderate[1]*0.03
fc$munkhouse_moderate_hi <- rep(0,param_T)
fc$munkhouse_moderate_low[1] <- LL_moderate[1]*0.03

fc$munkhouse_flat_low <- rep(0,param_T)
fc$munkhouse_flat_low[1] <- LL_flat[1]*0.03
fc$munkhouse_flat_mod <- rep(0,param_T)
fc$munkhouse_flat_low[1] <- LL_flat[1]*0.03
fc$munkhouse_flat_hi <- rep(0,param_T)
fc$munkhouse_flat_low[1] <- LL_flat[1]*0.03

