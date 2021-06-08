install.packages("sde")
library(sde)

set.seed(123)
par(mfrow=c(2,2))
plot (BM(0,0,1,100))
plot (GBM(1,1,sqrt(0.5),1,100))
plot (BBridge(0,-1,0,1,100))
