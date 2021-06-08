par(mfrow=c(2,2))
# Ornstein-Uhlenbeck process
set.seed(123)
d <- expression(-5*x)
s <- expression(3.5)
X<-sde.sim(X0=10,drift=d, sigma=s) plot(X,main="Ornstein-Uhlenbeck")
# Multiple trajectories of the O-U process set.seed(123)
X<-sde.sim(X0=10,drift=d, sigma=s, M=3) plot(X,main="Multiple trajectories of O-U")
# Cox-Ingersoll-Ross process
# dXt = (6-3*Xt)*dt + 2*sqrt(Xt)*dWt set.seed(123)
d <- expression( 6-3*x )
s <- expression( 2*sqrt(x) ) X<-sde.sim(X0=10,drift=d, sigma=s) plot(X,main="Cox-Ingersoll-Ross")
# Exact simulation
set.seed(123)
d <- expression(sin(x))
d.x <- expression(cos(x))
A <- function(x) 1-cos(x) X<-sde.sim(method="EA", delta=1/20, X0=0, N=500,
drift=d, drift.x = d.x, A=A) plot(X, main="Periodic drift")
