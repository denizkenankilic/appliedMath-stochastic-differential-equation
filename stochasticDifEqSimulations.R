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


# Ornstein - Uhlenbeck process set.seed(123)
d <- expression (-5 * x)
s <- expression (3.5)
X<-sde.sim (X0 =10, drift=d, sigma=s) par(mfrow=c(2,2))
plot (X, main ="Ornstein-Uhlenbeck") # Cox-Ingersoll-Ross(CIR-1)
set.seed (123)
d <- expression (6-3*x)
s <- expression (2* sqrt(x))
X<-sde.sim(X0 =10 , drift =d, sigma =s)
plot (X, main ="Cox-Ingersoll-Ross")
# Cox-Ingersoll-Ross(CIR-2)
d <- expression(6-3*x)
s <- expression ( 2* sqrt (x) )
s.x <- expression ( 1/ sqrt (x) )
set.seed (123)
X<-sde.sim (X0=10 , drift=d, sigma=s, sigma.x=s.x,
method ="milstein")
plot (X, main ="Cox-Ingersoll- Ross")
# Cox-Ingersoll-Ross(CIR-3)
set.seed(123)
d <- expression ((6-3*x^2-1)/(2*x))
s <- expression (1)
Y<-sde.sim (X0= sqrt(10), drift=d, sigma=s) plot (Y^2, main ="Cox-Ingersoll-Ross")

# Simulations for delta = 0.1 and delta = 0.25
bX <- expression ((5-11*x+6*x^2-x^3)) x0 <- 5
DT <- 0.1
par(mfrow=c(2,3))
set.seed (123)
X <- sde.sim(drift=bX,delta=DT,X0=x0)
plot (X, main="Euler")
set.seed (123)
Y <- sde.sim(drift=bX,method="ozaki",delta=DT,X0=x0) plot (Y, main ="Ozaki")
set.seed (123)
Z <- sde.sim(drift=bX,method="shoji",delta=DT,X0=x0) plot (Z,main ="Shoji-Ozaki")
DT <- 0.25
set.seed(123)
X <- sde.sim(drift=bX,delta=DT,X0=x0)
plot (X, main="Euler")
set.seed (123)
Y <- sde.sim(drift=bX,method="ozaki",delta=DT,X0=x0) plot (Y,main ="Ozaki")
set.seed(123)
Z <- sde.sim(drift=bX,method="shoji",delta=DT,X0=x0) plot (Z, main ="Shoji-Ozaki")


# SDE Simulation Exact Algorithm
set.seed(123)
d <- expression (-4*tanh(2*x))
d.x <- expression (-(4*(2/cosh(2*x)^2)))
A <- function(x) -(0.5+6/4)*log(cosh(2*x))
X0 <- rt (1, df=4)/2
F <- function(x) log(x+sqrt(1+x^2))/2
Y0 <- F(X0)
Y<-sde.sim (method="EA",delta=1/20,X0=Y0,N=500,drift=d,
               drift.x=d.x, A=A, k1=-4,k2 =8)
X <- sinh(Y)
XY<-ts( cbind (X,Y), start =0, delta =1/ 20)
plot (XY , main ="Original scale X vs transformed Y")
