

library(MTS)
setwd("D:/")
getwd

###Clase 1.  Introduccion de diseño de graficas

###EJEMPLO 1###################
da=read.table("ejemplo1.txt",header=T)  ## Cargar datos
head(da)
dim(da)
zt=cbind(log(da$gdp),da$rate)  ## Crear series de tiempo
tdx=da[,1]+da[,2]/12  ## Crear el calendario
require(MTS)
colnames(zt)
colnames(zt) <- c("ln(PIB)","T.D")
MTSplot(zt,tdx)  #grafico 


dzt=diffM(zt)   ## primeras diferencias
colnames(dzt) <- c("Crec. PIB","Cambio en la tasa")
MTSplot(dzt,tdx[2:256]) ##  

plot(dzt[,1],dzt[,2],xlab="Crecimiento del PIB",ylab="Cambio en la tasa") ## Obtain Figure 1.3 of the textbook

###EJEMPLO 2###################

hstarts=read.table("m-hstarts-3divisions.txt",header=T)## Vivienda
dim(hstarts)
head(hstarts)

tdx=hstarts[,1]+hstarts[,2]/12
MTSplot(hstarts[,3:5],tdx)  ## 



rate=read.table("m-unemp-states.txt",header=T) ## Load states unemployment rates
dim(rate)

head(rate)

tdx=c(1:429)/12+1976
ym=max(rate)  ## max. tasa de desempleo para escalar los graficos.



plot(tdx,rate[,1],xlab='Year',ylab='Rate',ylim=c(0,ym+1),type='l')
for(i in 2:50){
  lines(tdx,rate[,i],lty=i)
}




if("tseries" %in% rownames(installed.packages()) == FALSE)
{install.packages("tseries")}
if("MTS" %in% rownames(installed.packages()) == FALSE)
{install.packages("MTS")}
if("ppcor" %in% rownames(installed.packages()) == FALSE)
{install.packages("ppcor")}
library(tseries)
library(MTS)
library(ppcor)
library(TSA)
library(forecast)
library(ggplot2)

ms <- as.data.frame(read.csv("D:/WW2a.csv")[,-1])
rownames(ms) <- seq(as.Date("2009/6/1"), by="month",
                    length=90)
x.orig <- ms

##Gráfica del vector original
par(mfrow=c(3,3))
for(i in 1:8){
  plot.ts(x.orig[,i],main=colnames(x.orig)[i],xlab=NULL,
          ylab='Mil. dollars')}


par(mfrow=c(2,3))
for(i in 1:5){
  plot.ts(x.orig[,i],main=colnames(x.orig)[i],xlab=NULL,
          ylab='Mil. dollars')}


plot(seq(as.Date("2009/6/1"), by="month", length=90),x.orig
     [,1],type='l',ylab="Retail Sales",xlab="Date",ylim=c(min(x.orig),max(x.orig)))
for(i in 2:5){
  lines(seq(as.Date("2009/6/1"), by="month", length=90),
        x.orig[,i],type='l',lty=i,lwd=2)
}
legend("topleft",legend=colnames(zt),,lty=1:5)


koopact <- read.csv("WW10.csv",head=TRUE)
attach(koopact)
dat <- as.matrix(cbind(GDP252, GDP253, GDP254, GDP255, GDP256,
                       GDP257, GDP258, GDP259, GDP260, GDP261,
                       GDP266, GDP267, GDP268, GDP269, GDP271,
                       GDP272, GDP274, GDP275, GDP276, GDP277,
                       GDP278, GDP279, GDP280, GDP281, GDP282,
                       GDP284, GDP285, GDP286, GDP287, GDP288,
                       GDP289, GDP290, GDP291, GDP292, IPS11,
                       IPS299, IPS12, IPS13, IPS18, IPS25,
                       IPS32, IPS34, IPS38, IPS43, IPS307, IPS306,
                       CES275, CES277, CES278, CES003, CES006,
                       CES011, CES015, CES017, CES033, CES046,
                       CES048, CES049, CES053, CES088, CES140))

dat2 <- matrix(dat,195*61)
dat3 <- data.frame(y=dat2, group=rep(colnames(dat),
                                     each=195), obs=rep(1:195,61) )
ggplot(data = dat3, aes(x = obs, y = y, group = group)) +
  geom_line() +
  facet_wrap(~ group)

############################



d10 <- as.data.frame(read.csv("WW4a.csv")[,-1])
d10 <- t(t(d10) - colMeans(d10))
rownames(d10) <- seq(as.Date("2016/8/2"), by="day",
                     length=107)
cov(d10)
cor(d10)

##Grafico de series
plot(seq(as.Date("2016/8/2"), by="day", length=107),d10[,1],
     type='l',ylab="Stock Returns",xlab="Day")
for(i in 2:10){
  lines(seq(as.Date("2016/8/2"), by="day", length=107),
        d10[,i],type='l',col=i)
}
legend("topleft",legend=colnames(d10),col=1:10,lty=1)

##################
library(dplyr)

d5 <- as.data.frame(read.csv("WW4b.csv")[,-1])
dim(d5)
rownames(d5) <- seq(as.Date("1986/1/1"), by="month", length=212)
d5a<- select(d5,-X,-Month.1,-E.1,-A.1,-C.1,-H.1,-G.1)
#result <- filter(d5a,d5a$E >= 50)


##Matrices de Covarianza y correlación 
d5 <- t(t(d5a) - colMeans(d5a))
cov(d5)
cor(d5)

##Grafico 

plot(seq(as.Date("1986/1/1"), by="month", length=212),d5a
     [,1],type='l',ylab="Precios del consumidor
Indice (Jan84=100)",xlab="Date",ylim=c(50,260))
for(i in 2:5){
  lines(seq(as.Date("1986/1/1"), by="month",
            length=212),d5a[,i],type='l',lty=i)
}
#legend("topleft",legend=colnames(d5a),,lty=1:5)



#distribuciones multivariadas


library(MASS) # 
par(mfrow=c(1,4))
N = 2500
nu = 3
set.seed(5640)
cov=matrix(c(1,.8,.8,1),nrow=2)
x= mvrnorm(N, mu = c(0,0), Sigma=cov)
w = sqrt(nu/rchisq(N, df=nu))
x = x * cbind(w,w)
plot(x,main="(a)")



set.seed(5640)
cov=matrix(c(1,.8,.8,1),nrow=2)
x= mvrnorm(N, mu = c(0,0), Sigma=cov)
w1 = sqrt(nu/rchisq(N, df=nu))
w2 = sqrt(nu/rchisq(N, df=nu))
x = x * cbind(w1,w2)
plot(x,main="(b)")

set.seed(5640)
cov=matrix(c(1,0,0,1),nrow=2)
x= mvrnorm(N, mu = c(0,0), Sigma=cov)
w1 = sqrt(nu/rchisq(N, df=nu))
w2 = sqrt(nu/rchisq(N, df=nu))
x = x * cbind(w1,w2)
plot(x,main="(c)")

set.seed(5640)
cov=matrix(c(1,0,0,1),nrow=2)
x= mvrnorm(N, mu = c(0,0), Sigma=cov)
w = sqrt(nu/rchisq(N, df=nu))
x = x * cbind(w,w)
plot(x,main="(d)")

################################

#curvas de rendimiento

set.seed(5640)
cov=matrix(c(1,.8,.8,1),nrow=2)
x= mvrnorm(N, mu = c(0,0), Sigma=cov)
w1 = sqrt(nu/rchisq(N, df=nu))
w2 = sqrt(nu/rchisq(N, df=nu))
x = x * cbind(w1,w2)
plot(x,main="(b)")
set.seed(5640)
cov=matrix(c(1,0,0,1),nrow=2)
x= mvrnorm(N, mu = c(0,0), Sigma=cov)
w1 = sqrt(nu/rchisq(N, df=nu))
w2 = sqrt(nu/rchisq(N, df=nu))
x = x * cbind(w1,w2)
plot(x,main="(c)")
set.seed(5640)
cov=matrix(c(1,0,0,1),nrow=2)
x= mvrnorm(N, mu = c(0,0), Sigma=cov)
w = sqrt(nu/rchisq(N, df=nu))
x = x * cbind(w,w)
plot(x,main="(d)")

library(fEcofin)

plot(mk.maturity[,1],mk.zero2[5,2:56],type="l",
     xlab="maturity",ylab="yield")
lines(mk.maturity[,1],mk.zero2[6,2:56],lty=2,type="l")
lines(mk.maturity[,1],mk.zero2[7,2:56],lty=3,type="l")
lines(mk.maturity[,1],mk.zero2[8,2:56],lty=4,type="l")
legend("bottomright",c("1985-12-01", "1986-01-01",
                       "1986-02-01", "1986-03-01"),lty=1:4)


bondvalue <- function(c, T, r, par) {
  # Computes bond value (current price)
  # INPUT
  #   c = coupon
  #   T = time to maturity (years)
  #   r = vector of yeald to maturitt (semiannual rates)
  #   par = par value
  bv = c/r + (par - c/r) * (1+r)^(-2*T)
  bv
}

price = 1200
C = 40
T = 30
par = 1000

r = seq(0.02, .05, length = 300)
value = bondvalue(C, T, r, par)
yield2M = spline(value, r, xout=price)

plot(r, value, xlab = 'yield to maturity', ylab = 'price', type = 'l',
     main = 'par = 1000, C = 40, T = 30', lwd = 2)
abline(h = 1200)
abline(v = yield2M)

#### Part 2

install.packages("fEcofin", repos="http://R-Forge.R-project.org")


library(fEconfin)
help(fEcofin)


plot(mk.maturity[, 1], mk.zero2[5,2:56], type = "l", 
     xlab = "maturity", ylab = "yield")
lines(mk.maturity[, 1], mk.zero2[6, 2:56], lty = 2, type = "l")
lines(mk.maturity[, 1], mk.zero2[7, 2:56], lty = 3, type = "l")
lines(mk.maturity[, 1], mk.zero2[8, 2:56], lty = 4, type = "l")
legend("bottomright", c("1985-12-01", "1986-01-01", "1986-02-01", "1986-03-01"), lty = 1:4)

library("fEcofin")
library(mnormt)
Berndt  
= berndtInvest[,5:6]
names(Berndt)
plot(Berndt[,1],Berndt[,2],xlab="stock 1",ylab="Stock 2") 



#grafico simple

dat = read.csv("Stock_bond.csv",header=TRUE)

names(dat)
attach(dat)
par(mfrow=c(1,2))
plot(GM_AC)
plot(F_AC)

#calculo de retornos

n = dim(dat)[1]
GMReturn = GM_AC[2:n]/GM_AC[1:(n-1)] - 1
FReturn = F_AC[2:n]/F_AC[1:(n-1)] - 1
par(mfrow=c(1,1))
plot(GMReturn,FReturn)

################

data(EuStockMarkets)
mode(EuStockMarkets)
class(EuStockMarkets)
plot(EuStockMarkets)



logR = diff(log(EuStockMarkets))
plot(logR)

#de otra forma

plot(as.data.frame(logR))

index.names = dimnames(logR)[[2]]
par(mfrow=c(2,2))
for(i in 1:4)
{
  qqnorm(logR[,i],datax=T,main=index.names[i])
  qqline(logR[,i],datax=T)
  print(shapiro.test(logR[,i]))
}

#################

dat = read.table(file="WeekInt.txt",header=T)
attach(dat)
aaa_dif = diff(aaa)
cm10_dif = diff(cm10)

plot(cm10_dif,aaa_dif,xlab="Cambio en 10YR T tasa",
     ylab="cambio en tasa AAA ")


##############
#simulacion

library(MTS)
library(mvtnorm)
sig=diag(2)
x=rmvnorm(300,rep(0,2),sig)
MTSplot(x)

##############################
#segunda clase
############################


phi1=matrix(c(.2,-.6,.3,1.1),2,2) # lee phi_1
phi1


sig=matrix(c(1,0.8,0.8,2),2,2) # Leer sigma_a
sig

m1=eigen(phi1) # obtiene vectores y valores propios
m1


I4=diag(rep(1,4)) # Crea matriz identidad 4x4

pp=kronecker(phi1,phi1) # Kronecker product
pp

c1=c(sig)
c1

dd=I4-pp
ddinv=solve(dd)   ## Obtiene inversa
gam0=ddinv%*%matrix(c1,4,1)  # obtiene Gamma_0
gam0

g0=matrix(gam0,2,2)
g1=phi1%*%g0     ## Obtien  Gamma_1
g1

g2=phi1%*%g1
g2

D=diag(sqrt(diag(g0))) # Calculo de matrices de correlacion cruzadas
D


Di=solve(D)
Di%*%g0%*%Di

Di%*%g1%*%Di

Di%*%g2%*%Di


#Ejemplo con datos

#PIB de UK canada y US

require(MTS)  ### 
da=read.table("q-gdp-ukcaus.txt",header=T)
gdp=log(da[,3:5]) #logaritmo de las series
dim(gdp)

z=diffM(gdp)

z=z*100

dim(z)

Z=z[3:125,]
X=cbind(rep(1,123),z[2:124,],z[1:123,])
X=as.matrix(X)
XPX=t(X)%*%X
XPXinv=solve(XPX)
Z=as.matrix(Z)
XPZ=t(X)%*%Z
bhat=XPXinv%*%XPZ
bhat


A=Z-X%*%bhat
Sig=t(A)%*%A/(125-(3+1)*2-1)
Sig    

COV=kronecker(Sig,XPXinv)
se=sqrt(diag(COV))
beta=c(bhat)
para=cbind(beta,se,beta/se)
colnames(para) <- c("beta","se","t-ratio")
para

##############

sig=diag(2)
x=rmvnorm(300,rep(0,2),sig)
MTSplot(x) 
ccm(x)



q()  ## exit R 
