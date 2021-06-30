
#Ejemplo de Impulso respuesta


# Datos

data <- read.table("http://www.jmulti.de/download/datasets/e1.dat", skip = 6, header = TRUE)
library(MTS)
# 
# VAR(2) despues de diferencias
data <- data[1:76, ]

# Convertir en objeto de series de tiempo

data <- ts(data, start = c(1960, 1), frequency = 4)
data1 <- diff(data,12)
data# logs y diferencias
data <- diff(log(data))
datadata1<-diff(log(data),12)
data1
# grafica de datos

plot(data,  main = "Dataset E1 from L脙录tkepohl (2007)")


# Load package
library(vars)


?VAR
# Estimacion del modelo
model <- VAR(data, p = 2, type = "none")

# resumen de estadisticas
summary(model)

feir <- irf(model, impulse = "income", response = "cons",
            n.ahead = 8, ortho = FALSE, runs = 1000)

plot(feir)

# Resumen de estadisticas

model_summary <- summary(model)


# Obtencion de la matriz de varianzas y covarianzas
model_summary$covres

model_summary$corres


#Ya que los elementos fuera de la diagonal de la matriz de varianzas y covarianzas son diferentes de cero, 
#Se puede asumir que hay relaci贸n contemporan茅a entre las variables del modelo VAR. 
#Esto se comprueba con la matriz de correlaci贸n:

#Aunque, las matrices describen la correlaci贸n entre los errores, es confuso sabre en que sentido va la causalidad.
#.
#La identificaci贸n de las relaciones causales es uno de los mayores temas de investigaci贸n en el an谩lisis de los modelos VAR.
t(chol(model_summary$covres))



oir <- irf(model, impulse = "income", response = "cons",
           n.ahead = 8, ortho = TRUE, runs = 1000, seed = 12345)

plot(oir)




oir <- irf(model, impulse = "income", response = "cons",
           n.ahead = 12, ortho = TRUE, runs = 1000, seed = 12345)

plot(oir)


oir0 <- irf(model, impulse = "invest", response = "cons",
            n.ahead = 8, ortho = TRUE, runs = 1000, seed = 12345)

plot(oir0)


oir <- irf(model, impulse = "income", response = "cons",
           n.ahead = 8, ortho = TRUE, runs = 1000, seed = 12345)

plot(oir)



oir02 <- irf(model, impulse = "cons", response = "cons",
             n.ahead = 8, ortho = TRUE, runs = 1000, seed = 12345)

plot(oir02)



oir1 <- irf(model, impulse = "invest", response = "invest",
            n.ahead = 8, ortho = TRUE, runs = 1000, seed = 12345)

plot(oir1)



oir11 <- irf(model, impulse = "invest", response = "income",
             n.ahead = 8, ortho = TRUE, runs = 1000, seed = 12345)

plot(oir11)



oir12 <- irf(model, impulse = "invest", response = "cons",
             n.ahead = 8, ortho = TRUE, runs = 1000, seed = 12345)

plot(oir12)



oir2 <- irf(model, impulse = "income", response = "invest",
            n.ahead = 8, ortho = TRUE, runs = 1000, seed = 12345)

plot(oir2)




oir21 <- irf(model, impulse = "income", response = "cons",
             n.ahead = 8, ortho = TRUE, runs = 1000, seed = 12345)

plot(oir21)




oir22 <- irf(model, impulse = "income", response = "cons",
             n.ahead = 8, ortho = TRUE, runs = 1000, seed = 12345)

plot(oir22)

####################simulacion#######################
#library(tsDyn)

#VAR(2)

A = cbind(c(0.2,0),c(-0.3,0.4))
B = cbind(c(-0.1,0.1),c(0.2,-0.3))



varstep <- function(A,B,x,y) {
  e = rnorm(2,0,1)
  A%*%x +B%*%y + e
}

x1 = c(1,1) 
y2 = c(.5,5)


results = cbind(y2,x1)
for (t in seq(1,100))
{
  temp <- x1
  x1 <- varstep(A,B,x1,y2)
  results <- cbind(results, x1)
  y2 <- temp
}

xt = results[1,1:100]
yt = results[2,1:100]
plot(1:100,xt,type = "line")
lines(1:100,yt,col="red")


############################con funciones############################
B1<-matrix(c(0.7, 0.2, 0.2, 0.7), 2)
B1
var1 <- VAR.sim(B=B1, n=100, include="none")
ts.plot(var1, type="l", col=c(1,2))

######################################

B2<-rbind(c(0.5, 0.5, 0.5), c(0, 0.5, 0.5))
B2
varcov<-matrix(c(1,0.2, 0.3, 1),2)
varcov
var2 <- VAR.sim(B=B2, n=100, include="const", varcov=varcov)
ts.plot(var2, type="l", col=c(1,2))

###########################################
X=cbind(rep(1,123))
X
x <- matrix(1 2 3, nrow = 3, dimnames = list(c("X","Y","Z"), c("A","B","C")))
x

C<-matrix(c(0.8,0.4, -0.3, 0.6),2)

S<-matrix(c(2.0, 0.5, 0.5, 1.0),2)
S

m1 = VARMAsim(300,arlags = c(1),phi = C,sigma = S)
zt = m1$series
ts.plot(zt[,1])
zt
ts.plot(zt[,2])


da=read.table("q-gdp-ukcaus.txt",header=T) 
gdp=log(da[,3:5])
z=gdp[2:126,]-gdp[1:125,]
zt=diff(gdp,lag=12)
?diffz=z*100 
m1=VAR(z,p=2)
m2=refVAR(m1,thres=1.0)  #refinamiento del VAR o modelo simplificado
?refVAR
z1=z/100 ### tasas de crecimiento 
m2=VARorder(z1)
            
MTSdiag(m2,adj=12)
names(m1)
resi=m1$residuals  
resi
mq(resi,adj=18)



data("mts-examples",package="MTS")
gdp=log(qgdp[,3:5])
zt=diffM(gdp)
m1=VAR(zt,3)
m2=refVAR(m1)
names(m2)
m1=VAR(zt,2)
m2=refVAR(m1,thres=1.96)
#chequeo
MTSdiag(m2,adj=12)
#predicci髇

VARpred(m1,8)



fit_var1 <- VAR(z1,p=2)
var1_residuals <- resid(m1)
var1_residuals
mq(var1_residuals,adj=18) 


install.packages("MTS_VERSION.tar.gz",repos=NULL,type="source")
library(MTS)
install.packages("devtools")
library(devtools)
install_github('MTS','d-')


################################
#regresion de series de tiempo

df1 <- read.csv(file = "WW3a.csv")
colnames(df1)[2:5] <- c("Y1t","Y2t","Y3t","Y4t")
df1

plot(df1$Year,df1[,2],type='l',xlab='t',ylab='Ventas')
plot(df1$Year,df1[,3],type='l',xlab='t',ylab='Ventas-bebidas')
plot(df1$Year,df1[,4],type='l',xlab='t',ylab='Ventas-salud')
plot(df1$Year,df1[,5],type='l',xlab='t',ylab='Ventas-comercio')

plot(df1$Year,df1[,5],type='l',xlab='t',ylab='PIB')
plot(df1$Year,df1[,8],type='l',xlab='t',ylab='DJ')
plot(df1$Year,df1[,11],type='l',xlab='t',ylab='IPC')

#minimos cuadrados ordinarios

reg1 <- lm(cbind(df1$Y1t,df1$Y2t,df1$Y3t,df1$Y4t) ~ df1$GDP +
             df1$GDP.1 + df1$GDP.2 + df1$DJIA.1 + df1$CPI)
summary(reg1)

#Coef de determinaci髇 alto pero pocas estrellas (poca significancia de las variables)

#removiendo algunas

lm1 <- lm(Y1t ~ DJIA.1,data = df1)
summary(lm1)


lm2 <- lm(Y2t ~ CPI,data = df1)
summary(lm2)


lm3 <- lm(Y3t ~ GDP + GDP.2, data = df1)
summary(lm3)

lm4 <- lm(Y4t ~ GDP, data = df1)
summary(lm4)


#mejorar los de MCO

library(nlme)

lm1 <- gls(Y1t ~ GDP + GDP.1 + GDP.2 + DJIA.1 + CPI,
           data = df1, method = "ML", correlation = corARMA
           (p = 1,form = ~ Year),
           verbose = TRUE)
summary(lm1)



lm2 <- gls(Y2t ~ GDP + GDP.1 + GDP.2 + DJIA.1 + CPI,
           data = df1, method = "ML", correlation = corARMA
           (p = 1,form = ~ Year),
           verbose = TRUE)
summary(lm2)



lm3 <- gls(Y3t ~ GDP + GDP.1 + GDP.2 + DJIA.1 + CPI,
           data = df1, method = "ML", correlation = corARMA
           (p = 1,form = ~ Year),
           verbose = TRUE)
summary(lm3)

lm4 <- gls(Y4t ~ GDP + GDP.1 + GDP.2 + DJIA.1 + CPI,
           data = df1, method = "ML", correlation = corARMA
           (p = 1,form = ~ Year),
           verbose = TRUE)
summary(lm4)

df2 <- df1[df1$Year != 2015,]
reg2 <- lm(cbind(Y1t,Y2t,Y3t,Y4t) ~ GDP +
             GDP.1 + GDP.2 + DJIA.1 + CPI,data = df2)
summary(reg2)

predict(reg2, newdata = df1[df1$Year==2015,])

###########################

install.packages("devtools")
library(devtools)
install_github('MTS','d-')

library(MTS)

#####################simulacion###############################
theta=matrix(c(0.5,0.4,0,0.6),2,2); sigma=diag(2)
theta

m1=VARMAsim(200,malags=c(1),theta=theta,sigma=sigma)

zt=m1$series

plot(zt[,1],type='l',xlab='t',ylab='grafica 1')
plot(zt[,2],type='l',xlab='t',ylab='grafica 2')

m2=VMA(zt,q=1,include.mean=FALSE)


#####################################


######################
  install.packages("MTS_VERSION.tar.gz",repos=NULL,type="source")
library(MTS)
###########################
#Modelo de promedio m髒il
#portafolio con acciones

da=read.table("m-dec15678-6111.txt",header=T)
head(da)

  x=log(da[,2:6]+1)*100
 rtn=cbind(x$dec5,x$dec8)
 tdx=c(1:612)/12+1961
 par(mfcol=c(2,1))
 plot(tdx,rtn[,1],type='l',xlab='year',ylab='d5')
 plot(tdx,rtn[,2],type='l',xlab='year',ylab='d8')
 ccm(rtn)
 #las dos series tiene dependencia en un rezago
 
 
 #orden
 
 a<-VMAorder(zt,lag=20)
 
 m2=VMAe(rtn,q=1)
 
 rtn=cbind(ibm,ko)
 mq(rtn,10)
 
 mm=ccm(yt)

 #maxima verosimilitud condicional y exacta
 
 m1=VMA(rtn,q=1,include.mean=F) 
 
 m2=VMAe(rtn,q=1,include.mean=F)

   ###############################
 #simulacion
 
 p1=matrix(c(.816,-1.116,-.623,1.074),2,2)
  p2=matrix(c(-.643,.615,.592,-.133),2,2)
  phi=cbind(p1,p2)
  t1=matrix(c(0,-.801,-1.248,0),2,2)
  Sig=matrix(c(4,2,2,5),2,2)
  
  m1=VARMAsim(400,arlags=c(1,2),malags=c(1),phi=phi,
               theta=t1,sigma=Sig)
  zt=m1$series
  
  #funcion de correlacion extendida
  
  m2=Eccm(zt,maxp=5,maxq=6)
  
  
 m1=VMA(rtn,q=1)

 
 
 da=read.table("ushog.txt",header=T)
  head(da)
  m1=Eccm(da,maxp=5,maxq=6)
  
  VARorder(da,maxp=9)

  
  z1=diff(log(da1$pce)); z2=diff(log(da2$dspi))
  zt=cbind(z1,z2)*100
  colnames(zt) <- c("pceg","dspig")
  VARorder(zt)

  
  m1=VAR(zt,3) ## fit a VAR(3) model
  
  Eccm(zt,maxp=6,maxq=6)
  
  
  m2=VARMA(zt,p=3,q=1)
  
  
  
  da=read.table("m-hsmort7112.txt",header=T)
  zt=da[,3:4]; colnames(zt) <- c("hs","mort")
  dzt=diffM(zt)
  VARorder(dzt)

  m1=VAR(dzt,4) #### VAR(4) modelo
  
  m2=VMAe(rtn,q=1) #verosimilitud completa
####################################################
  
  
  ms <- as.data.frame(read.csv("WW2b.csv")[,-1])
  rownames(ms) <- seq(as.Date("2009/6/1"), by="month",
                      length=90)
  
  ## Construccion de modelos
  #diferencia
  dszt=diffM(ms,d=12)
  drszt=diffM(dszt)
  
  ## Plot the five differenced series
  par(mfrow=c(2,3))
  for(i in 1:5){
    plot.ts(drszt[,i],main=colnames(ms)[i],xlab=NULL,
            ylab='Differenciadas')}
  
  
  
  ## Funciones de matrices de correlacion y  parc y extendida
  ## CCM with default lag=12
  drszt.ccm<-ccm(drszt, lag=15)
  drszt.pacf<-pacf(drszt, 15)
  eccmz=Eccm(drszt)
  
  
  ## Ajuste del modelo
  m=VAR(drszt, p=2, include.mean=FALSE)
  
  ##pronosticos
  ff <- VARpred(m)
  ff$pred
  
  # IC_S 95%
  upper<-ff$pred+1.96*ff$se
  upper
  # IC_I 95%
lower<-ff$pred-1.96*ff$se
lower
  ##########################
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

m1 <- VAR(dat[1:(131+64-1),],p=1,output=FALSE,include.mean=FALSE)

round(m1$coef,2) # estimatacionr
round(m1$secoef,2) # error standard 

ff <- VARpred(m1)
ff$pred

# upper 95%
upper<-ff$pred+1.96*ff$se
upper
# lower 95%
lower<-ff$pred-1.96*ff$se
lower


#########################################





###########################################
  #Simulacion de VARMA(2,1)
   p1=matrix(c(.816,-1.116,-.623,1.074),2,2)
   p2=matrix(c(-.643,.615,.592,-.133),2,2)
   phi=cbind(p1,p2)
   t1=matrix(c(0,-.801,-1.248,0),2,2)
   Sig=matrix(c(4,2,2,5),2,2)
   m1=VARMAsim(400,arlags=c(1,2),malags=c(1),phi=phi,
                theta=t1,sigma=Sig)
   zt=m1$series
   m2=Eccm(zt,maxp=5,maxq=6)
   
   #############################
   
    da=read.table("ushog.txt",header=T)
    head(da)
    m1=Eccm(da,maxp=5,maxq=6)
    m1
    
    #####################################
    
    
    z1=diff(log(da1$pce)); z2=diff(log(da2$dspi))
    zt=cbind(z1,z2)*100
    colnames(zt) <- c("pceg","dspig")
    
    VARorder(da,maxp=9)
    ########################################
    
    
    da=read.table("m-hsmort7112.txt",header=T)
    zt=da[,3:4]; colnames(zt) <- c("hs","mort")
    dzt=diffM(zt)
    VARorder(dzt)
    m1=VAR(dzt,4)
    m1a=refVAR(m1,thres=1)
    
    MTSdiag(m1a)
    Eccm(dzt,maxp=6,maxq=6)
    m2=VARMA(dzt,p=2,q=1)
    m2a=refVARMA(m2,thres=0.8)
    m2b=refVARMA(m2a,thres=1)
    m2c=refVARMA(m2b,thres=1)
    ######################################
    
    
    #TAREA (haga un informe con el siguiente codigo)
    da=read.table("m-ibmko-0111.txt",header=T)
    head(da)
    lrtn=log(da[,2:3]+1)*100
    dim(da)
    tdx=c(1:132)/12+2001
    colnames(lrtn) <- c("ibm","ko")
    MTSplot(lrtn,tdx)
    ccm(lrtn)
    mq(lrtn,10)
    #utilizar la correwlacion cruzada extendida.
    #diferencias Eccm
    
    yt=diffM(lrtn)
    mm=ccm(yt)
    m1=VMA(lrtn,q=1,include.mean=F)
    m2=VMAe(lrtn,q=1,include.mean=F)
    m1=VMA(yt,q=1,include.mean=F)
    m2=VMAe(yt,q=1,include.mean=F)
    ##################################################
    
    setwd("D:/")
    d10 <- as.data.frame(read.csv("WW4a.csv")[,-1])
    d10 <- t(t(d10) - colMeans(d10))
    rownames(d10) <- seq(as.Date("2016/8/2"), by="day",
                       length=107)
    d10
     
     #covarianzas
    cov(d10)
    #correlaciones
        cor(d10)
    
    ##Grafico de las series de tiempo
    plot(seq(as.Date("2016/8/2"), by="day", length=107),d10[,1],
         type='l',ylab="Chevron-Stock Returns",xlab="Day")
    
    plot(seq(as.Date("2016/8/2"), by="day", length=107),d10[,2],
         type='l',ylab="EXXON-Stock Returns",xlab="Day")
    
    plot(seq(as.Date("2016/8/2"), by="day", length=107),d10[,3],
         type='l',ylab="APPLE-Stock Returns",xlab="Day")
    
    plot(seq(as.Date("2016/8/2"), by="day", length=107),d10[,4],
         type='l',ylab="Facebook-Stock Returns",xlab="Day")

    plot(seq(as.Date("2016/8/2"), by="day", length=107),d10[,5],
         type='l',ylab="Microsoft-Stock Returns",xlab="Day")

      plot(seq(as.Date("2016/8/2"), by="day", length=107),d10[,6],
         type='l',ylab="MERCK-Stock Returns",xlab="Day")
    
      
      plot(seq(as.Date("2016/8/2"), by="day", length=107),d10[,7],
           type='l',ylab="PFizer-Stock Returns",xlab="Day")
      
      
      plot(seq(as.Date("2016/8/2"), by="day", length=107),d10[,8],
           type='l',ylab="Bank of America-Stock Returns",xlab="Day")

      
      plot(seq(as.Date("2016/8/2"), by="day", length=107),d10[,9],
           type='l',ylab="JP Morgan of America-Stock Returns",xlab="Day")
      
      
      plot(seq(as.Date("2016/8/2"), by="day", length=107),d10[,9],
           type='l',ylab="Fargo-Stock Returns",xlab="Day")
      
      #grafica de todas
      
    for(i in 2:10){
      lines(seq(as.Date("2016/8/2"), by="day", length=107),
            d10[,i],type='l',col=i)
    }
    legend("topleft",legend=colnames(d10),col=1:10,lty=1)
   
     ##Analisis de componentes principales
    
    pca <- princomp(d10)
    pca
    pca <- princomp(d10,cor=F)
    lds <- pca$loadings
    
    #grafico de los componentes principales
    
    screeplot(pca,type="lines",main="Grafico cp")
  
    
     #basado en correlaci髇
    pca <- princomp(d10,cor=T)
    lds <- pca$loadings
    scs <- pca$scores
    screeplot(pca,type="lines",main="screeplot")
              ##Grafico Sectores por los primeros dos loadings
              library(ggplot2)
              C <- as.data.frame(cbind(lds[,1],lds[,2]))
              ggplot(C,aes(C[,1],C[,2],label=rownames(C))) +
                geom_point(size=4,col=3) +
                geom_text(vjust=0,hjust=0,angle = 10,size=5) +
                xlab("Loading 1") +
                ylab("Loading 2")
              ##Grafico  Series de tiempo por scores sobre las dos primeras 2 componentes 
              C <- as.data.frame(cbind(scs[,1],scs[,2]))
              palette(rainbow(400))
              ggplot(C,aes(C[,1],C[,2],label=substring(rownames(C),1,7))) +
                geom_point(size=4,col=1:nrow(C)) +
                geom_text(vjust=0,hjust=0,angle = 10,size=5) +
                xlab("Scoring 1") +
                ylab("Scoring 2")
              palette("default")
              ##Grafico: Serie de tiempo por Scores en las primesras dos componentes
              C <- as.data.frame(cbind(scs[,1],scs[,2]))
              palette(rainbow(400))
              ggplot(C,aes(C[,1],C[,2],label=substring(rownames(C),1,4))) +
                geom_path() +
                geom_point(size=4,col=1:nrow(C)) +
                geom_text(vjust=0,hjust=0,angle = 10,size=5) +
                xlab("Scoring 1") +
                ylab("Scoring 2")
              palette("default")
              plot(seq(as.Date("2009/4/1"), by="month", length=107),C[,1],
                   type="l",ylab="Scoring 1",xlab="Date")
              plot(seq(as.Date("2009/4/1"), by="month", length=107),C[,2],
                   type="l",ylab="Scoring 2",xlab="Date")
              ag <- aggregate(C[,1:2],list(substr(rownames(C),1,4)),mean)
              C <- as.data.frame(ag)
              ggplot(C,aes(C[,2],C[,3],label=C[,1])) +
                geom_point(size=4,col=3) +
                geom_text(vjust=0,hjust=0,angle = 10,size=5) +
                xlab("Scoring 1") +
                ylab("Scoring 2")

              
                            
##########################
              #otro ejemplo 
              
              library(dplyr)
              
              d5 <- as.data.frame(read.csv("WW4b.csv")[,-1])
              dim(d5)
              rownames(d5) <- seq(as.Date("1986/1/1"), by="month", length=212)
              d5a<- select(d5,-X,-Month.1,-E.1,-A.1,-C.1,-H.1,-G.1)
              #result <- filter(d5a,d5a$E >= 50)
              
              
              ##Covariance and Correlation Matrices
              d5 <- t(t(d5a) - colMeans(d5a))
              cov(d5)
              cor(d5)
              
              ##Plot Time Series Data
              plot(seq(as.Date("1986/1/1"), by="month", length=212),d5a
                   [,1],type='l',ylab="Precios del consumidor
Indice (Jan84=100)",xlab="Date",ylim=c(50,260))
              for(i in 2:5){
                lines(seq(as.Date("1986/1/1"), by="month",
                          length=212),d5a[,i],type='l',lty=i)
              }
              
              
              
              
              
              
              library(ggplot2)
              
              
              library(dplyr)
              
              d5 <- as.data.frame(read.csv("WW4b.csv")[,-1])
              dim(d5)
              rownames(d5) <- seq(as.Date("1986/1/1"), by="month", length=212)
              d5a<- select(d5,-X,-Month.1,-E.1,-A.1,-C.1,-H.1,-G.1)
              ##Import Data: monthly Consumer Price Index (CPI) from five
              d5 <- as.data.frame(read.csv("WW4b.csv")[,-1])
              rownames(d5) <- seq(as.Date("1986/1/1"), by="month",
                                  length=212)
              ##Covariance and Correlation Matrices
              d5 <- t(t(d5a) - colMeans(d5a))
              cov(d5)
              cor(d5)
              ##Plot Time Series Data
              plot(seq(as.Date("1986/1/1"), by="month", length=347),d5
                   [,1],type='l',ylab="Consumer Price
Index (Jan84=100)",xlab="Date",ylim=c(-90,160))
              for(i in 2:5){
                lines(seq(as.Date("1986/1/1"), by="month",
                          length=347),d5[,i],type='l',lty=i)
              }
              legend("topleft",legend=colnames(d5),,lty=1:5)
              ##Principal Component Analysis
              pca <- princomp(d5,cor=T)
              lds <- pca$loadings
              scs <- pca$scores
              screeplot(pca,type="lines",main="screeplot")
              ##Plot: Sectors by Their First 2 Loadings
              library(ggplot2)
              C <- as.data.frame(cbind(lds[,1],lds[,2]))
              ggplot(C,aes(C[,1],C[,2],label=rownames(C))) +
                geom_point(size=4,col=3) +
                geom_text(vjust=0,hjust=0,angle = 10,size=5) +
                xlab("Loading 1") +
                ylab("Loading 2")
              ##Plot: Time Points by Their Scores on First 2 Components
              C <- as.data.frame(cbind(scs[,1],scs[,2]))
              palette(rainbow(400))