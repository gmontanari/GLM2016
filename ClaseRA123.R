### ----- REGRESION AVANZADA ----- ###
# --- Prof. Luis E. Nieto Barajas --- #


#--- Usar espejo CRAN del ITAM ---
options(repos="cran.itam.mx")

#--- Funciones utiles ---
prob<-function(x){
  out<-min(length(x[x>0])/length(x),length(x[x<0])/length(x))
  out
}

#--- Ilustracion del proceso de inferencia ---

#-Proceso de aprendizaje normal-normal-
xbar<-40.9533
sig2<-400
n<-30

th0<-19
sig20<-219.47

y<-seq(-10,100,,100)
f0y<-dnorm(y,th0,sqrt(sig20))
liky<-dnorm(y,xbar,sqrt(sig2/n))
sig21<-1/(n/sig2+1/sig20)
th1<-sig21*(n/sig2*xbar+th0/sig20)
f1y<-dnorm(y,th1,sqrt(sig21))
ymax<-max(f0y,liky,f1y)
plot(y,f0y,ylim=c(0,ymax),type="l")
lines(y,liky,lty=2,col=2)
lines(y,f1y,lty=3,col=3)

#-Proceso de aprendizaje bernoulli-beta-

#Simulacion de datos Bernoulli
theta0 <- 0.6
n <- 10
x<-rbinom(n,1,theta0)
hist(x,freq=FALSE)

#Distribucion inicial para theta
a <- 1
b <- 1
theta<-seq(0,1,,100)
plot(theta,dbeta(theta,a,b),type="l")

#Distribucion final
a1 <- a + sum(x)
b1 <- b + n - sum(x)
plot(theta,dbeta(theta,a1,b1),type="l")

#Ambas
theta<-seq(0,1,,100)
ymax <- max(dbeta(theta,a,b),dbeta(theta,a1,b1))
plot(theta,dbeta(theta,a,b),type="l",ylim=c(0,ymax))
lines(theta,dbeta(theta,a1,b1),col=2)
abline(v=theta0,col=4)

#Aproximacion normal asintotica
mu <- (a1-1)/(a1+b1-2)
sig2 <- (a1-1)*(b1-1)/(a1+b1-2)^3
lines(theta,dnorm(theta,mu,sqrt(sig2)),col=3)


# --- Aproximaci?n Monte Carlo ---
#-Ejemplo 1-
x<-seq(-2,4,,100)
f<-function(x){
  out <- 5-(x-1)^2
  out <- ifelse (x < -1 | x>3,0,out)
  out
}
plot(x,f(x)*3/44,type="l",ylim=c(0,0.5))
lines(x,dnorm(x,0,1),lty=2,col=2)
lines(x,dnorm(x,1,2/3),lty=3,col=3)
lines(x,dnorm(x,1,1),lty=4,col=4)

#Caso 1: S=Normal estandar
mu<-0
sig<-1
N<-100000
y<-rnorm(N,mu,sig)
I1<-mean(f(y)/dnorm(y,mu,sig))
eI1<-sd(f(y)/dnorm(y,0,1))/sqrt(N)
print(c(I1,eI1))

#Caso 2: S=Normal no estandar
mu<-1
sig<-2/3
N<-100000
y<-rnorm(N,mu,sig)
I2<-mean(f(y)/dnorm(y,mu,sig))
eI2<-sd(f(y)/dnorm(y,mu,sig))/sqrt(N)
print(c(I2,eI2))

#Caso 3: S=Normal no estandar
mu<-1
sig<-1
N<-100000
y<-rnorm(N,mu,sig)
I3<-mean(f(y)/dnorm(y,mu,sig))
eI3<-sd(f(y)/dnorm(y,mu,sig))/sqrt(N)
print(c(I3,eI3))

#-Ejemplo 2-
f<-function(x){
  out<-ifelse(x<0,0,x)
  out<-ifelse(x>1,0,out)
  out
}

x<-seq(-1,2,,100)
plot(x,f(x),type="l",ylim=c(0,1.2))

N<-10000

#Caso 1: S=Uniforme
lines(x,dunif(x,0,1),col=2,lty=2)
y<-runif(N,0,1)
I1<-mean(f(y)/dunif(y,0,1))
eI1<-sd(f(y)/dunif(y,0,1))/sqrt(N)
print(c(I1,eI1))

#Caso 2: S=Exponencial
lines(x,dexp(x,1),col=3,lty=3)
y<-rexp(N,1)
I2<-mean(f(y)/dexp(y,1))
eI2<-sd(f(y)/dexp(y,1))/sqrt(N)
print(c(I2,eI2))

#Caso 3: S=Normal
lines(x,dnorm(x,0.5,1/3),col=4,lty=4)
y<-rnorm(N,0.5,1/3)
I3<-mean(f(y)/dnorm(y,0.5,1/3))
eI3<-sd(f(y)/dnorm(y,0.5,1/3))/sqrt(N)
print(c(I3,eI3))


#-Muestreador de Gibbs-
install.packages("bayesm")
library(bayesm)
out<-rbiNormGibbs(rho=.95)
out<-rbiNormGibbs(rho=-0.5)


###########################################

install.packages("R2OpenBUGS")
install.packages("R2jags")
library(R2OpenBUGS)
library(R2jags)

#-Working directory-
wdir<-"/Users/guillemontanari/ITAM/GLM - Nieto 2016"
setwd(wdir)


#--- Ejemplo 1---
#-Reading data-
n<-100
credito<-c(rep(1,n/2),rep(0,n/2))

#-Defining data-
data<-list("n"=n,"x"=credito)

#-Defining inits-
inits<-function(){list(theta=0.5,x1=rep(1,2))}
inits<-function(){list(lambda=0)}
inits<-function(){list(theta=0.5,eta=1)}

#-Selecting parameters to monitor-
parameters<-c("theta","x1")
parameters<-c("theta","eta")

#-Running code-
#OpenBUGS
#ej1.sim<-bugs(data,inits,parameters,model.file="Ej1.txt",
#              n.iter=5000,n.chains=1,n.burnin=500)
#JAGS
ej1.sim<-jags(data,inits,parameters,model.file="Ej1.txt",
              n.iter=5000,n.chains=1,n.burnin=500)

#-Monitoring chain-

#Traza de la cadena
traceplot(ej1.sim)

#Cadena

#OpenBUGS
#out<-ej1.sim$sims.list

#JAGS
out<-ej1.sim$BUGSoutput$sims.list

#z<-out$theta
#par(mfrow=c(2,2))
#plot(z,type="l")
#plot(cumsum(z)/(1:length(z)),type="l")
#hist(z,freq=FALSE)
#acf(z)

#Resumen (estimadores)
#OpenBUGS
#out.sum<-ej1.sim$summary

#JAGS
out.sum<-ej1.sim$BUGSoutput$summary

print(out.sum)

#DIC
#OpenBUGS
#out.dic<-ej1.sim$DIC

#JAGS
out.dic<-ej1.sim$BUGSoutput$DIC
print(out.dic)

#---------------------#
#Mezcla de betas
w<-seq(0.01,0.99,,100)
pp<-0.3
fw<-pp*dbeta(w,10,10)+(1-pp)*dbeta(w,6,0.01)
par(mfrow=c(1,1))
plot(w,fw,type="l")


#--- Ejemplo 2---
#-Reading data-
utilidad<-c(212, 207, 210,
196, 223, 193,
196, 210, 202, 221)
n<-length(utilidad)

#-Defining data-
data<-list("n"=n,"x"=utilidad)

#-Defining inits-
inits<-function(){list(mu=0,sig=1,x1=0)}

#-Selecting parameters to monitor-
parameters<-c("mu","sig","x1")

#-Running code-
#OpenBUGS
ej2.sim<-bugs(data,inits,parameters,model.file="Ej2.txt",
              n.iter=5000,n.chains=1,n.burnin=500)
#JAGS
ej2.sim<-jags(data,inits,parameters,model.file="Ej2.txt",
              n.iter=5000,n.chains=1,n.burnin=500)

#-Monitoring chain-

#Traza de la cadena
traceplot(ej2.sim)

#Cadena

#OpenBUGS
out<-ej2.sim$sims.list

#JAGS
out<-ej2.sim$BUGSoutput$sims.list

z<-out$x1
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)

#Resumen (estimadores)
#OpenBUGS
out.sum<-ej2.sim$summary

#JAGS
out.sum<-ej2.sim$BUGSoutput$summary

print(out.sum)

#DIC
#OpenBUGS
out.dic<-ej2.sim$DIC

#JAGS
out.dic<-ej2.sim$BUGSoutput$DIC

print(out.dic)


#--- Ejemplo 3 ---
#-Reading data-
calif<-read.table("http://allman.rhon.itam.mx/~lnieto/index_archivos/calificaciones.txt",header=TRUE)
n<-nrow(calif)
plot(calif)

#-Defining data-
data<-list("n"=n,"y"=calif$SP,"x"=calif$MO)

#-Defining inits-
inits<-function(){list(beta=rep(0,2),tau=1,yf=rep(0,n))}

#-Selecting parameters to monitor-
parameters<-c("beta","tau","yf")

#-Running code-
#OpenBUGS
ej3.sim<-bugs(data,inits,parameters,model.file="Ej3.txt",
              n.iter=10000,n.chains=1,n.burnin=1000)
#JAGS
ej3.sim<-jags(data,inits,parameters,model.file="Ej3.txt",
              n.iter=10000,n.chains=1,n.burnin=1000)

#-Monitoring chain-

#Traza de la cadena
traceplot(ej3.sim)

#Cadena

#OpenBUGS
out<-ej3.sim$sims.list

#JAGS
out<-ej3.sim$BUGSoutput$sims.list

z<-out$beta[,2]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)

#Resumen (estimadores)
#OpenBUGS
out.sum<-ej3.sim$summary

#JAGS
out.sum<-ej3.sim$BUGSoutput$summary

print(out.sum)
head(out.sum)

#Probabilidades
z<-out$beta[,1]
prob(z)

#DIC
out.dic<-ej3.sim$DIC
out.dic<-ej3.sim$BUGSoutput$DIC
print(out.dic)

#Predictions
out.yf<-out.sum[grep("yf",rownames(out.sum)),]
or<-order(calif$MO)
ymin<-min(calif$SP,out.yf[,c(1,3,7)])
ymax<-max(calif$SP,out.yf[,c(1,3,7)])
par(mfrow=c(1,1))
plot(calif$MO,calif$SP,ylim=c(ymin,ymax))
lines(calif$MO[or],out.yf[or,1],lwd=2,col=2)
lines(calif$MO[or],out.yf[or,3],lty=2,col=2)
lines(calif$MO[or],out.yf[or,7],lty=2,col=2)


#--- Ejemplo 4 ---
#TAREA


#--- Ejemplo 5 ---
#-Reading data-
mortality<-read.table("http://allman.rhon.itam.mx/~lnieto/index_archivos/mortality.txt",header=TRUE)
n<-nrow(mortality)
plot(mortality)
m<-1
nef<-c(100)
xf<-c(200)

#-Defining data-
data<-list("n"=n,"ne"=mortality$n,"y"=mortality$y,"x"=mortality$x,"m"=m,"nef"=nef,"xf"=xf)

#-Defining inits-
inits<-function(){list(beta=rep(0,2),yf1=rep(1,n),yf2=1)}

#-Selecting parameters to monitor-
parameters<-c("beta","yf1","yf2")

#-Running code-
#OpenBUGS
ej5a.sim<-bugs(data,inits,parameters,model.file="Ej5a.txt",
              n.iter=50000,n.chains=1,n.burnin=5000)
ej5b.sim<-bugs(data,inits,parameters,model.file="Ej5b.txt",
               n.iter=50000,n.chains=1,n.burnin=5000)
ej5c.sim<-bugs(data,inits,parameters,model.file="Ej5c.txt",
               n.iter=50000,n.chains=1,n.burnin=5000,debug=TRUE)
#JAGS
ej5a.sim<-jags(data,inits,parameters,model.file="Ej5a.txt",
              n.iter=50000,n.chains=1,n.burnin=5000)
ej5b.sim<-jags(data,inits,parameters,model.file="Ej5b.txt",
               n.iter=50000,n.chains=1,n.burnin=5000)
ej5c.sim<-jags(data,inits,parameters,model.file="Ej5c.txt",
               n.iter=50000,n.chains=1,n.burnin=5000)

#-Monitoring chain-

#Traza de la cadena
traceplot(ej5.sim)

#Cadena

#OpenBUGS
out<-ej5.sim$sims.list

#JAGS
out<-ej5.sim$BUGSoutput$sims.list

z<-out$beta[,2]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)

#Resumen (estimadores)
#OpenBUGS
out.sum<-ej5c.sim$summary

#JAGS
out.sum<-ej5.sim$BUGSoutput$summary

print(out.sum)
head(out.sum)

#Probabilidades
z<-out$beta[,1]
prob(z)

#DIC
out.dic<-ej5c.sim$DIC
out.dic<-ej5.sim$BUGSoutput$DIC
print(out.dic)

#Predictions
out.yf<-out.sum[grep("yf1",rownames(out.sum)),]
or<-order(mortality$x)
ymin<-min(mortality$y,out.yf[,c(1,3,7)])
ymax<-max(mortality$y,out.yf[,c(1,3,7)])

par(mfrow=c(1,1))
plot(mortality$x,mortality$y,ylim=c(ymin,ymax))
#Modelo 1
lines(mortality$x[or],out.yf[or,1],lwd=2,col=2)
lines(mortality$x[or],out.yf[or,3],lty=2,col=2)
lines(mortality$x[or],out.yf[or,7],lty=2,col=2)
#Modelo 2
lines(mortality$x[or],out.yf[or,1],lwd=2,col=3)
lines(mortality$x[or],out.yf[or,3],lty=2,col=3)
lines(mortality$x[or],out.yf[or,7],lty=2,col=3)

plot(mortality$y,out.yf[,1])
abline(a=0,b=1)
cor(mortality$y,out.yf[,1])


#--- Ejemplo 6 ---
#-Reading data-
desastres<-read.table("http://allman.rhon.itam.mx/~lnieto/index_archivos/desastres.txt",header=TRUE)
n<-nrow(desastres)
plot(desastres,type="l")
plot(desastres[2:n,2]-desastres[1:(n-1),2],type="l")
plot(log(desastres[2:n,2])-log(desastres[1:(n-1),2]),type="l")


#-Defining data-
data<-list("n"=n,"y"=desastres$No.Desastres,"x"=desastres$Anho)

#-Defining inits-
inits<-function(){list(beta=rep(0,2),yf1=rep(1,n))}
#inits<-function(){list(beta=rep(0,2),aux=1,aux2=1,yf1=rep(1,n))}
inits<-function(){list(beta=rep(0,n),tau.b=1,yf1=rep(1,n))}

#-Selecting parameters to monitor-
parameters<-c("beta","yf1","mu")
#parameters<-c("beta","r","tau","yf1","mu")

#-Running code-
#OpenBUGS
ej6a.sim<-bugs(data,inits,parameters,model.file="Ej6a.txt",
               n.iter=50000,n.chains=1,n.burnin=5000)
ej6b.sim<-bugs(data,inits,parameters,model.file="Ej6b.txt",
               n.iter=50000,n.chains=1,n.burnin=5000)
ej6c.sim<-bugs(data,inits,parameters,model.file="Ej6c.txt",
               n.iter=5000,n.chains=1,n.burnin=500)
#JAGS
ej6a.sim<-jags(data,inits,parameters,model.file="Ej6a.txt",
              n.iter=50000,n.chains=1,n.burnin=5000)
ej6b.sim<-jags(data,inits,parameters,model.file="Ej6b.txt",
               n.iter=50000,n.chains=1,n.burnin=5000)
ej6c.sim<-jags(data,inits,parameters,model.file="Ej6c.txt",
               n.iter=5000,n.chains=1,n.burnin=500)

#-Monitoring chain-

#Traza de la cadena
traceplot(ej6.sim)

#Cadena

#OpenBUGS
out<-ej6b.sim$sims.list

#JAGS
out<-ej6a.sim$BUGSoutput$sims.list

z<-out$tau
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)

#Resumen (estimadores)
#OpenBUGS
out.sum<-ej6a.sim$summary

#JAGS
out.sum<-ej6a.sim$BUGSoutput$summary

print(out.sum)
head(out.sum)

#Probabilidades
z<-out$beta[,1]
prob(z)

#DIC
out.dic<-ej6a.sim$DIC
out.dic<-ej6a.sim$BUGSoutput$DIC
print(out.dic)

#Predictions
out.yf<-out.sum[grep("yf1",rownames(out.sum)),]
ymin<-min(desastres[,2],out.yf[,c(1,3,7)])
ymax<-max(desastres[,2],out.yf[,c(1,3,7)])

par(mfrow=c(1,1))
plot(desastres,type="l",col="grey80",ylim=c(ymin,ymax))
lines(desastres[,1],out.yf[,1],lwd=2,col=2)
lines(desastres[,1],out.yf[,3],lty=2,col=2)
lines(desastres[,1],out.yf[,7],lty=2,col=2)

#Medias
out.mu<-out.sum[grep("mu",rownames(out.sum)),]
par(mfrow=c(1,1))
plot(desastres,type="l",col="grey80")
lines(desastres[,1],out.mu[,1],lwd=2,col=2)
lines(desastres[,1],out.mu[,3],lty=2,col=2)
lines(desastres[,1],out.mu[,7],lty=2,col=2)


#--- Ejemplo 7 ---
#-Reading data-
milk<-read.table("http://allman.rhon.itam.mx/~lnieto/index_archivos/milk.txt",header=TRUE)
milk$t<-1970:1982
n<-nrow(milk)
or<-order(milk$x)
plot(milk$x[or],milk$y[or],type="l")
text(milk$x[or],milk$y[or],labels=milk$t[or],cex=0.5,col=2)
plot(milk$t,milk$y,type="l")
plot(milk$t,milk$x,type="l")


#-Defining data-
m<-2
data<-list("n"=n,"m"=m,"y"=milk$y,"x"=milk$x/max(milk$x),"t"=milk$t/max(milk$t))
data<-list("n"=n,"y"=c(milk$y[1:(n-2)],NA,NA),"x"=milk$x/max(milk$x),"t"=milk$t/max(milk$t))

#-Defining inits-
#inits<-function(){list(beta=0,tau=1,yf1=rep(1,n))}
#inits<-function(){list(beta=rep(0,5),tau=1,yf1=rep(1,n))}
#inits<-function(){list(beta=rep(0,n+m),tau.y=1,tau.b=1,yf1=rep(0,n+m))}
#inits<-function(){list(beta=rep(0,n+m),tau.y=1,yf1=rep(0,n+m))}
#inits<-function(){list(beta=rep(0,n+m),tau.y=1,tau.b=1,yf1=rep(0,n+m),g=0)}
inits<-function(){list(beta=rep(0,n),tau.y=1,tau.b=1,yf1=rep(0,n),g=1)}

#-Selecting parameters to monitor-
#parameters<-c("beta","tau","yf1")
parameters<-c("beta","tau.y","tau.b","yf1","g")
#parameters<-c("beta","tau.y","tau.b","yf1")

#-Running code-
#OpenBUGS
ej7a.sim<-bugs(data,inits,parameters,model.file="Ej7a.txt",
               n.iter=50000,n.chains=2,n.burnin=5000)
ej7b.sim<-bugs(data,inits,parameters,model.file="Ej7b.txt",
               n.iter=50000,n.chains=1,n.burnin=5000)
ej7c.sim<-bugs(data,inits,parameters,model.file="Ej7c.txt",
               n.iter=100000,n.chains=1,n.burnin=10000)
#JAGS
ej7a.sim<-jags(data,inits,parameters,model.file="Ej7a.txt",
              n.iter=50000,n.chains=1,n.burnin=5000)
ej7b.sim<-jags(data,inits,parameters,model.file="Ej7b.txt",
               n.iter=50000,n.chains=1,n.burnin=5000)
ej7c.sim<-jags(data,inits,parameters,model.file="Ej7c.txt",
               n.iter=50000,n.chains=1,n.burnin=5000)

#-Monitoring chain-

#Traza de la cadena
traceplot(ej6.sim)

#Cadena

#OpenBUGS
out<-ej7c.sim$sims.list

#JAGS
out<-ej7a.sim$BUGSoutput$sims.list

z<-out$tau
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)

#Resumen (estimadores)
#OpenBUGS
out.sum<-ej7c.sim$summary

#JAGS
out.sum<-ej7a.sim$BUGSoutput$summary

print(out.sum)
head(out.sum)

#Probabilidades
z<-out$beta[,1]
prob(z)

#DIC
out.dic<-ej7c.sim$DIC
out.dic<-ej7a.sim$BUGSoutput$DIC
print(out.dic)

#Predictions
out.yf<-out.sum[grep("yf1",rownames(out.sum)),]
ymin<-min(milk$y,out.yf[,c(1,3,7)])
ymax<-max(milk$y,out.yf[,c(1,3,7)])
xmin<-min(milk$t)
xmax<-max(milk$t+m)

#x vs. y
par(mfrow=c(1,1))
plot(milk$x,milk$y,type="p",col="grey50",ylim=c(ymin,ymax))
points(milk$x,out.yf[,1],col=2,pch=16,cex=0.5)
segments(milk$x,out.yf[,3],milk$x,out.yf[,7],col=2)

#t vs y
par(mfrow=c(1,1))
plot(milk$t,milk$y,type="b",col="grey80",ylim=c(ymin,ymax),xlim=c(xmin,xmax))
lines(milk$t,out.yf[1:n,1],col=2)
lines(milk$t,out.yf[1:n,3],col=2,lty=2)
lines(milk$t,out.yf[1:n,7],col=2,lty=2)
lines(milk$t[n]:(milk$t[n]+m),out.yf[n:(n+m),1],col=4)
lines(milk$t[n]:(milk$t[n]+m),out.yf[n:(n+m),3],col=4,lty=2)
lines(milk$t[n]:(milk$t[n]+m),out.yf[n:(n+m),7],col=4,lty=2)

#betas
out.beta<-out.sum[grep("beta",rownames(out.sum)),]
ymin<-min(out.beta[,c(1,3,7)])
ymax<-max(out.beta[,c(1,3,7)])
plot(out.beta[,1],type="l",ylim=c(ymin,ymax))
lines(out.beta[,3],lty=2)
lines(out.beta[,7],lty=2)


#--- Ejemplo 8 ---
#-Reading data-
mercado<-read.table("http://allman.rhon.itam.mx/~lnieto/index_archivos/mercado.txt",header=TRUE)
n<-nrow(mercado)

pairs(mercado)
cor(mercado)

#-Defining data-
data<-list("n"=n,"y"=mercado$SHARE,"x1"=mercado$PRICE,"x2"=mercado$OPROM,"x3"=mercado$CPROM)

#-Defining inits-
inits<-function(){list(alpha=0,beta=rep(0,3),tau=1,yf1=rep(1,n))}

#-Selecting parameters to monitor-
parameters<-c("alpha","beta","tau","yf1")

#-Running code-
#OpenBUGS
ej8a.sim<-bugs(data,inits,parameters,model.file="Ej8a.txt",
               n.iter=50000,n.chains=1,n.burnin=5000)
#JAGS
ej8a.sim<-jags(data,inits,parameters,model.file="Ej8a.txt",
               n.iter=50000,n.chains=1,n.burnin=5000)

#-Monitoring chain-

#Cadena

#OpenBUGS
out<-ej8a.sim$sims.list

#JAGS
out<-ej8a.sim$BUGSoutput$sims.list

z<-out$alpha
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)

#Resumen (estimadores)
#OpenBUGS
out.sum<-ej8a.sim$summary

#JAGS
out.sum<-ej8a.sim$BUGSoutput$summary

print(out.sum)
head(out.sum)

#Probabilidades
z<-out$beta[,1]
prob(z)

#DIC
out.dic<-ej8a.sim$DIC
#out.dic<-ej8a.sim$BUGSoutput$DIC
print(out.dic)

#Predictions
out.yf<-out.sum[grep("yf1",rownames(out.sum)),]
y<-mercado$SHARE
ymin<-min(y,out.yf[,c(1,3,7)])
ymax<-max(y,out.yf[,c(1,3,7)])

#x1 vs. y
x<-mercado$PRICE
par(mfrow=c(1,1))
plot(x,y,type="p",col="grey50",ylim=c(ymin,ymax))
points(x,out.yf[,1],col=2,pch=16,cex=0.5)
segments(x,out.yf[,3],x,out.yf[,7],col=2)
#x2 vs. y
x<-mercado$OPROM
par(mfrow=c(1,1))
plot(x,y,type="p",col="grey50",ylim=c(ymin,ymax))
points(x,out.yf[,1],col=2,pch=16,cex=0.5)
segments(x,out.yf[,3],x,out.yf[,7],col=2)
#x3 vs. y
x<-mercado$CPROM
par(mfrow=c(1,1))
plot(x,y,type="p",col="grey50",ylim=c(ymin,ymax))
points(x,out.yf[,1],col=2,pch=16,cex=0.5)
segments(x,out.yf[,3],x,out.yf[,7],col=2)


#--- Ejemplo 10 ---
#-Reading data-
reclama<-read.table("http://allman.rhon.itam.mx/~lnieto/index_archivos/reclama.txt",header=TRUE)
n<-nrow(reclama)
par(mfrow=c(2,2))
plot(reclama$r)
plot(reclama$n,ylim=c(0,max(reclama$n)))
plot(reclama$r/reclama$n)

#-Defining data-
data<-list("n"=n,"y"=reclama$r,"ne"=reclama$n)

#-Defining inits-
inits<-function(){list(p=0.5,yf1=rep(1,n))}
inits<-function(){list(p=rep(0.5,n),yf1=rep(1,n))}
inits<-function(){list(p=rep(0.5,n),a=1,b=1,yf1=rep(1,n))}

#-Selecting parameters to monitor-
parameters<-c("p","yf1")
parameters<-c("p","eta","yf1")

#-Running code-
#OpenBUGS
ej10a.sim<-bugs(data,inits,parameters,model.file="Ej10a.txt",
                n.iter=50000,n.chains=1,n.burnin=5000)
ej10b.sim<-bugs(data,inits,parameters,model.file="Ej10b.txt",
                n.iter=50000,n.chains=1,n.burnin=5000)
ej10c.sim<-bugs(data,inits,parameters,model.file="Ej10c.txt",
                n.iter=100000,n.chains=1,n.burnin=10000)

#JAGS
ej10a.sim<-jags(data,inits,parameters,model.file="Ej10a.txt",
                n.iter=50000,n.chains=1,n.burnin=5000)
ej10b.sim<-jags(data,inits,parameters,model.file="Ej10b.txt",
                n.iter=50000,n.chains=1,n.burnin=5000)
ej10c.sim<-jags(data,inits,parameters,model.file="Ej10c.txt",
                n.iter=100000,n.chains=1,n.burnin=10000)

#-Monitoring chain-

#Traza de la cadena
traceplot(ej10.sim)

#Cadena

#OpenBUGS
outa<-ej10a.sim$sims.list
outb<-ej10b.sim$sims.list
outc<-ej10c.sim$sims.list

#JAGS
outa<-ej10a.sim$BUGSoutput$sims.list
outb<-ej10b.sim$BUGSoutput$sims.list
outc<-ej10c.sim$BUGSoutput$sims.list

z<-outc$p[,9]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)

#Resumen (estimadores)
#OpenBUGS
outa.sum<-ej10a.sim$summary
outb.sum<-ej10b.sim$summary
outc.sum<-ej10c.sim$summary

#JAGS
outa.sum<-ej10a.sim$BUGSoutput$summary
outb.sum<-ej10b.sim$BUGSoutput$summary
outc.sum<-ej10c.sim$BUGSoutput$summary

print(outa.sum)
head(outa.sum)

#Probabilidades
z<-out$beta[,1]
prob(z)

#DIC
#OpenBUGS
outa.dic<-ej10a.sim$DIC
outb.dic<-ej10b.sim$DIC
outc.dic<-ej10c.sim$DIC

#JAGS
outa.dic<-ej10a.sim$BUGSoutput$DIC
outb.dic<-ej10b.sim$BUGSoutput$DIC
outc.dic<-ej10c.sim$BUGSoutput$DIC

print(outa.dic)
print(outb.dic)
print(outc.dic)

#Estimaciones
outa.p<-outa.sum[grep("p",rownames(outa.sum)),]
outb.p<-outb.sum[grep("p",rownames(outb.sum)),]
outc.p<-outc.sum[grep("p",rownames(outc.sum)),]
outc.eta<-outc.sum[grep("eta",rownames(outc.sum)),]

#x vs. y
xmin<-0
xmax<-12
ymin<-0
ymax<-1
par(mfrow=c(1,1))
plot(reclama$r/reclama$n,type="p",col="grey50",xlim=c(xmin,xmax),ylim=c(ymin,ymax))
#
out.p<-outb.p
points(out.p[,1],col=2,pch=16,cex=0.5)
segments(1:10,out.p[,3],1:10,out.p[,7],col=2)
#
out.p<-outc.p
points((1:10)+0.2,out.p[,1],col=4,pch=16,cex=0.5)
segments((1:10)+0.2,out.p[,3],(1:10)+0.2,out.p[,7],col=4)
#
out.p<-outa.p
points(xmax-0.2,out.p[1],col=3,pch=16,cex=0.5)
segments(xmax-0.2,out.p[3],xmax-0.2,out.p[7],col=3)
#
out.p<-outc.eta
points(xmax,out.p[1],col=4,pch=16,cex=0.5)
segments(xmax,out.p[3],xmax,out.p[7],col=4)

