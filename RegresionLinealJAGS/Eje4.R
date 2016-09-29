install.packages("R2OpenBUGS")
install.packages("R2jags")
library(R2OpenBUGS)
library(R2jags)

#-Working directory-
wdir<-"/Users/guillemontanari/ITAM/github/GLM2016"
setwd(wdir)

# Cargo datos

salarios.df <- read.table("salarios.csv", header = TRUE, sep=",")
summary(salarios.df)

n=nrow(salarios.df)

# Definir datos
data <- list("n" = n,"y"=salarios.df$Y,"x1"=salarios.df$X1,"x2"=salarios.df$X2,"x3"=salarios.df$X3)
#
# Opcion 1:Usamos Normal
#
# Definir inits
inits<- function(){list(beta=rep(0,4),tau=1,yf=rep(0,3))}

#Parametros a monitorear
parametros<-c("beta","tau","yf")

#Invoco JAGS
ej4.sim <-jags(data,inits,parametros,model.file = "Ej4_1.txt",n.iter = 500000, n.chains=1, n.burnin=100000) 
out<-ej4.sim$BUGSoutput$sims.list

# Análisis gráfico
traceplot(ej4.sim)

# Analisis grafico de las betas
z<-out$beta[,1]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)

#Puntos de dispersion de las betas
z<-out$beta
par(mfrow=c(1,1))
plot(z)

# Monitoreo de los parametros del modelo
out.sum<-ej4.sim$BUGSoutput$summary
print(out.sum)
out.dic<-ej4.sim$BUGSoutput$DIC
print(out.dic)

#
# Opcion 2: Usamos exponencial
#
#Invoco JAGS
ej4.sim <-jags(data,inits,parametros,model.file = "Ej4_2.txt",n.iter = 5000, n.chains=1, n.burnin=100) 
out<-ej4.sim$BUGSoutput$sims.list

# Análisis gráfico
traceplot(ej4.sim)

# Analisis grafico de las betas
z<-out$beta[,1]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)

#Puntos de dispersion de las betas
z<-out$beta
par(mfrow=c(1,1))
plot(z)

# Monitoreo de los parametros del modelo
out.sum<-ej4.sim$BUGSoutput$summary
print(out.sum)
out.dic<-ej4.sim$BUGSoutput$DIC
print(out.dic)

#
# Opcion 3: Normalizamos datos y aplicamos normal
#
salarios.df2 <- scale(salarios.df)
summary(salarios.df2)

n=nrow(salarios.df2)

# Definir datos
data <- list("n" = n,"y"=salarios.df2$Y,"x1"=salarios.df2$X1,"x2"=salarios.df2$X2,"x3"=salarios.df2$X3)


data
# Definir inits
inits<- function(){list(beta=rep(0,4),tau=1,yf=rep(0,3))}

#Parametros a monitorear
parametros<-c("beta","tau","yf")

#Invoco JAGS
ej4.sim <-jags(data,inits,parametros,model.file = "Ej4_1.txt",n.iter = 5000, n.chains=1, n.burnin=100) 
out<-ej4.sim$BUGSoutput$sims.list

# Análisis gráfico
traceplot(ej4.sim)

# Analisis grafico de las betas
z<-out$beta[,1]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)

#Puntos de dispersion de las betas
z<-out$beta
par(mfrow=c(1,1))
plot(z)

# Monitoreo de los parametros del modelo
out.sum<-ej4.sim$BUGSoutput$summary
print(out.sum)
out.dic<-ej4.sim$BUGSoutput$DIC
print(out.dic)

#
# Opcion 4: Normalizamos datos y aplicamos exponencial
#
# Definir datos
data <- list("n" = n,"y"=salarios.df2$Y,"x1"=salarios.df2$X1,"x2"=salarios.df2$X2,"x3"=salarios.df2$X3)

# Definir inits
inits<- function(){list(beta=rep(0,4),tau=1,yf=rep(0,3))}

#Parametros a monitorear
parametros<-c("beta","tau","yf")

#Invoco JAGS
ej4.sim <-jags(data,inits,parametros,model.file = "Ej4_2.txt",n.iter = 5000, n.chains=1, n.burnin=100) 
out<-ej4.sim$BUGSoutput$sims.list

# Análisis gráfico
traceplot(ej4.sim)

# Analisis grafico de las betas
z<-out$beta[,1]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)

#Puntos de dispersion de las betas
z<-out$beta
par(mfrow=c(1,1))
plot(z)

# Monitoreo de los parametros del modelo
out.sum<-ej4.sim$BUGSoutput$summary
print(out.sum)
out.dic<-ej4.sim$BUGSoutput$DIC
print(out.dic)

#
# Opcion 5: las betas son normales(.5,.5)
#
# Definir datos
# data <- list("n" = n,"y"=salarios.df2$Y,"x1"=salarios.df2$X1,"x2"=salarios.df2$X2,"x3"=salarios.df2$X3)
data <- list("n" = n,"y"=salarios.df2[,1],"x1"=salarios.df2[,2],"x2"=salarios.df2[,3],"x3"=salarios.df2[,4])

# Definir inits
inits<- function(){list(beta=rep(0,4),tau=1,yf=rep(0,3))}

#Parametros a monitorear
parametros<-c("beta","tau","yf")

#Invoco JAGS
ej4.sim <-jags(data,inits,parametros,model.file = "Ej4_3.txt",n.iter = 500000, n.chains=1, n.burnin=10000) 
out<-ej4.sim$BUGSoutput$sims.list

# Análisis gráfico
traceplot(ej4.sim)

# Analisis grafico de las betas
z<-out$beta[,1]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)

#Puntos de dispersion de las betas
z<-out$beta
par(mfrow=c(1,1))
plot(z)

# Monitoreo de los parametros del modelo
out.sum<-ej4.sim$BUGSoutput$summary
print(out.sum)
out.dic<-ej4.sim$BUGSoutput$DIC
print(out.dic)
