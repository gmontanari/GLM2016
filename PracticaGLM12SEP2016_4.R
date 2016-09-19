### ----- REGRESION AVANZADA ----- ###
# --- Prof. Luis E. Nieto Barajas --- #

#-Muestreador de Gibbs-
#install.packages("bayesm")
#library(bayesm)
# Si vemos las graficas, en este caso me muevo mas lentamente porque los datos
# estan muy correlacionados entre si 0.95
#out<-rbiNormGibbs(rho=.95)
# Si vemos las graficas, en este caso me muevo mas rápido porque los datos
# NO estan muy correlacionados entre si -0.5, entonces los saltos son mas grandes
# y recorro las gráficas más rápido.
#out<-rbiNormGibbs(rho=-0.5)

###########################################
# Como hacre inferencia Bayesiana con R2jags

install.packages("R2OpenBUGS")
install.packages("R2jags")
library(R2OpenBUGS)
library(R2jags)

#-Working directory-
wdir<-"/Users/guillemontanari/ITAM/github/GLM2016"
setwd(wdir)

# Tenemos 100 = n creditos observados en el 2005
# Se han otorgado 50, entonces tengo exitos (otorgados) y fracasos (no otorgados)
# Creditos siguen una dis Ber(theta) -> Dist Inicial 
# Como tengo 50 creditos otorgados, 50 es la suma de los "exitos" del 2005
# 50 = suma de las xi's, es un resumen, es una estadistica suficiente
# En la historia del Banco, info subjetiva que puedo usar o no:
# en el 2004 la tasa promedio de exitos es del 0.6 (60%) Esperanza(theta)
# la desviacion estandard era de 0.04 de theta

# Inciso extra: 
# Qué sucede si hago una mezcla de Betas: 
# 0.3*Beta(theta/10,10)+ 0.7*Beta(theta, 6,0.01)
# ambos parametros "empujan" hacia el centro.


# Para este ejercicio usamos el archivo Eje1_2.txt
# que contiene los datos, modelo, parametros para Jags
# Que le doy a Jags:
# 1. La verosimilitud
# 2. La inicial de theta
# 3. Le doy la primera parte parte de la predictiva, jags marginaliza por mi y me da la predcitiva final
# 4. La dist final de theta, me la da Jags
#
#--- Ejemplo 1.b---
#-Reading data-
#n<-100
# EJEMPLO CON POCOS DATOS Y MI THETA GORRO CARGADO EN EL VALOR 9
N<-10
#credito<-c(rep(1,n/2),rep(0,n/2))
credito<-c(rep(1,9),rep(0,n/2))

#-Defining data-
# Los datos tienen que venir en formato Lista para que Jags los pueda leer
# Los nombres entre comillas es el del paquete R2jags y los que no los de R
data<-list("n"=n,"x"=credito)

#-Defining inits-
# Los puntos iniciales de la cadena es todo lo que tiene tilde

# Como theta y x1 tienen tilde en archivo de Ej1_1.txt los tengo que inicializar
#inits<-function(){list(theta=0.5,x1=rep(1,2))}

# No vamos a hacer la prueba de que pasa si no inicializo o doy valores a x1
#inits<-function(){list(lambda=0)}
inits<-function(){list(theta=0.5,eta=1)}

#-Selecting parameters to monitor-
#parameters<-c("theta","x1")
parameters<-c("theta","eta")

#-Running code-
#OpenBUGS
#ej1.sim<-bugs(data,inits,parameters,model.file="Ej1.txt",
#              n.iter=5000,n.chains=1,n.burnin=500)
#JAGS
ej1.sim<-jags(data,inits,parameters,model.file="Ej1_4.txt",
              n.iter=5000,n.chains=1,n.burnin=500)

#-Monitoring chain-

#Traza de la cadena
dev.off
traceplot(ej1.sim)

# ESTE CASO SI MIS DATOS ESTAN CARGADOS HACIA LA DERECHA, HACIA ALLA VAN.
# LA DIST FINAL DEPENDERA TANTO DE LOS DATOS COMO DEL TAMAÑO DE LA MUESTRA

#Como no es un análisis conjugado, se mueve mas lentamente
#Cadena

#OpenBUGS
#out<-ej1.sim$sims.list

#JAGS
out<-ej1.sim$BUGSoutput$sims.list
names(out)
#Para ver LA DISTRIBUCION FINAL DE Theta
z<-out$theta
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)

#Para ver X1(futuras)
x1<-out$x1
dim(x1)
head(x1[,1])
x1[1:20,1]
plot(x1[1:20,1])
plot(x1[1:20,1],type="l")
plot(x1[1:1000,1],type="l")
plot(x1[1:1125,1],type="l")

#Resumen (estimadores)
#OpenBUGS
#out.sum<-ej1.sim$summary

#JAGS
out.sum<-ej1.sim$BUGSoutput$summary
print(out.sum)

#mean         sd        2.5%         25%         50%         75%     97.5%
#deviance 139.4692276 1.17989434 138.6305884 138.7189655 138.9941036 139.7287812 142.77445
#eta        1.0071111 0.08406441   1.0000000   1.0000000   1.0000000   1.0000000   1.00000
#theta      0.4989248 0.04554466   0.4115981   0.4684238   0.4992906   0.5292374   0.58573
> 
#OpenBUGS
#out.dic<-ej1.sim$DIC

#JAGS
out.dic<-ej1.sim$BUGSoutput$DIC
print(out.dic)
#[1] 140.1653
#---------------------#
#Mezcla de betas
w<-seq(0.01,0.99,,100)
fw<-dbeta(w,10,10)+(1-pp)
par(mfrow=c(1,1))
plot(w,fw,type="l")

w<-seq(0.01,0.99,,100)
fw<-dbeta(w,6,0.01)
par(mfrow=c(1,1))
plot(w,fw,type="l")

# DIST INICIAL
w<-seq(0.01,0.99,,100)
pp<-0.3
fw<-pp*dbeta(w,10,10.1)+(1-pp)*dbeta(w,5,0.05)
par(mfrow=c(1,1))
plot(w,fw,type="l")

# DIST FINAL
z<-out$theta
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)

