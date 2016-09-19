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

# Inciso c: 
# Determina la distribución inicial de referencia. Es otro analisis conjugado, pero en este caso 
# no es informativa, a diferencia de los incisos a y b.
# Es decir usar la dist inicial de referencia para theta, que tiene un modelo Ber.
# La buscamos en el formulario (procesos inferenciales)
# Beta(1/2, 1/2)

# Para este ejercicio usamos el archivo Eje1_3.txt
# que contiene los datos, modelo, parametros para Jags
# Que le doy a Jags:
# 1. La verosimilitud
# 2. La inicial de theta
# 3. Le doy la primera parte parte de la predictiva, jags marginaliza por mi y me da la predcitiva final
# 4. La dist final de theta, me la da Jags
#
#--- Ejemplo 1.c---
#-Reading data-
n<-100
credito<-c(rep(1,n/2),rep(0,n/2))

#-Defining data-
# Los datos tienen que venir en formato Lista para que Jags los pueda leer
# Los nombres entre comillas es el del paquete R2jags y los que no los de R
data<-list("n"=n,"x"=credito)

#-Defining inits-
# Los puntos iniciales de la cadena es todo lo que tiene tilde

# Inciso a y c: Como theta y x1 tienen tilde en archivo de Ej1_3.txt los tengo que inicializar
inits<-function(){list(theta=0.5,x1=rep(1,2))}

# Inciso b: No vamos a hacer la prueba de que pasa si no inicializo o doy valores a x1
#inits<-function(){list(lambda=0)}
#inits<-function(){list(theta=0.5,eta=1)}

#-Selecting parameters to monitor-
parameters<-c("theta","x1")
#parameters<-c("theta","eta")

#-Running code-
#OpenBUGS
#ej1.sim<-bugs(data,inits,parameters,model.file="Ej1.txt",
#              n.iter=5000,n.chains=1,n.burnin=500)
#JAGS
ej1.sim<-jags(data,inits,parameters,model.file="Ej1_3.txt",
              n.iter=5000,n.chains=1,n.burnin=500)

#-Monitoring chain-

#Traza de la cadena
dev.off
traceplot(ej1.sim)

#Como no es un análisis conjugado, se mueve mas lentamente
#Cadena

#OpenBUGS
#out<-ej1.sim$sims.list

#JAGS
out<-ej1.sim$BUGSoutput$sims.list
names(out)
#Para ver Theta
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

#                  mean         sd       2.5%         25%         50%        75%       97.5%
#deviance 139.6420193 1.42855298 138.630366 138.7226475 139.0596732 139.986878 143.6907289
#theta      0.4987939 0.04994522   0.401548   0.4659379   0.4997497   0.530386   0.5965978
#x1[1]      0.5191111 0.49985684   0.000000   0.0000000   1.0000000   1.000000   1.0000000
#x1[2]      0.4657778 0.49904931   0.000000   0.0000000   0.0000000   1.000000   1.0000000
> 
#OpenBUGS
#out.dic<-ej1.sim$DIC

#JAGS
out.dic<-ej1.sim$BUGSoutput$DIC
print(out.dic)
#DIC
#[1] 140.6624

