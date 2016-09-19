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

# Usando la informaciòn del año pasado describa la mejor beta que mejor describa
# ese conocimiento inicial
# Utilice un modelo Beta(a,b)
# cuanto vale a y b tal que se satisfaga la dist inicial
# E(theta)= a/(a+b)=0.6
# Var(theta)= (a*a)/((a+b)^2*(a+b+1))=(0.04)^2
# despejamos y a=89.4 b=59.6

# Para este ejercicio usamos el archivo Eje1.txt
# que contiene los datos, modelo, parametros para Jags
# Que le doy a Jags:
# 1. La verosimilitud
# 2. La inicial de theta
# 3. Le doy la primera parte parte de la predictiva, jags marginaliza por mi y me da la predcitiva final
# 4. La dist final de theta, me la da Jags
#
#--- Ejemplo 1---
#-Reading data-
n<-100
credito<-c(rep(1,n/2),rep(0,n/2))

#-Defining data-
# Los datos tienen que venir en formato Lista para que Jags los pueda leer
# Los nombres entre comillas es el del paquete R2jags y los que no los de R
data<-list("n"=n,"x"=credito)

#-Defining inits-
# Los puntos iniciales de la cadena

inits<-function(){list(theta=0.5,x1=rep(1,2))}
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
ej1.sim<-jags(data,inits,parameters,model.file="Ej1_1.txt",
              n.iter=5000,n.chains=1,n.burnin=500)

#-Monitoring chain-

#Traza de la cadena
dev.off
traceplot(ej1.sim)

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

#                mean         sd        2.5%         25%         50%         75%       97.5%
#deviance 140.5116685 1.69485341 138.6364268 139.2399886 140.0750683 141.2718391 144.8527702
#theta      0.5603297 0.03143026   0.4989016   0.5390094   0.5599006   0.5807434   0.6228173
#x1[1]      0.5484444 0.49786893   0.0000000   0.0000000   1.0000000   1.0000000   1.0000000
#x1[2]      0.5404444 0.49858320   0.0000000   0.0000000   1.0000000   1.0000000   1.0000000

#DIC
#OpenBUGS
#out.dic<-ej1.sim$DIC

#JAGS
out.dic<-ej1.sim$BUGSoutput$DIC
print(out.dic)
# [1] 141.9479
