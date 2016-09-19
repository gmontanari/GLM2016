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

# Inciso b: 
# Usando la informaciòn del año pasado encuentre la distribución normal transformada que mejor
# describe el conocimiento inicial
# Vamos a cambiar de modelo, que no sea conjugada. 
# Vamos a armar una función logística tal que transformo al parametro, que esta en los reales positivos, 
# con una funciòn que lo lleve a todos los reales y asi poder usar una funciòn Normal.
# eta=log(theta/(1-theta)), con eta viviendo en los reales
# Para volver a obtener theta=e^eta/(1+e^eta)
# La idea es asignar a eta una dist normal, con ciertos parametros, de tal manera, que si yo la transformo
# me dá otra función, no importa cual, que satisfaga estos parámetros:
# Quiero que theta este alrededor de 0.6, entonces eta esta alrededor de eta=log(0.6/(1-0.6))=0.4054
# ese sera mi Media de eta
# Lo mismo para la desviaciòn estandard, aunque esta no es trivial. El profe hizo un poco de ensayoy error

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
n<-100
credito<-c(rep(1,n/2),rep(0,n/2))

#-Defining data-
# Los datos tienen que venir en formato Lista para que Jags los pueda leer
# Los nombres entre comillas es el del paquete R2jags y los que no los de R
data<-list("n"=n,"x"=credito)

#-Defining inits-
# Los puntos iniciales de la cadena es todo lo que tiene tilde

# Como theta y x1 tienen tilde en archivo de Ej1_1.txt los tengo que inicializar
#inits<-function(){list(theta=0.5,x1=rep(1,2))}

# No vamos a hacer la prueba de que pasa si no inicializo o doy valores a x1
inits<-function(){list(lambda=0)}
#inits<-function(){list(theta=0.5,eta=1)}

#-Selecting parameters to monitor-
parameters<-c("theta","x1")
#parameters<-c("theta","eta")

#-Running code-
#OpenBUGS
#ej1.sim<-bugs(data,inits,parameters,model.file="Ej1.txt",
#              n.iter=5000,n.chains=1,n.burnin=500)
#JAGS
ej1.sim<-jags(data,inits,parameters,model.file="Ej1_2.txt",
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

#                mean        sd        2.5%         25%         50%         75%       97.5%
#deviance 140.3596765 1.6426976 138.6379775 139.0781120 139.8909605 141.1977868 144.1667908
#theta      0.5563588 0.0328662   0.4949282   0.5332139   0.5558874   0.5796186   0.6160477
#x1[1]      0.5351111 0.4989875   0.0000000   0.0000000   1.0000000   1.0000000   1.0000000
#x1[2]      0.5457778 0.4981214   0.0000000   0.0000000   1.0000000   1.0000000   1.0000000

#OpenBUGS
#out.dic<-ej1.sim$DIC

#JAGS
out.dic<-ej1.sim$BUGSoutput$DIC
print(out.dic)
#DIC
#141.7089

