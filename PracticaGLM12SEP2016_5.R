### ----- REGRESION AVANZADA ----- ###
# --- Prof. Luis E. Nieto Barajas --- #

install.packages("R2OpenBUGS")
install.packages("R2jags")
library(R2OpenBUGS)
library(R2jags)

#-Working directory-
wdir<-"/Users/guillemontanari/ITAM/GLM - Nieto 2016"
setwd(wdir)


#--- Ejemplo 2--- INCISO a
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
#ej2.sim<-bugs(data,inits,parameters,model.file="Ej2.txt",
              n.iter=5000,n.chains=1,n.burnin=500)
#JAGS
ej2.sim<-jags(data,inits,parameters,model.file="Ej2_1.txt",
              n.iter=5000,n.chains=1,n.burnin=500)

#-Monitoring chain-

#Traza de la cadena
traceplot(ej2.sim)

#Cadena

#OpenBUGS
#out<-ej2.sim$sims.list

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
#out.sum<-ej2.sim$summary

#JAGS
out.sum<-ej2.sim$BUGSoutput$summary
print(out.sum)

#mean        sd       2.5%        25%       50%       75%     97.5%
#deviance  75.62946  1.541495  74.036597  74.449334  75.15015  76.31266  79.67552
#mu       205.51886  2.895125 199.858253 203.659342 205.60011 207.49962 211.05460
#sig       10.55915  2.016374   7.378339   9.102452  10.33714  11.75082  15.22267
#x1       205.39282 11.646964 183.709996 198.144865 205.38865 212.71173 228.58531

#DIC
#OpenBUGS
#out.dic<-ej2.sim$DIC

#JAGS
out.dic<-ej2.sim$BUGSoutput$DIC
print(out.dic)
[1] 76.81757

