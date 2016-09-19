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
#              n.iter=5000,n.chains=1,n.burnin=500)
#JAGS
ej2.sim<-jags(data,inits,parameters,model.file="Ej2_2.txt",
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

#             mean        sd       2.5%        25%       50%       75%     97.5%
#deviance  76.21419  2.185113  74.054854  74.634714  75.57600  77.00635  82.26974
#mu       206.73108  3.743996 199.502647 204.460420 206.61534 209.07929 214.74607
#sig       11.17184  2.790158   7.040606   9.242904  10.58812  12.80445  17.75642
#x1       206.38647 11.704830 183.513546 198.932290 206.04779 213.62141 230.04425
 
#DIC
#OpenBUGS
#out.dic<-ej2.sim$DIC

#JAGS
out.dic<-ej2.sim$BUGSoutput$DIC
print(out.dic)
#[1] 78.60155

