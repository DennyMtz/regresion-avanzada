#-Working directory-

wdir<-""
wdir<-"/home/denny/github/regresion-avanzada/codigo/m1-5"
setwd(wdir)

library(R2OpenBUGS)
library(dplyr)
library(readr)
library(R2jags)
#--- Funciones utiles ---
prob<-function(x){
  out<-min(length(x[x>0])/length(x),length(x[x<0])/length(x))
  out
}

tabla_4anios_mods <- read.csv("../../datos/tabla_4anios_mods.csv")

#Transformamos las variables explicativas en %
########### El ifelse checa si el denominador es 0 y en ese caso pone 0 a la proporción
tabla_4anios_mods$POR_VPH_PISODT <- ifelse(tabla_4anios_mods$VIVPAR_HAB==0,0,tabla_4anios_mods$VPH_PISODT/tabla_4anios_mods$VIVPAR_HAB)
tabla_4anios_mods$POR_VPH_AUTOM <- ifelse(tabla_4anios_mods$VIVPAR_HAB==0,0,tabla_4anios_mods$VPH_AUTOM/tabla_4anios_mods$VIVPAR_HAB)


tabla <- tabla_4anios_mods
variables <- c("homi_count","POBTOT","PROM_OCUP","POR_VPH_PISODT","POR_VPH_AUTOM","bin_af", "bin_ab")
datos <- tabla %>% select(one_of(variables))

#-Defining data-
n <- nrow(datos)*1
unos <- rep(1,n)

#poisson y bin neg - exposure es el offset de POP_TOT
data<-list("n"=n,"y"=datos$homi_count,"exposure"=datos$POBTOT,"x1"= datos$POR_VPH_AUTOM, "x2"=datos$bin_ab, "x3"=datos$bin_af, 
           "x4"=(datos$POR_VPH_AUTOM*datos$bin_ab),"x5"=(datos$POR_VPH_AUTOM*datos$bin_af),"z1"=datos$bin_ab, "z2"=datos$bin_af)

#-Defining inits-
inits<-function(){list(beta=rep(1,6),gamma=rep(1,3),ypred=rep(1,n))}

#-Selecting parameters to monitor-
parameters<-c("beta","gamma","ypred")

#-Running code-
#OpenBUGS
zip_1_5.sim<-bugs(data,inits,parameters,model.file="ZIPoisson_1-5.txt",
                  n.iter=50000,n.chains=1,n.burnin=5000)

out_zip_1_5.sim<-zip_1_5.sim$sims.list

traceplot(zip_1_5.sim)
plot(zip_1_5.sim)


#Resumen (estimadores)
#OpenBUGS
out.sum_zip_1_5.sim <- zip_1_5.sim$summary
#print(out.sum_zip_1_5.sim)
head(out.sum_zip_1_5.sim)
write.csv(out.sum_zip_1_5.sim,"out.sum_zip_1_5.sim.csv")
#DIC
zip_1_5.sim.dic<-zip_1_5.sim$DIC
print(zip_1_5.sim.dic) #8.4e+11

out.ypred<-out.sum_zip_1_5.sim [grep("ypred",rownames(out.sum_zip_1_5.sim )),]
cor(datos$homi_count,out.ypred[,1])
#0.6046126

######################## Analisis MCMC para beta1
beta_num <- 1
z<-out_zip_1_5.sim$beta[,beta_num]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)
############### p-value
prob(z) #0

######################## Analisis MCMC para beta2
beta_num <- 2
z<-out_zip_1_5.sim$beta[,beta_num]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)
############### p-value
prob(z) #0

######################## Analisis MCMC para beta 3
beta_num <- 3
z<-out_zip_1_5.sim$beta[,beta_num]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)
############### p-value
prob(z) #0

######################## Analisis MCMC para gamma 1
gamma_num <- 1
z<-out_zip_1_5.sim$gamma[,gamma_num]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)
############### p-value
prob(z)  #0.1359333

######################## Analisis MCMC para gamma 2
gamma_num <- 2
z<-out_zip_1_5.sim$gamma[,gamma_num]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)
############### p-value
prob(z) #0.4323556

######################## Analisis MCMC para gamma 3
gamma_num <- 3
z<-out_zip_1_5.sim$gamma[,gamma_num]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)
############### p-value
prob(z) #0.1391556


############################3
#Predictions
out.ypred<-out.ypred<-out.sum_zip_1_5.sim [grep("ypred",rownames(out.sum_zip_1_5.sim )),]
or<-order(datos$POR_VPH_AUTOM)
ymin<-min(datos$homi_count,out.ypred[,c(1,3,7)])
ymax<-max(datos$homi_count,out.ypred[,c(1,3,7)])
par(mfrow=c(1,1))
plot(datos$POR_VPH_AUTOM,datos$homi_count,ylim=c(ymin,ymax))
lines(datos$POR_VPH_AUTOM[or],out.ypred[or,1],lwd=2,col=2)
lines(datos$POR_VPH_AUTOM[or],out.ypred[or,3],lty=2,col=3)
lines(datos$POR_VPH_AUTOM[or],out.ypred[or,7],lty=2,col=3)


out.ypred<-out.ypred<-out.sum_zip_1_5.sim [grep("ypred",rownames(out.sum_zip_1_5.sim )),]
or<-order(datos$PROM_OCUP)
ymin<-min(datos$homi_count,out.ypred[,c(1,3,7)])
ymax<-max(datos$homi_count,out.ypred[,c(1,3,7)])
par(mfrow=c(1,1))
plot(datos$PROM_OCUP,datos$homi_count,ylim=c(ymin,ymax))
lines(datos$PROM_OCUP[or],out.ypred[or,1],lwd=2,col=2)
lines(datos$PROM_OCUP[or],out.ypred[or,3],lty=2,col=3)
lines(datos$PROM_OCUP[or],out.ypred[or,7],lty=2,col=3)

plot(datos$homi_count,out.ypred[,1])
