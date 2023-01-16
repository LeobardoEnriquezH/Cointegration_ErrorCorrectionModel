#ECONOMETRIA II

#cp: Indicador Mensual del Consumo Privado en el Mercado Interior
#igae: Indicador Global de la Actividad Económica. Índice de volumen físico 
#tcr: Índice de tipo de cambio real

#Cambiar el directorio de trabajo
setwd("C:\\Users\\Leobardo\\Documents\\GitHub\\Cointegration_ErrorCorrectionModel")

# Lectura de la base de datos
Consumo <- read.csv("modelo_consumo.csv")
attach(Consumo)
summary(Consumo)
#PARA VER LA BASE DE DATOS
View(Consumo)

# Establecer que las variables son series de tiempo
cp <- ts(cp, frequency=12, start=c(1993,1))
igae <- ts(igae, frequency=12, start=c(1993,1))
tcr <- ts(tcr, frequency=12, start=c(1993,1))

# Aplicar el logaritmo y primera diferencia a las variables 
lcp <- log(cp)
ligae <- log(igae)
ltcr <- log(tcr)
dcp  <- diff(cp)
digae  <- diff(igae)
dtcr  <- diff(tcr)
dlcp  <- diff(log(cp))
dligae  <- diff(log(igae))
dltcr  <- diff(log(tcr))

#Librerías a utilizar 
library(urca)
library(car)
library(tseries)

# Grafica de las variables 

plot.ts(cp)
plot.ts(igae)
plot.ts(tcr)

plot.ts(lcp)
plot.ts(ligae)
plot.ts(ltcr)

plot.ts(dcp)
plot.ts(digae)
plot.ts(dtcr)

plot.ts(dlcp)
plot.ts(dligae)
plot.ts(dltcr)

#Pruebas de raíz unitaria

#H0: Yt NO es estacionaria
#Ha: Yt es estacionaria


################### cp ###############################
#PH DFA GASTO GOBIERNO 
lc.df <- ur.df(y=cp, type='trend', lags=4, selectlags=c("AIC"))
summary(lc.df)

lc.df <- ur.df(y=cp, type='drift', lags=4, selectlags=c("AIC"))
summary(lc.df)

lc.df <- ur.df(y=cp, type='none', lags=4, selectlags=c("AIC"))
summary(lc.df) 

#####Sin rezagos

lc.df <- ur.df(y=cp, type='trend', selectlags=c("AIC"))
summary(lc.df)

lc.df <- ur.df(y=cp, type='drift', selectlags=c("AIC"))
summary(lc.df)

lc.df <- ur.df(y=cp, type='none', selectlags=c("AIC"))
summary(lc.df) 

############



################REGRESIÓN#########################
#Detección gráfica cointegración

summary(Consumo)

plot(igae, main="Consumo privado e IGAE", ylim=c(54.6,118.3))
lines(igae, col="black")
lines(cp, col="red")

summary(lcp)
summary(ligae)

plot(ligae, main="LNConsumo privado y LNIGAE", ylim=c(4,5))
lines(ligae, col="black")
lines(lcp, col="red")


plot(tcr, main="Consumo privado y tipo de cambio real", ylim=c(54.6,138.2))
lines(tcr, col="black")
lines(cp, col="red")

summary(lcp)
summary(ltcr)

plot(ltcr, main="LNConsumo privado y LNtipo de cambio real", ylim=c(4,5))
lines(ltcr, col="black")
lines(lcp, col="red")

#ESTABLECER VARS.DEPENDIENTES, INDEPENDIENTES
# Funciones para probar cointegracion tipo Granger
mod_lin <- lm(cp ~ igae)
summary(mod_lin)

mod_log <- lm(lcp ~ ligae)
summary(mod_log)

# Generar los residuales 
res_lin <- residuals.lm(mod_lin)
summary(res_lin)
res_log <- residuals.lm(mod_log)
summary(res_log)

#Prueba Durbin Watson 
library(lmtest)
dwtest(formula = mod_lin)
dwtest(formula = mod_log)

#Gráficas residuales
plot(res_lin)
plot(res_log)

qqnorm(res_lin)
qqline(res_lin)

qqnorm(res_log)
qqline(res_log)

#########Realizar pruebas de raíz unitaria de los residuales de ambos modelos
# Prueba de Phillips y Ouliaris para cointegracion 
#modelo_lineal
ecb.cp <- cbind(cp, igae)

Lc.po <- ca.po(ecb.cp, type="Pz")
summary(Lc.po)

Lc.po <- ca.po(ecb.cp, type="Pu")
summary(Lc.po)


#modelo_logaritmico
ecb.lcp <- cbind(lcp, ligae)

Lc.po <- ca.po(ecb.lcp, type="Pz")
summary(Lc.po)

Lc.po <- ca.po(ecb.lcp, type="Pu")
summary(Lc.po)


# Rezagos de los residuales 
res_lin_1 <- res_lin[1:296]
res_log_1 <- res_log[1:296]

res_1.l=lag(res_lin_1)

# Modelos de Correccion de Error (MCE) 
# Lineal
mce_lin <- lm(dcp ~digae+res_lin_1)
summary(mce_lin)

# Log-log
mce_log <- lm(dlcp ~dligae+res_log_1)
summary(mce_log)


#####Modelo múltiple 

#ESTABLECER VARS.DEPENDIENTES, INDEPENDIENTES
# Funciones para probar cointegracion tipo Granger
mod_linm <- lm(cp ~ igae+tcr)
summary(mod_linm)

mod_logm <- lm(lcp ~ ligae+ltcr)
summary(mod_logm)

# Generar los residuales 
res_linm <- residuals.lm(mod_linm)
summary(res_linm)
res_logm <- residuals.lm(mod_logm)
summary(res_logm)

#Prueba Durbin Watson 
library(lmtest)
dwtest(formula = mod_linm)
dwtest(formula = mod_logm)

#Gráficas residuales
plot(res_linm)
plot(res_logm)

qqnorm(res_linm)
qqline(res_linm)

qqnorm(res_logm)
qqline(res_logm)

#########Realizar pruebas de raíz unitaria de los residuales de ambos modelos
# Prueba de Phillips y Ouliaris para cointegracion 
#modelo_lineal
ecb.cpm <- cbind(cp, igae+tcr)

Lc.po <- ca.po(ecb.cpm, type="Pz")
summary(Lc.po)

Lc.po <- ca.po(ecb.cpm, type="Pu")
summary(Lc.po)


#modelo_logaritmico
ecb.lcpm <- cbind(lcp, ligae+ltcr)

Lc.po <- ca.po(ecb.lcpm, type="Pz")
summary(Lc.po)

Lc.po <- ca.po(ecb.lcpm, type="Pu")
summary(Lc.po)


# Rezagos de los residuales 
res_linm_1 <- res_linm[1:296]
res_logm_1 <- res_logm[1:296]

res_1.l=lag(res_lin_1)

# Modelos de Correccion de Error (MCE) 
# Lineal
mce_linm <- lm(dcp ~digae+dtcr+res_linm_1)
summary(mce_linm)

# Log-log
mce_logm <- lm(dlcp ~dligae+dltcr+res_logm_1)
summary(mce_logm)




