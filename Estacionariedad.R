#ECONOMETRIA II

#Cambiar el directorio de trabajo
setwd("C:\\Users\\Leobardo\\Documents\\GitHub\\Cointegration_ErrorCorrectionModel")

# Lectura de la base de datos
Gasto <- read.csv("gasto_gob_eh.csv")
attach(Gasto)
summary(Gasto)
#PARA VER LA BASE DE DATOS
View(Gasto)

#Cargar la libreria urca
library(urca)
library(car)
library(tseries)

# Establecer que las variables son series de tiempo (TS) Y QUE SU FREC. ES ANUAL
gastogob <- ts(gg, frequency=1, start=c(1962))
exportpetro <- ts(xp, frequency=1, start=c(1962))
exportmatprima <- ts(xmp, frequency=1, start=c(1962))


# Aplicar el logaritmo y primera diferencia a las variables 
lgastogob <- log(gastogob)
lexportpetro <- log(exportpetro)
lexportmatprima <- log(exportmatprima)
dgastogob  <- diff(gastogob)
dexportpetro  <- diff(exportpetro)
dexportmatprima <- diff(exportmatprima)
dlgastogob  <- diff(lgastogob)
dlexportpetro  <- diff(lexportpetro)
dlexportmatprima <- diff(lexportmatprima)

# Grafica de las variables 
plot(gastogob, main="Gasto de gobierno", ylim=c(8.091e+07,5.587e+10 ))
lines(exportpetro, col="black")

plot.ts(gastogob)
plot.ts(exportpetro)
plot.ts(exportmatprima)

plot.ts(lgastogob)
plot.ts(lexportpetro)
plot.ts(lexportmatprima)

plot.ts(dgastogob)
plot.ts(dexportpetro)
plot.ts(dexportmatprima)

plot.ts(dlgastogob)
plot.ts(dlexportpetro)
plot.ts(dlexportmatprima)

#Identificación gráfica de estacionariedad
#Autocorrelogramas
acf(gastogob)
acf(exportpetro)
acf(exportmatprima)

acf(lgastogob)
acf(lexportpetro)
acf(lexportmatprima)

acf(dgastogob)
acf(dexportpetro)
acf(dexportmatprima)

acf(dlgastogob)
acf(dlexportpetro)
acf(dlexportmatprima)

#Pruebas de raíz unitaria 
adf.test(dlgastogob)
adf.test(exportpetro)
adf.test(exportmatprima)

###***************##
lc.df <- ur.df(y=gastogob, type='trend',lags=4, selectlags=c("AIC"))
summary(lc.df)

lc.df <- ur.df(y=gastogob, type='drift',lags=4, selectlags=c("AIC"))
summary(lc.df)

lc.df <- ur.df(y=gastogob, type='none',lags=4, selectlags=c("AIC"))
summary(lc.df)

################

lc.df <- ur.df(y=dgastogob, type='trend',lags=4, selectlags=c("AIC"))
summary(lc.df)

lc.df <- ur.df(y=dgastogob, type='drift',lags=4, selectlags=c("AIC"))
summary(lc.df)

lc.df <- ur.df(y=dgastogob, type='none',lags=4, selectlags=c("AIC"))
summary(lc.df)

#Pruebas PP 

lc.pp <- ur.pp(gastogob, type="Z-tau",model="trend", lags="long")
summary(lc.pp)

lc.pp <- ur.pp(gastogob, type="Z-tau",model="constant", lags="long")
summary(lc.pp)

#Pruebas KPSS para cointegracion tipo Granger
lc.kpss <- ur.kpss(gastogob, type="tau", lags="short", use.lag = NULL)
summary(lc.kpss)


#****************
  #ESTABLECER VARS.DEPENDIENTES, INDEPENDIENTES
# Funciones para probar cointegracion tipo Granger
mod_lin <- lm(gastogob ~ exportpetro)
summary(mod_lin)
mod_log <- lm(lgastogob ~ lexportpetro)
summary(mod_log)

# Generar los residuales de la ecuacion
res_lin <- residuals.lm(mod_lin)
summary(res_lin)
res_log <- residuals.lm(mod_log)
summary(res_log)

residualPlot(mod_log)
plot(res_lin)
plot(res_log)
  
#********Realizar pruebas de raíz unitaria de los residuales de ambos modelos 

##########################################################################



###################GASTO DE GOBIERNO###############################
#PH DFA GASTO GOBIERNO
lc.df <- ur.df(y=gastogob, type='trend', selectlags=c("AIC"))
summary(lc.df)

lc.df <- ur.df(y=gastogob, type='drift', selectlags=c("AIC"))
summary(lc.df)

lc.df <- ur.df(y=gastogob, type='none', selectlags=c("AIC"))
summary(lc.df)

#PH PHILLIPS PERRON GASTO GOBIERNO

lc.pp <- ur.pp(gastogob, type="Z-tau", model="trend", lags="long")
summary(lc.pp)

lc.pp <- ur.pp(gastogob, type="Z-tau", model="constant", lags="long")
summary(lc.pp)

#Pruebas KPSS para cointegracion tipo Granger
lc.kpss <- ur.kpss(gastogob, type="tau", lags="short", use.lag = NULL)
summary(lc.kpss)


###################LN GASTO DE GOBIERNO###############################

#PH DFA LN GASTO GOBIERNO
lc.df <- ur.df(y=lgastogob, type='trend', selectlags=c("AIC"))
summary(lc.df)

lc.df <- ur.df(y=lgastogob, type='drift', selectlags=c("AIC"))
summary(lc.df)

lc.df <- ur.df(y=lgastogob, type='none', selectlags=c("AIC"))
summary(lc.df)

#PH PHILLIPS PERRON GASTO GOBIERNO

lc.pp <- ur.pp(lgastogob, type="Z-tau",model="trend", lags="long")
summary(lc.pp)

lc.pp <- ur.pp(lgastogob, type="Z-tau",model="constant", lags="long")
summary(lc.pp)

#Pruebas KPSS para cointegracion tipo Granger
lc.kpss <- ur.kpss(lgastogob, type="tau", lags="short", use.lag = NULL)
summary(lc.kpss)


###################DIF GASTO DE GOBIERNO###############################
#PH DFA GASTO GOBIERNO
lc.df <- ur.df(y=dgastogob, type='trend', selectlags=c("AIC"))
summary(lc.df)

lc.df <- ur.df(y=dgastogob, type='drift', selectlags=c("AIC"))
summary(lc.df)

lc.df <- ur.df(y=dgastogob, type='none', lags=4,selectlags=c("AIC"))
summary(lc.df)

#PH PHILLIPS PERRON GASTO GOBIERNO

lc.pp <- ur.pp(dgastogob, type="Z-tau", model="trend", lags="long")
summary(lc.pp)

lc.pp <- ur.pp(dgastogob, type="Z-tau", model="constant", lags="long")
summary(lc.pp)

#Pruebas KPSS para cointegracion tipo Granger
lc.kpss <- ur.kpss(dgastogob, type="tau", lags="short", use.lag = NULL)
summary(lc.kpss)


###################DIF LN GASTO DE GOBIERNO###############################

#PH DFA LN GASTO GOBIERNO
lc.df <- ur.df(y=dlgastogob, type='trend', selectlags=c("AIC"))
summary(lc.df)

lc.df <- ur.df(y=dlgastogob, type='drift', selectlags=c("AIC"))
summary(lc.df)

lc.df <- ur.df(y=dlgastogob, type='none', selectlags=c("AIC"))
summary(lc.df)

#PH PHILLIPS PERRON GASTO GOBIERNO

lc.pp <- ur.pp(dlgastogob, type="Z-tau",model="trend", lags="long")
summary(lc.pp)

lc.pp <- ur.pp(dlgastogob, type="Z-tau",model="constant", lags="long")
summary(lc.pp)

#Pruebas KPSS para cointegracion tipo Granger
lc.kpss <- ur.kpss(dlgastogob, type="tau", lags="short", use.lag = NULL)
summary(lc.kpss)


################REGRESIÓN#########################
#Detección gráfica cointegración

summary(Gasto)


plot(exportpetro, main="Gasto de gobierno y Exportaciones petroleras", ylim=c(8.091e+07,1.380e+11))
lines(exportpetro, col="black")
lines(gastogob, col="red")

summary(lgastogob)
summary(lexportpetro)

plot(lexportpetro, main="LnGasto de gobierno y LNExportaciones petroleras", ylim=c(18,26))
lines(lexportpetro, col="black")
lines(lgastogob, col="red")

#ESTABLECER VARS.DEPENDIENTES, INDEPENDIENTES
# Funciones para probar cointegracion tipo Granger
mod_lin <- lm(gastogob ~ exportpetro)
summary(mod_lin)


mod_log <- lm(lgastogob ~ lexportpetro)
summary(mod_log)

# Generar los residuales de la ecuacion
res_lin <- residuals.lm(mod_lin)
summary(res_lin)
library(lmtest)
dwtest(formula = mod_lin)


boxplot(res_lin)
plot(res_lin)
boxplot(res_lin)
residualPlot(mod_lin)
qqnorm(res_lin)
qqline(res_lin)

res_log <- residuals.lm(mod_log)
summary(res_log)
dwtest(formula = mod_log)

boxplot(res_log)
plot(res_log)
boxplot(res_log)
residualPlot
qqnorm(res_log)
qqline(res_log)

##***Realizar pruebas de raíz unitaria de los residuales de ambos modelos

###################RES_LIN###############################
#PH DFA
lc.df <- ur.df(y=res_lin, type='trend', selectlags=c("AIC"))
summary(lc.df)

lc.df <- ur.df(y=res_lin, type='drift', selectlags=c("AIC"))
summary(lc.df)

lc.df <- ur.df(y=res_lin, type='none', selectlags=c("AIC"))
summary(lc.df)

#PH PHILLIPS PERRON
lc.pp <- ur.pp(res_lin, type="Z-tau", model="trend", lags="long")
summary(lc.pp)

lc.pp <- ur.pp(res_lin, type="Z-tau", model="constant", lags="long")
summary(lc.pp)

#Pruebas KPSS para cointegracion tipo Granger
lc.kpss <- ur.kpss(res_lin, type="tau", lags="short", use.lag = NULL)
summary(lc.kpss)

# Prueba de Phillips y Ouliaris para cointegracion
ecb.gg <- cbind(gastogob, exportpetro)

Lc.po <- ca.po(ecb.gg, type="Pz")
summary(Lc.po)

Lc.po <- ca.po(ecb.gg, type="Pu")
summary(Lc.po)




#######################################################################
# Prueba de Phillips y Ouliaris para cointegracion
ecb.gg <- cbind(gastogob, exportpetro)

Lc.po <- ca.po(ecb.gg, type="Pz")
summary(Lc.po)

Lc.po <- ca.po(ecb.gg, type="Pu")
summary(Lc.po)



##########################################################################

#====== Vectores de  Cointegracion (VC) con dos variables

# Trabajar con los residuales de la ecuacion

# Rezagos de los residuales
# se restringen los errores, dado que al final sobra el ultimo dato, se indica que observaciones queremos utilizar
res_lin_1 <- res_lin[1:53]
res_log_1 <- res_log[1:53]

# Modelos de Correccion de Error (MCE)
# Lineal
mce_lin <- lm(dgastogob ~dexportpetro+res_lin_1)
summary(mce_lin)

# Log-log
mce_log <- lm(dlgastogob ~dlexportpetro+res_log_1)
summary(mce_log)





#####################################################################
###################################################################

####Modelo gasto publico-exportaciones materias
summary(Gasto)

plot(exportmatprima, main="Gasto de gobierno y Exportaciones materias primas", ylim=c(4.030e+08, 1.380e+11))
lines(exportmatprima, col="black")
lines(gastogob, col="red")

summary(lgastogob)
summary(lexportmatprima)

plot(lexportmatprima, main="LnGasto de gobierno y LNExportaciones materias primas", ylim=c(19,26))
lines(lexportmatprima, col="black")
lines(lgastogob, col="red")

######Regresión


mod_lin2 <- lm(gastogob ~ exportmatprima)
summary(mod_lin2)


mod_log2 <- lm(lgastogob ~ lexportmatprima)
summary(mod_log2)

# Generar los residuales de la ecuacion
res_lin2 <- residuals.lm(mod_lin2)
summary(res_lin2)

res_log2 <- residuals.lm(mod_log2)
summary(res_log2)

# Prueba de Phillips y Ouliaris para cointegracion
ecb.gg1 <- cbind(gastogob, exportmatprima)

Lc.po1 <- ca.po(ecb.gg1, type="Pz")
summary(Lc.po1)

Lc.po1 <- ca.po(ecb.gg1, type="Pu")
summary(Lc.po1)


ecb.gg2 <- cbind(lgastogob, lexportmatprima)

Lc.po2 <- ca.po(ecb.gg2, type="Pz")
summary(Lc.po2)

Lc.po2 <- ca.po(ecb.gg2, type="Pu")
summary(Lc.po2)

##########

# Rezagos de los residuales
res_lin_2 <- res_lin2[1:53]
res_log_2 <- res_log2[1:53]

# Modelos de Correccion de Error (MCE)
# Lineal
mce_lin2 <- lm(dgastogob ~dexportmatprima+res_lin_2)
summary(mce_lin2)

# Log-log
mce_log2 <- lm(dlgastogob ~dlexportmatprima+res_log_2)
summary(mce_log2)













