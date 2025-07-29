################################################################################
###### Script to make a basic plot of the predictions of the indicators   ######
###### also including the unicertainty from the Monte Carlo simualtions   ######
###### Author: Roger Amate (AZTI)                                         ######
###### year: 2025                                                         ######
################################################################################

library(tidyr)
library(dplyr)
library(ggplot2)


# set the wd where the results of the Ecosim indicators are found
setwd("~/Results/1. Historical/Outputs_Indicators_Historical/Eco_Ind/csv")

# Ajustar los márgenes del gráfico
par(mar=c(4, 7.5, 6, 2) + 0.1)

# Read in the data
Ind <-read.csv("Indicator - 9.csv", header=T, sep=",")

# Crear el gráfico base sin datos
plot(Ind$time, Ind$observed, xlim=c(2003, 2022), ylim=c(0.4,0.7), type="n", xlab="", ylab="", cex.axis=2.5, las=1)

# Añadir el fondo gris claro al área de los datos
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="lightgray")

# Añadir los datos al gráfico
polygon(c(Ind$time, rev(Ind$time)), c(Ind$p95, rev(Ind$p05)), col = adjustcolor("blue", alpha.f = 0.1), border = NA)
lines(Ind$time, Ind$predicted, lwd=3.5, col="black")

# Realizar el test de correlación
leyenda <- cor.test(Ind$time, Ind$predicted, method="spearman")

# Obtener los valores del test
pvalor <- round(leyenda$p.value, 3)
pvalor1 <- ifelse(pvalor == 0, "<0.001", pvalor)
rho <- round(leyenda$estimate, 3)

# Añadir la leyenda con los resultados del test
legend(2012, 0.7, legend=c(paste("p-value =", pvalor1), paste("rho =", rho)), bty="n", cex=2.5, ncol=1, text.font=2, box.lty=0, y.intersp=0.9)

# Añadir el título al gráfico
title(main="Predatory biomass", cex.main=3.5)


##############################################
##############################################


# Read in the data
Ind <-read.csv("Indicator - 2.csv", header=T, sep=",")

# Crear el gráfico base sin datos
plot(Ind$time, Ind$observed, xlim=c(2003, 2022), ylim=c(0.28,0.56), type="n", xlab="", ylab="", cex.axis=2.5, las=1)

# Añadir el fondo gris claro al área de los datos
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="lightgray")

# Añadir los datos al gráfico
polygon(c(Ind$time, rev(Ind$time)), c(Ind$p95, rev(Ind$p05)), col = adjustcolor("blue", alpha.f = 0.1), border = NA)
lines(Ind$time, Ind$predicted, lwd=3.5, col="black")

# Realizar el test de correlación
leyenda <- cor.test(Ind$time, Ind$predicted, method="spearman")

# Obtener los valores del test
pvalor <- round(leyenda$p.value, 3)
pvalor1 <- ifelse(pvalor == 0, "<0.001", pvalor)
rho <- round(leyenda$estimate, 3)

# Añadir la leyenda con los resultados del test
legend(2012, 0.56, legend=c(paste("p-value =", pvalor1), paste("rho =", rho)), bty="n", cex=2.5, ncol=1, text.font=2, box.lty=0, y.intersp=0.9)

# Añadir el título al gráfico
title(main="Commercial species biomass", cex.main=3.5)

##############################################
##############################################


# Read in the data
Ind <-read.csv("Indicator - 23.csv", header=T, sep=",")

# Crear el gráfico base sin datos
plot(Ind$time, Ind$observed, xlim=c(2003, 2022), ylim=c(1.105,1.13), type="n", xlab="", ylab="", cex.axis=2.5, las=1)

# Añadir el fondo gris claro al área de los datos
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="lightgray")

# Añadir los datos al gráfico
polygon(c(Ind$time, rev(Ind$time)), c(Ind$p95, rev(Ind$p05)), col = adjustcolor("blue", alpha.f = 0.1), border = NA)
lines(Ind$time, Ind$predicted, lwd=3.5, col="black")

# Realizar el test de correlación
leyenda <- cor.test(Ind$time, Ind$predicted, method="spearman")

# Obtener los valores del test
pvalor <- round(leyenda$p.value, 3)
pvalor1 <- ifelse(pvalor == 0, "<0.001", pvalor)
rho <- round(leyenda$estimate, 3)

# Añadir la leyenda con los resultados del test
legend(2012, 1.1315, legend=c(paste("p-value =", pvalor1), paste("rho =", rho)), bty="n", cex=2.5, ncol=1, text.font=2, box.lty=0, y.intersp=0.9)

# Añadir el título al gráfico
title(main="mTLco", cex.main=3.5)

##############################################
##############################################


# Read in the data
Ind <-read.csv("Indicator - 10.csv", header=T, sep=",")

# Crear el gráfico base sin datos
plot(Ind$time, Ind$observed, xlim=c(2003, 2022), ylim=c(4,6.2), type="n", xlab="", ylab="", cex.axis=2.5, las=1)

# Añadir el fondo gris claro al área de los datos
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="lightgray")

# Añadir los datos al gráfico
polygon(c(Ind$time, rev(Ind$time)), c(Ind$p95, rev(Ind$p05)), col = adjustcolor("blue", alpha.f = 0.1), border = NA)
lines(Ind$time, Ind$predicted, lwd=3.5, col="black")

# Realizar el test de correlación
leyenda <- cor.test(Ind$time, Ind$predicted, method="spearman")

# Obtener los valores del test
pvalor <- round(leyenda$p.value, 3)
pvalor1 <- ifelse(pvalor == 0, "<0.001", pvalor)
rho <- round(leyenda$estimate, 3)

# Añadir la leyenda con los resultados del test
legend(2012, 6.22, legend=c(paste("p-value =", pvalor1), paste("rho =", rho)), bty="n", cex=2.5, ncol=1, text.font=2, box.lty=0, y.intersp=0.9)

# Añadir el título al gráfico
title(main="Kempton's Q", cex.main=3.5)

##############################################
##############################################


# Read in the data
Ind <-read.csv("Indicator - 21.csv", header=T, sep=",")

# Crear el gráfico base sin datos
plot(Ind$time, Ind$observed, xlim=c(2003, 2022), ylim=c(4,4.22), type="n", xlab="", ylab="", cex.axis=2.5, las=1)

# Añadir el fondo gris claro al área de los datos
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="lightgray")

# Añadir los datos al gráfico
polygon(c(Ind$time, rev(Ind$time)), c(Ind$p95, rev(Ind$p05)), col = adjustcolor("blue", alpha.f = 0.1), border = NA)
lines(Ind$time, Ind$predicted, lwd=3.5, col="black")

# Realizar el test de correlación
leyenda <- cor.test(Ind$time, Ind$predicted, method="spearman")

# Obtener los valores del test
pvalor <- round(leyenda$p.value, 3)
pvalor1 <- ifelse(pvalor == 0, "<0.001", pvalor)
rho <- round(leyenda$estimate, 3)

# Añadir la leyenda con los resultados del test
legend(2002, 4.06, legend=c(paste("p-value =", pvalor1), paste("rho =", rho)), bty="n", cex=2.5, ncol=1, text.font=2, box.lty=0, y.intersp=0.9)

# Añadir el título al gráfico
title(main="mTLc", cex.main=3.5)

##############################################
##############################################


# Read in the data
Ind <-read.csv("Indicator - 26.csv", header=T, sep=",")

# Crear el gráfico base sin datos
plot(Ind$time, Ind$observed, xlim=c(2003, 2022), ylim=c(4.15,4.25), type="n", xlab="", ylab="", cex.axis=2.5, las=1)

# Añadir el fondo gris claro al área de los datos
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="lightgray")

# Añadir los datos al gráfico
polygon(c(Ind$time, rev(Ind$time)), c(Ind$p95, rev(Ind$p05)), col = adjustcolor("blue", alpha.f = 0.1), border = NA)
lines(Ind$time, Ind$predicted, lwd=3.5, col="black")

# Realizar el test de correlación
leyenda <- cor.test(Ind$time, Ind$predicted, method="spearman")

# Obtener los valores del test
pvalor <- round(leyenda$p.value, 3)
pvalor1 <- ifelse(pvalor == 0, "<0.001", pvalor)
rho <- round(leyenda$estimate, 3)

# Añadir la leyenda con los resultados del test
legend(2002, 4.18, legend=c(paste("p-value =", pvalor1), paste("rho =", rho)), bty="n", cex=2.5, ncol=1, text.font=2, box.lty=0, y.intersp=0.9)

# Añadir el título al gráfico
title(main="mTLco > 4", cex.main=3.5)

##############################################
##############################################
