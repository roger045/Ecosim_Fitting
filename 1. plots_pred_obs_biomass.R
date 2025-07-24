################################################################################
###### Script to make a basic plot of the predicted and observed biomass  ######
###### also including the unicertainty from the Monte Carlo simualtions   ######
###### Author: Roger Amate (AZTI)                                         ######
###### year: 2024                                                         ######
################################################################################

library(tidyr)
library(dplyr)
library(ggplot2)


# set the wd where the results of the ecosim are found
setwd("~/1. Results/Ecosim_results_allfits")

# read the biomass .csv
biomass <-read.csv("TIO_v2_balanced_diet_matrix_allfit_biomass.csv", header=T, sep=",", skip=7)


################ Swordfish #################
SWO <-biomass[,c(1,6,7)]
years <- seq(2003, 2022, by=1/12.6)
SWO$Year <- years

# Ajustar los márgenes del gráfico
par(mar=c(4, 7, 6, 2) + 0.1)

# Crear el gráfico base sin datos
plot(SWO$Year, SWO$biomass..predicted..Swordfish, xlim=c(2003, 2022), ylim=c(0,0.02), type="n", xlab="", ylab="", cex.axis=2.5, las=1)

# Añadir el fondo gris claro al área de los datos
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="lightgray")

# Añadir los datos al gráfico
setwd("~/Results/1. Historical/Outputs_250_Historical/csv/Biomass")
SWO <-read.csv("FG12.csv", header=T, sep=",")
polygon(c(SWO$years, rev(SWO$years)), c(SWO$p95, rev(SWO$p05)), col = adjustcolor("blue", alpha.f = 0.1), border = NA)
lines(SWO$years, SWO$predicted, lwd=2, col="blue")
points(SWO$years, SWO$observed, pch=19, cex=1.2)

# Realizar el test de correlación
leyenda <- cor.test(SWO$years, SWO$predicted, method="spearman")

# Obtener los valores del test
pvalor <- round(leyenda$p.value, 3)
pvalor1 <- ifelse(pvalor == 0, "<0.001", pvalor)
rho <- round(leyenda$estimate, 3)

# Añadir la leyenda con los resultados del test
legend(2009, 0.006, legend=c(paste("p-valor =", pvalor1), paste("rho =", rho)), bty="n", cex=2.5, ncol=1, text.font=2, box.lty=0, y.intersp=0.9)

# Añadir el título al gráfico
title(main="12. Swordfish", cex.main=3.5)

# Añadir la leyenda en la parte inferior
#legend("bottom", legend=c("Predicted", "Observed"), col=c("blue", "black"), lty=c(1, NA), pch=c(NA, 19), horiz=TRUE, bty="n", cex=1.2, lwd=2)

##############################################

################ Bigeye adult #################
BETa <-biomass[,c(1,8,9)]
years <- seq(2003, 2022, by=1/12.6)
BETa$Year <- years

# Crear el gráfico base sin datos
plot(BETa$Year, BETa$biomass..predicted..Bigeye.adult, xlim=c(2003, 2022), ylim=c(0,0.04), type="n", xlab="", ylab="", cex.axis=2.5, las=1)

# Añadir el fondo gris claro al área de los datos
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="lightgray")

# Cambiamos el directorio, abrimos el archivo desde estan los p05 y p95 del Monte Carlo y lo ploteamos
setwd("~/Results/1. Historical/Outputs_250_Historical/csv/Biomass")
BETa <-read.csv("FG13.csv", header=T, sep=",")
polygon(c(BETa$years, rev(BETa$years)), c(BETa$p95, rev(BETa$p05)), col=adjustcolor("purple3", alpha.f = 0.1), border = NA)
lines(BETa$years, BETa$predicted, lwd=2, col="purple3")
points(BETa$years, BETa$observed, pch=19, cex=1.2)

# Realizar el test de correlación
leyenda <- cor.test(BETa$years, BETa$predicted, method="spearman")

# Obtener los valores del test
pvalor <- round(leyenda$p.value, 3)
pvalor1 <- ifelse(pvalor == 0, "<0.001", pvalor)
rho <- round(leyenda$estimate, 3)

# Añadir la leyenda con los resultados del test
legend(2012, 0.042, legend=c(paste("p-valor =", pvalor1), paste("rho =", rho)), bty="n", cex=2.5, ncol=1, text.font=2, box.lty=0, y.intersp=0.9)

# Añadir el título al gráfico
title(main="13. Bigeye adult", cex.main=3.5)

# Añadir la leyenda en la parte inferior
#legend("bottom", legend=c("Predicted", "Observed"), col=c("purple3", "black"), lty=c(1, NA), pch=c(NA, 19), horiz=TRUE, bty="n", cex=1.2, lwd=2)

##############################################


################ Bigeye juvenile #################
BETj <-biomass[,c(1,10,11)]
years <- seq(2003, 2022, by=1/12.6)
BETj$Year <- years

# Crear el gráfico base sin datos
plot(BETj$Year, BETj$biomass..predicted..Bigeye.juvenile, xlim=c(2003, 2022), ylim=c(0,0.02), type="n", xlab="", ylab="", cex.axis=2.5, las=1)

# Añadir el fondo gris claro al área de los datos
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="lightgray")

# Cambiamos el directorio, abrimos el archivo desde estan los p05 y p95 del Monte Carlo y lo ploteamos
setwd("~/Results/1. Historical/Outputs_250_Historical/csv/Biomass")
BETj <-read.csv("FG14.csv", header=T, sep=",")
polygon(c(BETj$years, rev(BETj$years)), c(BETj$p95, rev(BETj$p05)), col=adjustcolor("purple", alpha.f = 0.1), border = NA)
lines(BETj$years, BETj$predicted, lwd=2, col="purple")
points(BETj$years, BETj$observed, pch=19, cex=1.2)

# Realizar el test de correlación
leyenda <- cor.test(BETj$years, BETj$predicted, method="spearman")

# Obtener los valores del test
pvalor <- round(leyenda$p.value, 3)
pvalor1 <- ifelse(pvalor == 0, "<0.001", pvalor)
rho <- round(leyenda$estimate, 3)

# Añadir la leyenda con los resultados del test
legend(2012, 0.022, legend=c(paste("p-valor =", pvalor1), paste("rho =", rho)), bty="n", cex=2.5, ncol=1, text.font=2, box.lty=0, y.intersp=0.9)

# Añadir el título al gráfico
title(main="14. Bigeye juvenile", cex.main=3.5)

# Añadir la leyenda en la parte inferior
#legend("bottom", legend=c("Predicted", "Observed"), col=c("purple", "black"), lty=c(1, NA), pch=c(NA, 19), horiz=TRUE, bty="n", cex=1.2, lwd=2)

##############################################

################ Yellowfin adult #################
YFTa <-biomass[,c(1,12,13)]
years <- seq(2003, 2022, by=1/12.6)
YFTa$Year <- years

# Crear el gráfico base sin datos
plot(YFTa$Year, YFTa$biomass..predicted..Yellowfin.adult, xlim=c(2003, 2022), ylim=c(0,0.1), type="n", xlab="", ylab="", cex.axis=2.5, las=1)

# Añadir el fondo gris claro al área de los datos
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="lightgray")

# Cambiamos el directorio, abrimos el archivo desde estan los p05 y p95 del Monte Carlo y lo ploteamos
setwd("~/Results/1. Historical/Outputs_250_Historical/csv/Biomass")
YFTa <-read.csv("FG15.csv", header=T, sep=",")
polygon(c(YFTa$years, rev(YFTa$years)), c(YFTa$p95, rev(YFTa$p05)), col=adjustcolor("darkorange2", alpha.f = 0.2), border = NA)
lines(YFTa$years, YFTa$predicted, lwd=2, col="darkorange2")
points(YFTa$years, YFTa$observed, pch=19, cex=1.2)

# Realizar el test de correlación
leyenda <- cor.test(YFTa$years, YFTa$predicted, method="spearman")

# Obtener los valores del test
pvalor <- round(leyenda$p.value, 3)
pvalor1 <- ifelse(pvalor == 0, "<0.001", pvalor)
rho <- round(leyenda$estimate, 3)

# Añadir la leyenda con los resultados del test
legend(2012, 0.1, legend=c(paste("p-valor =", pvalor1), paste("rho =", rho)), bty="n", cex=2.5, ncol=1, text.font=2, box.lty=0, y.intersp=0.9)

# Añadir el título al gráfico
title(main="15. Yellowfin adult", cex.main=3.5)

# Añadir la leyenda en la parte inferior
#legend("bottom", legend=c("Predicted", "Observed"), col=c("darkorange2", "black"), lty=c(1, NA), pch=c(NA, 19), horiz=TRUE, bty="n", cex=1.2, lwd=2)

##############################################

################ Yellowfin juvenile #################
YFTj <-biomass[,c(1,14,15)]
years <- seq(2003, 2022, by=1/12.6)
YFTj$Year <- years

# Crear el gráfico base sin datos
plot(YFTj$Year, YFTj$biomass..predicted..Yellowfin.juvenile, xlim=c(2003, 2022), ylim=c(0,0.05), type="n", xlab="", ylab="", cex.axis=2.5, las=1)

# Añadir el fondo gris claro al área de los datos
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="lightgray")

# Cambiamos el directorio, abrimos el archivo desde estan los p05 y p95 del Monte Carlo y lo ploteamos
setwd("~/Results/1. Historical/Outputs_250_Historical/csv/Biomass")
YFTj <-read.csv("FG16.csv", header=T, sep=",")
polygon(c(YFTj$years, rev(YFTj$years)), c(YFTj$p95, rev(YFTj$p05)), col=adjustcolor("darkorange1", alpha.f = 0.2), border = NA)
lines(YFTj$years, YFTj$predicted, lwd=2, col="darkorange1")
points(YFTj$years, YFTj$observed, pch=19, cex=1.2)

# Realizar el test de correlación
leyenda <- cor.test(YFTj$years, YFTj$predicted, method="spearman")

# Obtener los valores del test
pvalor <- round(leyenda$p.value, 3)
pvalor1 <- ifelse(pvalor == 0, "<0.001", pvalor)
rho <- round(leyenda$estimate, 3)

# Añadir la leyenda con los resultados del test
legend(2012, 0.051, legend=c(paste("p-valor =", pvalor1), paste("rho =", rho)), bty="n", cex=2.5, ncol=1, text.font=2, box.lty=0, y.intersp=0.9)

# Añadir el título al gráfico
title(main="16. Yellowfin juvenile", cex.main=3.5)

# Añadir la leyenda en la parte inferior
#legend("bottom", legend=c("Predicted", "Observed"), col=c("darkorange1", "black"), lty=c(1, NA), pch=c(NA, 19), horiz=TRUE, bty="n", cex=1.2, lwd=2)

##############################################

################ Skipjack adult #################
SKJa <-biomass[,c(1,16,17)]
years <- seq(2003, 2022, by=1/12.6)
SKJa$Year <- years

# Crear el gráfico base sin datos
plot(SKJa$Year, SKJa$biomass..predicted..Skipjack.adult, xlim=c(2003, 2022), ylim=c(0,0.08), type="n", xlab="", ylab="", cex.axis=2.5, las=1)

# Añadir el fondo gris claro al área de los datos
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="lightgray")

# Cambiamos el directorio, abrimos el archivo desde estan los p05 y p95 del Monte Carlo y lo ploteamos
setwd("~/Results/1. Historical/Outputs_250_Historical/csv/Biomass")
SKJa <-read.csv("FG17.csv", header=T, sep=",")
polygon(c(SKJa$years, rev(SKJa$years)), c(SKJa$p95, rev(SKJa$p05)), col=adjustcolor("firebrick", alpha.f = 0.2), border = NA)
lines(SKJa$years, SKJa$predicted, lwd=2, col="firebrick")
points(SKJa$years, SKJa$observed, pch=19, cex=1.2)

# Realizar el test de correlación
leyenda <- cor.test(SKJa$years, SKJa$predicted, method="spearman")

# Obtener los valores del test
pvalor <- round(leyenda$p.value, 3)
pvalor1 <- ifelse(pvalor == 0, "<0.001", pvalor)
rho <- round(leyenda$estimate, 3)

# Añadir la leyenda con los resultados del test
legend(2012, 0.025, legend=c(paste("p-valor =", pvalor1), paste("rho =", rho)), bty="n", cex=2.5, ncol=1, text.font=2, box.lty=0, y.intersp=0.9)

# Añadir el título al gráfico
title(main="17. Skipjack adult", cex.main=3.5)

# Añadir la leyenda en la parte inferior
#legend("bottom", legend=c("Predicted", "Observed"), col=c("firebrick", "black"), lty=c(1, NA), pch=c(NA, 19), horiz=TRUE, bty="n", cex=1.2, lwd=2)

##############################################

################ Skipjack juvenile #################
SKJj <-biomass[,c(1,18,19)]
years <- seq(2003, 2022, by=1/12.6)
SKJj$Year <- years

# Ajustar los márgenes del gráfico un poco mas grandes
par(mar=c(4, 7.5, 6, 2) + 0.1)

# Crear el gráfico base sin datos
plot(SKJj$Year, SKJj$biomass..predicted..Skipjack.juvenile, xlim=c(2003, 2022), ylim=c(0,0.003), type="n", xlab="", ylab="", cex.axis=2.5, las=1)

# Añadir el fondo gris claro al área de los datos
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="lightgray")

# Cambiamos el directorio, abrimos el archivo desde estan los p05 y p95 del Monte Carlo y lo ploteamos
setwd("~/Results/1. Historical/Outputs_250_Historical/csv/Biomass")
SKJj <-read.csv("FG18.csv", header=T, sep=",")
polygon(c(SKJj$years, rev(SKJj$years)), c(SKJj$p95, rev(SKJj$p05)), col=adjustcolor("firebrick3", alpha.f = 0.2), border = NA)
lines(SKJj$years, SKJj$predicted, lwd=2, col="firebrick3")
points(SKJj$years, SKJj$observed, pch=19, cex=1.2)

# Realizar el test de correlación
leyenda <- cor.test(SKJj$years, SKJj$predicted, method="spearman")

# Obtener los valores del test
pvalor <- round(leyenda$p.value, 3)
pvalor1 <- ifelse(pvalor == 0, "<0.001", pvalor)
rho <- round(leyenda$estimate, 3)

# Añadir la leyenda con los resultados del test
legend(2012, 0.001, legend=c(paste("p-valor =", pvalor1), paste("rho =", rho)), bty="n", cex=2.5, ncol=1, text.font=2, box.lty=0, y.intersp=0.9)

# Añadir el título al gráfico
title(main="18. Skipjack juvenile", cex.main=3.5)

# Añadir la leyenda en la parte inferior
#legend("bottom", legend=c("Predicted", "Observed"), col=c("firebrick3", "black"), lty=c(1, NA), pch=c(NA, 19), horiz=TRUE, bty="n", cex=1.2, lwd=2)

##############################################

################ Albacore #################
ALB <-biomass[,c(1,20,21)]
years <- seq(2003, 2022, by=1/12.6)
ALB$Year <- years

# Crear el gráfico base sin datos
plot(ALB$Year, ALB$biomass..predicted..Albacore, xlim=c(2003, 2022), ylim=c(0,0.0035), type="n", xlab="", ylab="", cex.axis=2.5, las=1)

# Añadir el fondo gris claro al área de los datos
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="lightgray")

# Cambiamos el directorio, abrimos el archivo desde estan los p05 y p95 del Monte Carlo y lo ploteamos
setwd("~/Results/1. Historical/Outputs_250_Historical/csv/Biomass")
ALB <-read.csv("FG19.csv", header=T, sep=",")
polygon(c(ALB$years, rev(ALB$years)), c(ALB$p95, rev(ALB$p05)), col=adjustcolor("green4", alpha.f = 0.2), border = NA)
lines(ALB$years, ALB$predicted, lwd=2, col="green4")
points(ALB$years, ALB$observed, pch=19, cex=1.2)

# Realizar el test de correlación
leyenda <- cor.test(ALB$years, ALB$predicted, method="spearman")

# Obtener los valores del test
pvalor <- round(leyenda$p.value, 3)
pvalor1 <- ifelse(pvalor == 0, "<0.001", pvalor)
rho <- round(leyenda$estimate, 3)

# Añadir la leyenda con los resultados del test
legend(2012, 0.0037, legend=c(paste("p-valor =", pvalor1), paste("rho =", rho)), bty="n", cex=2.5, ncol=1, text.font=2, box.lty=0, y.intersp=0.9)

# Añadir el título al gráfico
title(main="19. Albacore", cex.main=3.5)

# Añadir la leyenda en la parte inferior
#legend("bottom", legend=c("Predicted", "Observed"), col=c("green4", "black"), lty=c(1, NA), pch=c(NA, 19), horiz=TRUE, bty="n", cex=1.2, lwd=2)

##############################################

################ Blue shark #################
BSH <-biomass[,c(1,2,3)]
years <- seq(2003, 2022, by=1/12.6)
BSH$Year <- years

# Ajustar los márgenes del gráfico como al inicio
par(mar=c(4, 7, 6, 2) + 0.1)

# Crear el gráfico base sin datos
plot(BSH$Year, BSH$biomass..predicted..Blue.shark, xlim=c(2003, 2022), ylim=c(0,0.02), type="n", xlab="", ylab="", cex.axis=2.5, las=1)

# Añadir el fondo gris claro al área de los datos
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="lightgray")

# Cambiamos el directorio, abrimos el archivo desde estan los p05 y p95 del Monte Carlo y lo ploteamos
setwd("~/Results/1. Historical/Outputs_250_Historical/csv/Biomass")
BSH <-read.csv("FG4.csv", header=T, sep=",")
polygon(c(BSH$years, rev(BSH$years)), c(BSH$p95, rev(BSH$p05)), col=adjustcolor("darkcyan", alpha.f = 0.2), border = NA)
lines(BSH$years, BSH$predicted, lwd=2, col="darkcyan")
points(BSH$years, BSH$observed, pch=19, cex=1.2)

# Realizar el test de correlación
leyenda <- cor.test(BSH$years, BSH$predicted, method="spearman")

# Obtener los valores del test
pvalor <- round(leyenda$p.value, 3)
pvalor1 <- ifelse(pvalor == 0, "<0.001", pvalor)
rho <- round(leyenda$estimate, 3)

# Añadir la leyenda con los resultados del test
legend(2012, 0.022, legend=c(paste("p-valor =", pvalor1), paste("rho =", rho)), bty="n", cex=2.5, ncol=1, text.font=2, box.lty=0, y.intersp=0.9)

# Añadir el título al gráfico
title(main="4. Blue shark", cex.main=3.5)

# Añadir la leyenda en la parte inferior
#legend("bottom", legend=c("Predicted", "Observed"), col=c("darkcyan", "black"), lty=c(1, NA), pch=c(NA, 19), horiz=TRUE, bty="n", cex=1.2, lwd=2)

##############################################


################ Billfishes #################
BILL <-biomass[,c(1,4,5)]
years <- seq(2003, 2022, by=1/12.6)
BILL$Year <- years

# Crear el gráfico base sin datos
plot(BILL$Year, BILL$biomass..predicted..Billfishes, xlim=c(2003, 2022), ylim=c(0,0.02), type="n", xlab="", ylab="", cex.axis=2.5, las=1)

# Añadir el fondo gris claro al área de los datos
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="lightgray")

# Cambiamos el directorio, abrimos el archivo desde estan los p05 y p95 del Monte Carlo y lo ploteamos
setwd("~/Results/1. Historical/Outputs_250_Historical/csv/Biomass")
BILL <-read.csv("FG11.csv", header=T, sep=",")
polygon(c(BILL$years, rev(BILL$years)), c(BILL$p95, rev(BILL$p05)), col=adjustcolor("mediumseagreen", alpha.f = 0.2), border = NA)
lines(BILL$years, BILL$predicted, lwd=2, col="mediumseagreen")
points(BILL$years, BILL$observed, pch=19, cex=1.2)

# Realizar el test de correlación
leyenda <- cor.test(BILL$years, BILL$predicted, method="spearman")

# Obtener los valores del test
pvalor <- round(leyenda$p.value, 3)
pvalor1 <- ifelse(pvalor == 0, "<0.001", pvalor)
rho <- round(leyenda$estimate, 3)

# Añadir la leyenda con los resultados del test
legend(2012, 0.0065, legend=c(paste("p-valor =", pvalor1), paste("rho =", rho)), bty="n", cex=2.5, ncol=1, text.font=2, box.lty=0, y.intersp=0.9)

# Añadir el título al gráfico
title(main="11. Other billfishes", cex.main=3.5)

# Añadir la leyenda en la parte inferior
#legend("bottom", legend=c("Predicted", "Observed"), col=c("mediumseagreen", "black"), lty=c(1, NA), pch=c(NA, 19), horiz=TRUE, bty="n", cex=1.2, lwd=2)

##############################################


################ Small epeipelagic fishes #################
SEF <-biomass[,c(1,22,23)]
years <- seq(2003, 2022, by=1/12.6)
SEF$Year <- years

# Ajustar los márgenes del gráfico para que quepa el título
par(mar=c(4, 7, 6, 2) + 0.1)

# Crear el gráfico base sin datos
plot(SEF$Year, SEF$biomass..predicted..Small.epipelagic.fishes, xlim=c(2003, 2022), ylim=c(0,0.8), type="n", xlab="", ylab="", cex.axis=2.5, las=1)

# Añadir el fondo gris claro al área de los datos
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="lightgray")

# Cambiamos el directorio, abrimos el archivo desde estan los p05 y p95 del Monte Carlo y lo ploteamos
setwd("~/Results/1. Historical/Outputs_250_Historical/csv/Biomass")
SEF <-read.csv("FG24.csv", header=T, sep=",")
polygon(c(SEF$years, rev(SEF$years)), c(SEF$p95, rev(SEF$p05)), col=adjustcolor("darkviolet", alpha.f = 0.2), border = NA)
lines(SEF$years, SEF$predicted, lwd=2, col="darkviolet")
points(SEF$years, SEF$observed, pch=19, cex=1.2)

# Realizar el test de correlación
leyenda <- cor.test(SEF$years, SEF$predicted, method="spearman")

# Obtener los valores del test
pvalor <- round(leyenda$p.value, 3)
pvalor1 <- ifelse(pvalor == 0, "<0.001", pvalor)
rho <- round(leyenda$estimate, 3)

# Añadir la leyenda con los resultados del test
legend(2012, 0.22, legend=c(paste("p-valor =", pvalor1), paste("rho =", rho)), bty="n", cex=2.5, ncol=1, text.font=2, box.lty=0, y.intersp=0.9)

# Añadir el título al gráfico
title(main="24. Small epipelagic fishes", cex.main=3.5)

# Añadir la leyenda en la parte inferior
#legend("bottom", legend=c("Predicted", "Observed"), col=c("darkviolet", "black"), lty=c(1, NA), pch=c(NA, 19), horiz=TRUE, bty="n", cex=1.2, lwd=2)

##############################################


################ Mesopelagic fishes #################
MESO <-biomass[,c(1,24,25)]
years <- seq(2003, 2022, by=1/12.6)
MESO$Year <- years

# Crear el gráfico base sin datos
plot(MESO$Year, MESO$biomass..predicted..Mesopelagic.fishes, xlim=c(2003, 2022), ylim=c(0,3.5), type="n", xlab="", ylab="", cex.axis=2.5, las=1)

# Añadir el fondo gris claro al área de los datos
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="lightgray")

setwd("~/Results/1. Historical/Outputs_250_Historical/csv/Biomass")
MESO <-read.csv("FG26.csv", header=T, sep=",")
polygon(c(MESO$years, rev(MESO$years)), c(MESO$p95, rev(MESO$p05)), col=adjustcolor("red2", alpha.f = 0.2), border = NA)
lines(MESO$years, MESO$predicted, lwd=2, col="red2")
points(MESO$years, MESO$observed, pch=19, cex=1.2)

# Realizar el test de correlación
leyenda <- cor.test(MESO$years, MESO$predicted, method="spearman")

# Obtener los valores del test
pvalor <- round(leyenda$p.value, 3)
pvalor1 <- ifelse(pvalor == 0, "<0.001", pvalor)
rho <- round(leyenda$estimate, 3)

# Añadir la leyenda con los resultados del test
legend(2012, 1.05, legend=c(paste("p-valor =", pvalor1), paste("rho =", rho)), bty="n", cex=2.5, ncol=1, text.font=2, box.lty=0, y.intersp=0.9)

# Añadir el título al gráfico
title(main="26. Small mesopelagic fishes", cex.main=3.5)

# Añadir la leyenda en la parte inferior
#legend("bottom", legend=c("Predicted", "Observed"), col=c("red2", "black"), lty=c(1, NA), pch=c(NA, 19), horiz=TRUE, bty="n", cex=1.2, lwd=2)

##############################################


