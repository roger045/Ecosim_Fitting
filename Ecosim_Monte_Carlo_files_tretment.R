#################################
#  Ecosim fitting plots script  #
#################################

#------------------ CLEAN WORK DIRECTORY ------------------#
rm(list = ls())


# Laura Julià Melis (lauraj@icm.csic.es) - 03/2023
# Roger Amate (ramate@azti.es) - 07/2025

# 1. SET UP
# =========

# Set working directory
setwd("~/Results/1. Historical/MC_250_Historical/mc_Basic_model")

# Load necessary packages
library(readr)
library(dplyr)
library(tidyr)
library(purrr)


## 2. GET DATA
## ===========

#find biomass files in the folder
file.b <- list.files(path="~/Results/1. Historical/MC_250_Historical/mc_Basic_model", pattern = glob2rx("biomass_annual.csv"), recursive = T, full.names = T)
#find catch files in the folder
file.c <- list.files(path="~/Results/1. Historical/MC_250_Historical/mc_Basic_model", pattern = glob2rx("catch_annual.csv"), recursive = T, full.names = T)

#join all in a dataframe 
#include skip to delete the first rows
data.b <- file.b %>% lapply(read.csv, as.is=T, skip = 9, header = TRUE,sep=",")
data.c <- file.c %>% lapply(read.csv, as.is=T, skip = 9, header = TRUE, sep=",")

#find predicted values 
#include skip to delete the first rows
predicted.b <- read.csv("~/Results/1. Historical/Predicted/biomass_annual.csv", header = TRUE,sep=",", skip = 9)
predicted.c <- read.csv("~/Results/1. Historical/Predicted/catch_annual.csv", header = TRUE,sep=",", skip = 9)

#find observed values 
#include skip to delete the first rows
observed.b <- read.csv("~/Results/Ecosim_results_allfits/TIO_v2_balanced_diet_matrix_allfit_biomass.csv", header = TRUE,sep=",", skip = 7)
observed.c <- read.csv("~/Results/Ecosim_results_allfits/TIO_v2_balanced_diet_matrix_allfit_catches.csv",header = TRUE,sep=",", skip = 7)

#find number and name FG
fg <- read.csv("~/Results/fg_names_TIO2.csv",header = TRUE,sep=";")



## 3. PREPROCESSING
## ================

#start at row 6 (because observed values are in month 6 (June)) until the last one (234) and start at column 3 (because the first column is at 3) until the the last one and jumping every 2 (1 observed and 1 predicted) 
observed.b <- observed.b[seq(6,234, by=12), seq(3,33, by=2)]
observed.c <- observed.c[seq(6,234, by=12), seq(3,49, by=2)]
#this changes the number of the rownames to the year
rownames(observed.b) <- rownames(observed.b) <- 2003:2022

# Identify colnames with FG number (names in observed columns don't match so it has to be manually done):
colnames(observed.b) <- c(4,11:19,24,26,30:33) # just change the column name with the numbers of the corresponding FG to that column
colnames(observed.c) <- c(2:25) # just change the column name with the numbers of the corresponding FG to that column

#include 
colnames(predicted.b)[2:36] <- colnames(predicted.c)[2:36] <- 1:35 #number of FG + 1 from 2nd row



## 4. CREATE DATAFRAMES WITH MEAN AND CI(95%) PER YEAR
## ===================================================

#lenght is the number of FG (35)
biomass <- vector(mode='list', length=35)
catch <- vector(mode='list', length=35)
names(biomass) <- names(catch) <- fg$fg_name

for (d in c("biomass", "catch")){
  # 1. Get dataset
  if((d == "biomass")){data <- data.b} else{data <- data.c}
  
  # 2. Save values in "df" of all simulations (columns) per year(rows) for each functional group  
  for(j in 2:ncol(data[[1]])){   # first row is year
    df <- data.frame(matrix(nrow = 20)) 
    for (i in 1:length(data)){
      x <- data[[i]][[j]]
      df[,i] <- as.vector(x)
    }
    
    # 3. Calculate mean and CI + add observation/prediction
    if((d == "biomass")){
      biomass[[j-1]] <- data.frame("years"= 2003:2022,
                                   "mean" = rowMeans(df),
                                   "p05" = apply(df, 1, FUN = quantile, probs = .05),
                                   "p95" = apply(df, 1, FUN = quantile, probs = .95),
                                   "predicted"= predicted.b[,which(colnames(predicted.b) == j-1)],
                                   "observed" = observed.b[,which(colnames(observed.b) == j-1)])
    } else{
      catch[[j-1]] <- data.frame("years"= 2003:2022,
                                 "mean" = rowMeans(df),
                                 "p05" = apply(df, 1, FUN = quantile, probs = .05),
                                 "p95" = apply(df, 1, FUN = quantile, probs = .95),
                                 "predicted"= predicted.c[,which(colnames(predicted.c) == j-1)],
                                 "observed" = observed.c[,which(colnames(observed.c) == j-1)])
    }
  } 
}

rm(data, d, i, j, x, file.b, file.c)

## Guardamos los archivos de cada FG en su carpeta correspondiente de biomass o catch
setwd("~/Results/1. Historical/Outputs_250_Historical/csv/Biomass")

for (i in seq_along(biomass)) {
  nombre_df <- paste0("FG", i)  # Nombre único por cada data.frame
  write.csv(biomass[[i]], file = paste0(nombre_df, ".csv"), row.names = FALSE)
}

setwd("~/Results/1. Historical/Outputs_250_Historical/csv/Catch")

for (i in seq_along(catch)) {
  nombre_df <- paste0("FG", i)  # Nombre único por cada data.frame
  write.csv(catch[[i]], file = paste0(nombre_df, ".csv"), row.names = FALSE)
}



## 5. ECOSIM PLOTS PER FUNCTIONAL GROUP
## ====================================

for(d in c("Biomass", "Catch")){
  # 1. Get dataset
  if((d == "Biomass")){data <- biomass} else{data <- catch}
  
  for(i in 1:length(data)){ # for each FG
    
    # 2. Define y-axis limits
    if(ncol(data[[i]]) ==6){
      ymin = min(min(data[[i]]$observed, na.rm=T), min(data[[i]]$p05))
      ymax= max(max(data[[i]]$observed, na.rm=T), max(data[[i]]$p95))
    }else{
      ymin = min(data[[i]]$p05)
      ymax= max(data[[i]]$p95)
    }
    
    # 3. If there are no catches, don't make plot
    if(mean(data[[i]][[2]])== 0){ next }
    
    # 4. Open png file
    # 4. Open png file
    png(paste0("csv/", d, "/FG", i, ".png"), width = 1000, height = 732)
    
    # 5. Make plot
    #par "oma" allows to increase the space for title of the axis (in this case to enter "Biomass (T/km2))
    par(oma = c(0, 10, 1, 0), mgp = c(6, 3, 0))
    plot(data[[i]]$years, data[[i]]$predicted, 
         xlim=c(2003, 2022), ylim=c(0, ymax), xaxt="n",yaxt="n", ann=FALSE, las=1, cex.axis=2.5, text.font=3,
         type="l",  
         main= paste0(fg$fg_name[i], " - FG", i), cex.main=5, cex.axis =5, cex.lab =5)
    mtext("", cex=2.5, line=2.5, side=2)
    mtext("", side=1, line=3, cex=2.5, font=1, las=1)
    title = paste0(fg$fg_name[i], " - FG", i)
    mtext(title, cex = 5, side =3, line =1.5)
    axis(1,at=seq(2003,2022,6),las=1, cex.axis=4.5)
    axis(2,las=2, ylim=c(0, ymax), cex.axis=4.5)
    polygon(c(data[[i]]$years, rev(data[[i]]$years)),c(data[[i]]$p95, rev(data[[i]]$p05)),
            col = " lightsteelblue1", border=NA)
    points(data[[i]]$years, data[[i]]$predicted, type='l', lwd=4, lty=1)
    if(ncol(data[[i]]) ==6){
      points(data[[i]]$years, data[[i]]$observed, pch=19, cex=2.2) 
    }
    
    leyenda <- cor.test(data[[i]]$year, data[[i]]$predicted, method="spearman")
    pvalor <- ifelse(round(leyenda$p.value, 3) == 0, "<0.001", round(leyenda$p.value, 3))
    rho <-round(leyenda$estimate,3)
    legend ("bottomleft", legend=c(paste("p-value =",  pvalor), paste("rho =", rho)),
            bty="n", cex=3.5, ncol=1, text.font=2.5, box.lty=0, y.intersp = 0.9)
    # 6. Close device to save plot
    dev.off()
  }
}

rm(data, d, i, rho, pvalor, leyenda, ymin, ymax)

rm(data, d, i, rho, pvalor, leyenda, ymin, ymax)


#xlab="Time", ylab=paste(d, "(T/km2)"),
