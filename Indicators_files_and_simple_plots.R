#################################
#  Ecological Indicators Plots  #
#################################

#------------------ CLEAN WORK DIRECTORY ------------------#
rm(list = ls())
# 1. SET UP
# =========

# Set working directory
setwd("~/1. Results")

# Load necessary packages
library(zoo)

## 2. GET DATA
## ===========
#include skip to delete the first rows
biodiv <- read.csv("~/Results/1. Historical/Indicators_Historical/biodiv_ind_Monte Carlo.csv",header = TRUE,sep=";", skip = 8)
uncertainty <- read.csv("~/Results/1. Historical/Indicators_Historical/biodiv_ind_Monte Carlo_uncertainty.csv",header = TRUE,sep=";", skip = 5)
names <- read.csv("~/Results/1. Historical/Indicators_Historical/biodiv_ind_Monte Carlo_names.csv", sep=";", header=T)
pred <- read.csv("~/Results/1. Historical/Indicators_Historical/biodiv_ind_Ecosim.csv",header = TRUE,sep=",", skip = 8)


## 3. PREPROCESSING
## ================
# 3.1. Get annual data (taking mean of every 12 rows)
biodiv <- as.data.frame(rollapply(biodiv, width=12, by=12, FUN=mean))
biodiv$Time <- rep(2003:2022, 250)   #35 is the number of mcm runs saved

pred <- as.data.frame(rollapply(pred, width=12, by=12, FUN=mean))
pred$Time <- rep(2003:2022)


## 4. CREATE DATAFRAMES WITH MEAN AND CI(95%) PER YEAR
## ===================================================
indicators <- vector(mode='list', length=39)
names(indicators) <- names$y.label

# 1. Save values in "df" of all simulations (columns) per year(rows) for each indicator 
for(j in 3:ncol(biodiv)){   # first rows are trial and year
  df <- data.frame(matrix(nrow =length(unique(biodiv$Time)), ncol=max(unique(biodiv$Trial)))) 
  for (i in unique(biodiv$Time)){
    x <- subset(biodiv, Time == i)
    df[i-2002,] <- as.vector(x[,j])
  }
  
  # 2. Calculate mean and CI + add observation/prediction
  indicators[[j-2]] <- data.frame("time"= unique(biodiv$Time),
                               "mean" = rowMeans(df),
                               "p05" = apply(df, 1, FUN = quantile, probs = .05),
                               "p95" = apply(df, 1, FUN = quantile, probs = .95),
                               "predicted"= pred[,j-1])
}

rm(i,j,df,x)


setwd("~/Results/1. Historical/Outputs_Indicators_Historical/Eco_Ind/csv")

for (i in seq_along(indicators)) {
  nombre_df <- paste0("Indicator - ", i)  # Nombre único por cada data.frame
  write.csv(indicators[[i]], file = paste0(nombre_df, ".csv"), row.names = FALSE)
}
# Each indicator will be saved in a .csv file. Like Indicator - 1, Indicator - 2,... 
# Each number corresponds to the indicator with the same row number in the file biodiv_ind_Monte Carlo_names.csv

## 5. PLOTS OF ECOLOGICAL INDICATORS OVER TIME
## ==========================================

setwd("~/Results/1. Historical")

for(i in 1:length(indicators)){ # for each FG
  # 1. Define y-axis limits, label and size of the title
  ymin = min(min(indicators[[i]]$predicted, na.rm=T), min(indicators[[i]]$p05))
  ymax= max(max(indicators[[i]]$predicted, na.rm=T), max(indicators[[i]]$p95))
  label= ifelse(names$units[i]== "", names$y.label[i], paste0(names$y.label[i], " (", names$units[i], ")"))  # some indicators don't have units
  cex.title =ifelse(nchar(names$y.label[i])> 15, 1.2, 1.7) # if title is too long (>70 characters) decrease size
  
  # 2. Open png file
  png(paste0("Outputs_Indicators_Historical/Eco_Ind/", colnames(biodiv)[i+2], ".png"), width = 750, height = 500)
  
  # 3. Make plot
  par(oma = c(0, 3, 0, 0))
  plot(indicators[[i]]$time, indicators[[i]]$predicted, ylim=c(ymin, ymax), xlim=c(2003,2022),xaxt="n",yaxt="n", ann=FALSE, las=1,
       type="l", xlab="Time", ylab=label, 
       main= names$y.label[i], cex.main=cex.title, cex.axis = 1.5, cex.lab = 1.5)
  title2=paste0(names$units[i])
  mtext(title2, cex=1.7, line=4, side=2)
  mtext("Year", side=1, line=3, cex=1.7, font=1, las=1)
  title = paste0(names$y.label[i])
  mtext(title, cex = 2, side =3, line =1.5)
  axis(1,at=seq(2005,2050,5),las=0, cex.axis=1.4)
  axis(2,las=2, ylim=c(0, ymax), cex.axis=1.4)
  polygon(c(indicators[[i]]$time, rev(indicators[[i]]$time)),c(indicators[[i]]$p95, rev(indicators[[i]]$p05)),
          col = "lightsteelblue1", border=NA)
  points(indicators[[i]]$time, indicators[[i]]$predicted, type='l', lwd=2, lty=1)
  
  leyenda <- cor.test(indicators[[i]]$time, indicators[[i]]$predicted, method="spearman")
  pvalor <- ifelse(round(leyenda$p.value, 3) == 0, "<0.001", round(leyenda$p.value, 3))
  rho <-round(leyenda$estimate,3)
  legend ("bottomleft", legend=c(paste("p-value =",  pvalor), paste("rho =", rho)),
          bty="n", cex=1.5, ncol=1, text.font=1.5, box.lty=0, y.intersp = 0.9)
  # 4. Close device to save plot
  dev.off()
}

rm(i, label, pvalor, rho, ymax, ymin,cex.title, leyenda)




