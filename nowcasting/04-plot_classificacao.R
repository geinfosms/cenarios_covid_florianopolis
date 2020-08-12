# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)
options(java.parameters = "-Xmx10g") #Evitar que o java tenha problemas de memória

# Pacotes -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(ggpubr)


# Importanto bases ---------------------------------------------------------------
source("nowcasting/03-predicao_dos_testes.R")
cum_base <- subset(cum_base, as.Date(cum_base$INICIO_SINTOMAS) < (Sys.Date()-3)) 
pred_base <- subset(pred_base, as.Date(pred_base$INICIO_SINTOMAS) < (Sys.Date()-3)) 


# Plotando RESULTADOs -----------------------------------------------------
plot1 <- ggplot(cum_base, aes(as.Date(INICIO_SINTOMAS), MEDIANA_CASOS, group = DADOS, color = DADOS))+
	geom_line(aes())+
	geom_ribbon(aes(ymin = II_025, ymax = II_975, fill = DADOS), alpha = 0.3, color = F)+
	theme_bw()+
	labs(y = "Number of Cases", 
	     x = "Date of Symptoms Onset")+
	ylim(0,350)

plot2 <- ggplot(cum_base, aes(as.Date(INICIO_SINTOMAS), MEDIANA_CASOS, group = DADOS, color = DADOS))+
	geom_smooth(se = F)+
	theme_bw()+
	labs(y = "Number of Cases - Smoothed", 
	     x = "Date of Symptoms Onset")+
	ylim(0,350)


plot3 <- ggplot(pred_base, aes(as.Date(INICIO_SINTOMAS), CUM_CASOS, group = DADOS, color = DADOS))+
	geom_line()+
	geom_ribbon(aes(ymin = CUM_II_025, ymax = CUM_II_975, fill = DADOS), alpha = 0.3, color = F)+
	theme_bw()+
	labs(y = "Number of Cases - Cummulated", 
	     x = "Date of Symptoms Onset")+
	scale_fill_discrete(name = "Label:")+
	scale_color_discrete(name = "Label:")

ggarrange(ggarrange(plot1, plot2, ncol = 2, labels = c("A", "B"), legend = F), plot3,
  		nrow = 2, labels = c("", "C"),legend = "bottom")


