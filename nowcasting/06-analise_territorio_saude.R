# Trabalhar com áreas de saúde
# Dividir as ações em 2 tipos: Fechadas e Abertas com restrição
# Calcular o contrafactual para cada área de saúde como não tratado
# Ver a questão do tempo de tratamento (dose por dia) (número de infectados por dia)
# Problema pode ser com a adesão ao tratamento
# A simples prescrição de um remédio não vai produzir efeito. Ele só poderá fazer efeito se o paciente tomar o remédio
# Não avaliaremos no sentido da abertura. Apenas dos fechamentos
# guido imbens the book of why. https://arxiv.org/pdf/1907.07271.pdf


# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)
options(java.parameters = "-Xmx8g") #Evitar que o java tenha problemas de memória

# Pacotes -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(caret)
library(mlr)
library(forecast)
library(doParallel)
library(parallelMap)
library(tigerstats)
library(matrixStats)
library(httr)
library(RCurl)

# Importanto  ---------------------------------------------------------------
casos_por_territorio <- read_csv("nowcasting/dados/casos_por_territorio.csv")

# Nowcasting --------------------------------------------------------------

#Truncandos os dados da contaminação à notificação
tempo_contaminacao_inicio_sintomas <- 5
recorte <- (tempo_contaminacao_inicio_sintomas + tail(gd_t_sint_not$q80_smooth,1))
casos_por_territorio <- subset(casos_por_territorio, casos_por_territorio$INICIO_SINTOMAS <
			(tail(casos_por_territorio$INICIO_SINTOMAS,1) - recorte))

casos_por_territorio$TERRITORIO <- as.factor(casos_por_territorio$TERRITORIO) 

cortes <- levels(casos_por_territorio$TERRITORIO)
casos_por_territorio_proj <- list()
for(i in seq_along(casos_por_territorio_corte$TERRITORIO)){
	#Cortando base por território
	casos_por_territorio_corte <- subset(casos_por_territorio, as.numeric(casos_por_territorio$TERRITORIO) == 1)
	#Utilizando auto.arima para nowcasting do dados truncados
	casos_por_territorio_proj[[i]] <- forecast(auto.arima(casos_por_territorio_corte$CASOS),
					      h=((Sys.Date()-1)-max(as.Date(casos_por_territorio_corte$INICIO_SINTOMAS))))$mean[1:((Sys.Date()-1)-max(as.Date(casos_por_territorio_corte$INICIO_SINTOMAS)))] %>%
		as.data.frame()
	
	casos_por_territorio_proj[[i]] <- data.frame(CASOS = casos_por_territorio_proj[[i]],
						INICIO_SINTOMAS = c(max(as.Date(casos_por_territorio_corte$INICIO_SINTOMAS)+1):(Sys.Date()-1)))
	
	names(casos_por_territorio_proj[[i]]) <- c("CASOS", "INICIO_SINTOMAS")
	casos_por_territorio_proj[[i]]$INICIO_SINTOMAS <- as.Date(casos_por_territorio_proj[[i]]$INICIO_SINTOMAS, origin = "1970-01-01")
}

casos_por_territorio <- rbind(casos_por_territorio, casos_por_territorio_proj) %>% as.data.frame()

ggplot(casos_por_territorio, aes(INICIO_SINTOMAS, CASOS, group = 1)) +
	geom_line()

casos_por_territorio$CASOS <- round(casos_por_territorio$CASOS, digits = 0)


# Evolução por quantil de renda do território -----------------------------
casos_por_territorio$TERRITORIO <- as.character(casos_por_territorio$TERRITORIO)
renda_territorio <- data.frame(TERRITORIO = demografia$territorio,
			       RENDA = demografia$renda_med_pess,
			       POP = demografia$populacao)
renda_territorio$TERRITORIO <- as.character(renda_territorio$TERRITORIO)
renda_territorio$RENDA <- as.numeric(as.character(renda_territorio$RENDA))
base_renda <- merge(renda_territorio, casos_por_territorio, by = "TERRITORIO", all = T)
base_renda <- subset(base_renda, base_renda$TERRITORIO != "cs_costa_da_lagoa")
base_renda$QUANT_RENDA <- ifelse(base_renda$RENDA < quantile(base_renda$RENDA,0.2), "Q1",
				 ifelse(base_renda$RENDA < quantile(base_renda$RENDA,0.4), "Q2",
				        ifelse(base_renda$RENDA < quantile(base_renda$RENDA,0.6), "Q3",
				               ifelse(base_renda$RENDA < quantile(base_renda$RENDA,0.8), "Q4","Q5"))))
base_renda$POP <- as.numeric(as.character(base_renda$POP))
base_renda <- base_renda %>% 
	group_by(INICIO_SINTOMAS,QUANT_RENDA) %>% 
	summarise(CASOS = sum(CASOS, na.rm = T),
		  POP = sum(POP, na.rm = T))
base_renda$TAXA_CASOS <- base_renda$CASOS / base_renda$POP *1000 



base_renda <- subset(base_renda, base_renda$INICIO_SINTOMAS < (Sys.Date()-7))

ggplot(base_renda, aes(INICIO_SINTOMAS, TAXA_CASOS, color = QUANT_RENDA))+
	geom_smooth()

# Rt por quantil de renda da região --------------------------------------------------


