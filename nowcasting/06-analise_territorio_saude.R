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
centroides_areas <- read_csv("nowcasting/dados/mapas/abrangencia_cs/abrangencia_cs_centroides.csv", 
				      locale = locale(encoding = "WINDOWS-1252"))
# Nowcasting --------------------------------------------------------------

#Truncandos os dados da contaminação à notificação
tempo_contaminacao_inicio_sintomas <- 5
recorte <- (tempo_contaminacao_inicio_sintomas + tail(gd_t_sint_not$q80_smooth,1))
casos_por_territorio <- subset(casos_por_territorio, casos_por_territorio$INICIO_SINTOMAS <
			(tail(casos_por_territorio$INICIO_SINTOMAS,1) - recorte))

casos_por_territorio$TERRITORIO <- as.factor(casos_por_territorio$TERRITORIO) 

cortes <- levels(casos_por_territorio$TERRITORIO)

casos_por_territorio_union <- list()

for(i in seq_along(cortes)){
	#Cortando base por território
	casos_por_territorio_corte <- subset(casos_por_territorio, as.character(casos_por_territorio$TERRITORIO) %in% cortes[i])
	#Utilizando auto.arima para nowcasting do dados truncados
	casos_por_territorio_proj <- forecast(auto.arima(casos_por_territorio_corte$CASOS),
					      h=((Sys.Date()-1)-max(as.Date(casos_por_territorio_corte$INICIO_SINTOMAS))))$mean[1:((Sys.Date()-1)-max(as.Date(casos_por_territorio_corte$INICIO_SINTOMAS)))] %>%
		as.data.frame()
	casos_por_territorio_union[[i]] <- data.frame(CASOS = casos_por_territorio_proj,
						     INICIO_SINTOMAS = c(max(as.Date(casos_por_territorio_corte$INICIO_SINTOMAS)+1):(Sys.Date()-1)),
						     TERRITORIO = cortes[i])
	names(casos_por_territorio_union[[i]]) <- c("CASOS", "INICIO_SINTOMAS", "TERRITORIO")
	casos_por_territorio_union[[i]]$INICIO_SINTOMAS <- as.Date(casos_por_territorio_union[[i]]$INICIO_SINTOMAS, origin = "1970-01-01")
}

renda_territorio <- do.call(rbind, casos_por_territorio_union)

renda_territorio <- rbind(casos_por_territorio, renda_territorio) %>% as.data.frame()

ggplot(renda_territorio, aes(INICIO_SINTOMAS, CASOS, color = TERRITORIO)) +
	geom_smooth()

# Evolução por quantil de renda do território -----------------------------
territorio <- row.names(demografia)
demografia <- sapply(demografia, function(x)as.numeric(x)) %>% as.data.frame()
renda_dem <- data.frame(TERRITORIO = territorio,
			       RENDA = demografia$renda_med_pess,
			       BRANCO = demografia$cor_pele_1branca,
			       NEGRO = (demografia$cor_pele_2preta + demografia$cor_pele_4parda ),
			       ESCOL_0_4 = (demografia$escolaridade_nao_alfabetizados +
			       	    	   demografia$escolaridade_01_ano +
			       	    	   demografia$escolaridade_02_ano +
			       	     	   demografia$escolaridade_03_ano +
			       	    	   demografia$escolaridade_04_ano),
			       ESCOL_5_8 = (demografia$escolaridade_05_ano +
				     	  demografia$escolaridade_06_ano +
				    	  demografia$escolaridade_07_ano +
				    	  demografia$escolaridade_08_ano),
			       ESCOL_9_11 = (demografia$escolaridade_09_ano +
				     	demografia$escolaridade_10_ano +
				     	demografia$escolaridade_11_ano +
				     	demografia$escolaridade_12_ano),
			       ESCOL_MAIS_12 = (demografia$escolaridade_13_ano +
				      	demografia$escolaridade_14_ano +
				      	demografia$escolaridade_15_ano +
				      	demografia$escolaridade_16_ano +
				      	demografia$escolaridade_17_anos_ou_mais),
			       POP = demografia$populacao)

#renda_dem$TERRITORIO <- as.character(renda_dem$TERRITORIO)

renda_dem$RENDA <- as.numeric(as.character(renda_dem$RENDA))

base_renda <- merge(renda_dem, renda_territorio, by = "TERRITORIO", all = T)

base_renda <- subset(base_renda, base_renda$TERRITORIO != "cs_costa_da_lagoa")

base_renda$QUANT_RENDA <- ifelse(base_renda$RENDA < quantile(base_renda$RENDA,0.1), "Q01",
				 ifelse(base_renda$RENDA < quantile(base_renda$RENDA,0.2), "Q02", 
				        ifelse(base_renda$RENDA < quantile(base_renda$RENDA,0.3), "Q03",
				               ifelse(base_renda$RENDA < quantile(base_renda$RENDA,0.4), "Q04",
				                      ifelse(base_renda$RENDA < quantile(base_renda$RENDA,0.5), "Q05",
				                             ifelse(base_renda$RENDA < quantile(base_renda$RENDA,0.6), "Q06",
				                                    ifelse(base_renda$RENDA < quantile(base_renda$RENDA,0.7), "Q07",
				                                           ifelse(base_renda$RENDA < quantile(base_renda$RENDA,0.8), "Q08",
				                                                  ifelse(base_renda$RENDA < quantile(base_renda$RENDA,0.9), "Q09",
				                      "Q10")))))))))

base_renda$POP <- as.numeric(as.character(base_renda$POP))
base_renda$TAXA_CASOS <- base_renda$CASOS / base_renda$POP *1000 


base_renda_group <- base_renda %>% 
	group_by(INICIO_SINTOMAS,QUANT_RENDA) %>% 
	summarise(CASOS = sum(CASOS, na.rm = T),
		  POP = sum(POP, na.rm = T))

base_renda_group$TAXA_CASOS <- base_renda_group$CASOS / base_renda_group$POP *1000 

ggplot(base_renda_group, aes(INICIO_SINTOMAS, TAXA_CASOS, color = QUANT_RENDA))+
	geom_smooth()



# Raca-etnia --------------------------------------------------------------
base_renda$PROP_NEGRO_BRANCO <- base_renda$NEGRO / base_renda$BRANCO 
base_renda$QUANT_COR <- ifelse(base_renda$PROP_NEGRO_BRANCO < quantile(base_renda$PROP_NEGRO_BRANCO,0.333), "Q01",
				 ifelse(base_renda$PROP_NEGRO_BRANCO < quantile(base_renda$PROP_NEGRO_BRANCO,0.666), "Q02", "Q03"))
				                             

base_cor_group <- base_renda %>% 
	group_by(INICIO_SINTOMAS, QUANT_COR) %>% 
	summarise(CASOS = sum(CASOS, na.rm = T),
		  POP = sum(POP, na.rm = T))

base_cor_group$TAXA_CASOS <- base_cor_group$CASOS / base_cor_group$POP *1000 


ggplot(base_cor_group, aes(INICIO_SINTOMAS, TAXA_CASOS, color = QUANT_COR))+
	geom_smooth()


# Escolaridade ------------------------------------------------------------
base_renda$PROP_BAIXO_ESTUDO <- base_renda$ESCOL_5_8 / base_renda$ESCOL_MAIS_12 
base_renda$QUANT_EST <- ifelse(base_renda$PROP_BAIXO_ESTUDO < quantile(base_renda$PROP_BAIXO_ESTUDO,0.333), "Q01",
			       ifelse(base_renda$PROP_BAIXO_ESTUDO < quantile(base_renda$PROP_BAIXO_ESTUDO,0.666), "Q02", "Q03"))


base_est_group <- base_renda %>% 
	group_by(INICIO_SINTOMAS, QUANT_EST) %>% 
	summarise(CASOS = sum(CASOS, na.rm = T),
		  POP = sum(POP, na.rm = T))

base_est_group$TAXA_CASOS <- base_est_group$CASOS / base_est_group$POP *1000 


ggplot(base_est_group, aes(INICIO_SINTOMAS, TAXA_CASOS, color = QUANT_EST))+
	geom_smooth()




# Rt por quantil de renda da região --------------------------------------------------

write.csv(demografia, "demografia.csv", row.names = F)


