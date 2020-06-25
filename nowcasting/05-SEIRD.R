# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)


# Pacotes -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(forecast)
library(reshape2)
library(EpiModel)
library(EpiEstim)
library(RcppRoll)


# Importação das Bases --------------------------------------------------------------------
#Utiliza-se os dados com nowcasting, ou seja, 
#o número esperado de pessoas com exame positivo ou negativo 
#se todos os pacientes notificados tivessem feito exame.

covid <- read_csv("nowcasting/dados/covid_preditos.csv")
covid <- subset(covid, covid$DADOS == "Nowcasted")
covid <- covid %>% dplyr::select(INICIO_SINTOMAS, MEDIANA_CASOS)
#Completando a base com dias que não tiveram casos
calendario <- data.frame("INICIO_SINTOMAS" = c(as.Date("2020-02-01"):(Sys.Date()-1)))
calendario$INICIO_SINTOMAS <- as.Date(calendario$INICIO_SINTOMAS, origin = "1970-01-01")
covid <- merge(covid, calendario, by = "INICIO_SINTOMAS", all = T)
covid[is.na(covid)] <- 0

# Definição dos parâmentro iniciais para o modelo  ------------------------------------------------

#################################################
#Ajsute para a truncagem à direita
#################################################

# A estimativa em tempo quase real requer não apenas inferir os tempos de infecção a partir dos dados observados, 
# mas também ajustar as observações ausentes de infecções recentes. 
# A ausência de infecções recentes nos dados analisados é conhecida como truncamento. 
# Sem ajuste para o truncamento correto, o número de infecções recentes parecerá artificialmente baixo 
# porque ainda não foram relatadas. (Gostic KM et al, 2020)
#Em Florianópolis, o tempo médio do início dos sintomas à notificação é de 3 dias.
#Dessa forma os casos que apresentaram sintomas hoje, só serão notificados em 3 dias.
#Para ajustar esse fenômeno, os dados serão truncados a direita em 4 dias (3 dias a partir da data de ontem
#quando os dados foram extraídos) e o modelo de suavização esponencial
#ou ARIMA, com o menor erro quadrado será ajustado para predizer o número de pessoas com início dos
#sintomas nos dias truncados.
covid$INICIO_SINTOMAS <- as.Date(covid$INICIO_SINTOMAS)
covid <- covid[order(covid$INICIO_SINTOMAS),]
covid <- subset(covid, covid$INICIO_SINTOMAS < Sys.Date() - 4)
covid_proj <- forecast::forecast(auto.arima(covid$MEDIANA_CASOS),h=4) %>% as.data.frame()
covid_proj$INICIO_SINTOMAS <- c((Sys.Date()-4) : (Sys.Date()-1))
covid_proj$INICIO_SINTOMAS <- as.Date(covid_proj$INICIO_SINTOMAS, origin = "1970-01-01")
covid_proj <- covid_proj %>% select(INICIO_SINTOMAS, `Point Forecast`) 
names(covid_proj)[2] <- "MEDIANA_CASOS"
covid <- rbind(covid, covid_proj) %>% as.data.frame()

#################################################
#Estimativa do número de expostos
#################################################
#O perído de incubação é definido como tempo do contágio ao início dos sintomas. (Prem K et al, 2020; He X et al, 2020).
#A média do período de incubação da COVID-19 foi estimada em 5.0 dias (IC95% 4.2, 6.0). (Prem K et al, 2020)
#O perído de exposição pode ser definido como tempo entre o contato com o vírus (início da incubação) ao início da infectividade.
#Os pacientes expostos iniciam o contágio, em média, 2 a 3 dias antes do início dos sintomas. (Wölfel R et all, 2020).
#Adotou-se, então, como período de exposição aquele entrea 5 dias e 3 dias antes do início dos sintomas. 
#Para se analisar a quantidade de pacientes expostos por dia, subtraiu-se 5 da data de início de sintomas e utilizou-se
#soma móvel de 2 dias (5 dia ao 3 dia antes do início dos sintomas).Para corrigir o truncados à direita, 
#utilizou-se o modelo de suavização esponencial ou ARIMA, com o menor erro quadrado.

expostos <- covid 
expostos$DATA <- (expostos$INICIO_SINTOMAS - 5) %>% as.character() 
expostos$INICIO_SINTOMAS <- NULL
expostos$EXPOSTOS <- roll_sum(covid$MEDIANA_CASOS,2, fill = 0, align = "right") #menos de 3 dias do início dos sintomas
expostos <- expostos %>% dplyr::select(DATA, EXPOSTOS)
expostos_proj <- forecast(auto.arima(expostos$EXPOSTOS),
			  h=((Sys.Date()-1)-max(as.Date(expostos$DATA))))$mean[1:((Sys.Date()-1)-max(as.Date(expostos$DATA)))] %>% 
	as.data.frame()
expostos_proj$DATA <- c((max(as.Date(expostos$DATA))+1):(Sys.Date()-1)) %>% as.Date(origin = "1970-01-01") %>% 
	as.character()
names(expostos_proj) <- c("EXPOSTOS", "DATA")
expostos <- rbind(expostos, expostos_proj) %>% as.data.frame()

#################################################
#Estimativa do número de infectantes
#################################################
#De acordo com estudos recentes(Zou L et al, 2020; To KKW et al, 2020), a carga viral diminuiu monotonicamente 
#após o início dos sintomas. Outro estudo de Wuhan detectou o vírums em pacientes 20 dias (mediana) 
#após o início dos sintomas (Zhou F et al, 2020). Contido após 8 didas do início dos sintomas, 
#o vírus vivo não pode mais ser cultivado, o que pode indicar o fim do perído de infectividade. (Wölfel R et al, 2020) 
#Esta pesquisa adotou, então, como período infectante aquelene entre dois dias antes e 8 dias após o início dos sintomas.
#Para a estimativa dos casos truncados à direita, utilizou-se o modelo de suavização esponencial
#ou ARIMA, com o menor erro quadrado
infectantes <- covid 
infectantes$DATA <- (infectantes$INICIO_SINTOMAS - 3) %>% as.character() 
infectantes$INICIO_SINTOMAS <- NULL
infectantes$INFECTANTES <- roll_sum(covid$MEDIANA_CASOS,11, fill = 0, align = "right") #menos de 3 dias do início dos sintomas
infectantes <- infectantes %>% dplyr::select(DATA, INFECTANTES)
infectantes_proj <- forecast(auto.arima(infectantes$INFECTANTES),
			     h=((Sys.Date()-1)-max(as.Date(infectantes$DATA))))$mean[1:((Sys.Date()-1)-max(as.Date(infectantes$DATA)))] %>% 
	as.data.frame()
infectantes_proj$DATA <- c((max(as.Date(infectantes$DATA))+1):(Sys.Date()-1)) %>% as.Date(origin = "1970-01-01") %>% 
	as.character()
names(infectantes_proj) <- c("INFECTANTES", "DATA")
infectantes <- rbind(infectantes, infectantes_proj) %>% as.data.frame()

#################################################
#Estimativa do número de óbitos
#################################################
obitos <- data.frame("DATA" = c("2020-03-31", "2020-04-02", "2020-04-07", "2020-04-21",
				"2020-04-24", "2020-04-30", "2020-05-04", "2020-06-06",
				"2020-06-10", "2020-06-12", "2020-06-19", "2020-06-20", 
				"2020-06-22", "2020-06-22"),
				"OBITOS" = rep(1,14))


#################################################
#Estimativa do número de recuperados
#################################################
#Considerou-se recuperado o indivíduo com mais de 8 dias de início dos sintomas e que não foi a óbito. Óbitos e recuperados
#são cumulativos
recuperados <- covid
recuperados$DATA <- (recuperados$INICIO_SINTOMAS + 9) %>% as.character()
recuperados$INICIO_SINTOMAS <- NULL
names(recuperados)[1] <- "RECUPERADOS"
recuperados <- merge(recuperados, obitos, by = "DATA", all = T)
recuperados[is.na(recuperados)] <- 0
recuperados$RECUPERADOS <- recuperados$RECUPERADOS - recuperados$OBITOS
#Base SEIRD
base <- merge(recuperados, expostos, by = "DATA", all = T)
base <- merge(base, infectantes, by = "DATA", all = T)
##Cumulativos e suceptíveis
base$DATA <- as.Date(base$DATA)
base <- base[order(base$DATA),]
base <- subset(base, base$RECUPERADOS > 0)
base$CUM_RECUPERADOS <- cumsum(base$RECUPERADOS)
base$CUM_OBITOS <- cumsum(base$OBITOS)

#################################################
#Estimativa do número de suscetíveis
#################################################
POP <- 500973
base$SUSCETIVEIS <- POP - base$CUM_RECUPERADOS - base$CUM_OBITOS - base$EXPOSTOS - base$INFECTANTES
base <- na.omit(base) #excluindo dadas posteriores a sysdate -1



# Estimando o Rt ----------------------------------------------------------
incidencia <- (POP - base$SUSCETIVEIS) #População - suscetíveis = número de pessoas que já foram infectadas
names(incidencia) <- base$DATA # a base precisa ser um vetor nomeado


res <- estimate_R(incidencia,
		  method =  "parametric_si",
		  config = make_config(list(
		  	mean_si = 4.8, std_si = 2.3))
)



res1 <- estimate_R(incidencia,
		   method =  "parametric_si",
		   config = make_config(list(si_parametric_distr = "L",
		   			  mean_si = 4.8, std_mean_si = 0.71,
		   			  min_mean_si = 3.8, max_mean_si = 6.1,
		   			  std_si = 2.3, std_std_si = 0.58,
		   			  min_std_si = 1.6, max_std_si = 3.5))
)

plot(res)
plot(res1)


res_melt <- cbind(res$R$`Quantile.0.025(R)`, res$R$`Median(R)`, res$R$`Quantile.0.975(R)`, 
		  base$DATA[(length(base$DATA) - (length(res$R$`Quantile.0.025(R)`)-1)):length(base$DATA)]) %>% as.data.frame()
names(res_melt) <- c("IC025", "MEDIA", "IC975", "DATA")
res_melt <- melt(res_melt, id.vars = "DATA")
res_melt$DATA <- as.Date(res_melt$DATA, origin = "1970-01-01")
ggplot(res_melt, aes(DATA, value, group = variable, color = variable))+
	geom_line()+
	theme_bw()


# Forecast do número de casos e dos óbitos --------------------------------

#Função SEIRD
SEIRD <- function(t, t0, parms) {
	with(as.list(c(t0, parms)), {
		
		# Population size
		num <- s.num + e.num + i.num + r.num + d.num
		
		# Effective contact rate and FOI from a rearrangement of Beta * c * D
		ce <- Rt / i.dur
		lambda <- ce * i.num/num
		
		dS <- -lambda*s.num
		dE <- lambda*s.num - (1/e.dur)*e.num
		dI <- (1/e.dur)*e.num - (1 - cfr)*(1/i.dur)*i.num - cfr*(1/i.dur)*i.num
		dR <- (1 - cfr)*(1/i.dur)*i.num
		dD <- cfr*(1/i.dur)*i.num
		
		#Outputs
		list(c(dS, dE, dI, dR, dD,
		       se.flow = lambda * s.num,
		       ei.flow = (1/e.dur) * e.num,
		       ir.flow = (1 - cfr)*(1/i.dur) * i.num,
		       id.flow = cfr*(1/i.dur)*i.num),
		     num = num,
		     s.prev = s.num / num,
		     e.prev = e.num / num,
		     i.prev = i.num / num,
		     ei.prev = (e.num + i.num)/num,
		     r.prev = r.num / num,
		     d.prev = d.num / num)
	})
}



#Parâmetros
##Número de reprodução efetivo = Rt = Estimado do nowcasting
##Duração da exposição (não transmissível) = e.dur = 3 dias
##Duração da infecção (trainsmissível) = i.dur = 11 dias
##Letalidade = cfr

#Estados iniciais
##Suscetiveis = s.num = População - Expostos - Infectados - Recuperados - Óbitos
##Expostos (não transmissores) = e.num = Estimado do nowcasting
##Infectados (transmissores) = i.num = Estimado do nowcasting
##Recuperados = r.num = Estimado do nowcasting
##Óbitos = d.num = Estimado do nowcasting

crf_atual <- tail(base$CUM_OBITOS,1)[1]/(POP - tail(base$SUSCETIVEIS,1)[1]) #População - suscetíveis = número de pessoas que já foram infectadas

param <- param.dcm(Rt = c(tail(res$R$`Quantile.0.025(R)`,1), tail(res$R$`Mean(R)`,1), tail(res$R$`Quantile.0.975(R)`,1)), 
		   e.dur = 3, i.dur = 11, cfr = crf_atual)
init <- init.dcm(s.num = tail(base$SUSCETIVEIS,1)[1], 
		 e.num = tail(base$EXPOSTOS,1)[1], 
		 i.num = tail(base$INFECTANTES,1)[1], 
		 r.num = tail(base$CUM_RECUPERADOS,1)[1], 
		 d.num = tail(base$CUM_OBITOS,1)[1],
		 se.flow = 0, 
		 ei.flow = 0, 
		 ir.flow = 0, 
		 id.flow = 0)
projecao <- 14
control <- control.dcm(nsteps = projecao, new.mod = SEIRD)
mod <- dcm(param, init, control)
mod




par(mfrow = c(1, 2))
plot(mod, y = "i.num", main = "Number Infected")
plot(mod, y = "d.num", main = "Number Death")


#Checagem do modelo: Pop (500973) - Suscetíveis = Expostos + Infectantes + Recuperados + Óbitos
POP - tail(mod$epi$s.num,1)[2] 
tail(mod$epi$e.num,1)[2] + tail(mod$epi$i.num,1)[2] + tail(mod$epi$r.num,1)[2] + tail(mod$epi$d.num,1)[2]


#Gráficos
mod_melt <- mod$epi$i.num
names(mod_melt) <- c("IC025", "MEDIANA", "IC975")
mod_melt$INICIO_SINTOMAS <- c((Sys.Date()+1):(Sys.Date()+projecao))
mod_melt$INICIO_SINTOMAS <- mod_melt$INICIO_SINTOMAS 

mod_melt <- melt(mod_melt, id.vars = "INICIO_SINTOMAS")
names(mod_melt) <- c("DATA", "DADOS", "INFECTANTES")
mod_melt$DATA <- as.Date(mod_melt$DATA, origin = "1970-01-01")

base <- base %>% dplyr::select(DATA, INFECTANTES)
base$DADOS <- "Nowcasted"

mod_melt <- rbind(base, mod_melt)
mod_melt <- subset(mod_melt, mod_melt$DADOS != "Measured")

ggplot(mod_melt, aes(DATA, INFECTANTES, color = DADOS))+
	geom_line()+
	theme_bw()

