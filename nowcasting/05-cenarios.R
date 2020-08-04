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
library(gam)
library(foreign)
#devtools::install_github("tidyverse/googlesheets4")
library(googlesheets4)
library(lubridate)
library(RPostgreSQL)
library(magrittr)
library(RcppRoll)


# Nowcasting --------------------------------------------------------------------
#Utiliza-se os dados da classificação, ou seja,
#o número esperado de pessoas com exame positivo ou negativo
#se todos os pacientes notificados tivessem feito exame.
source("nowcasting/03-predicao_dos_testes.R")

covid <- cum_base
covid <- subset(covid, covid$DADOS == "Nowcasted")
covid <- covid %>% dplyr::select(INICIO_SINTOMAS, MEDIANA_CASOS)
#Completando a base com dias que não tiveram casos
calendario <- data.frame("INICIO_SINTOMAS" = c(as.Date("2020-02-01"):(Sys.Date()-1)))
calendario$INICIO_SINTOMAS <- as.Date(calendario$INICIO_SINTOMAS, origin = "1970-01-01")
covid <- merge(covid, calendario, by = "INICIO_SINTOMAS", all = T)
covid[is.na(covid)] <- 0

covid$MEDIANA_CASOS <- loess(covid$MEDIANA_CASOS ~ as.numeric(covid$INICIO_SINTOMAS))$fitted
covid <- subset(covid, covid$MEDIANA_CASOS >=0)

# Definição dos parâmentro iniciais para o modelo  ------------------------------------------------

#################################################
#Ajsute para a truncagem à direita
#################################################

# A estimativa em tempo quase real requer não apenas inferir os tempos de infecção a partir dos dados observados,
# mas também ajustar as observações ausentes de infecções recentes.
# A ausência de infecções recentes nos dados analisados é conhecida como truncamento.
# Sem ajuste para o truncamento correto, o número de infecções recentes parecerá artificialmente baixo
# porque ainda não foram relatadas. (Gostic KM et al, 2020)
#Para corrigir isso, os casos classificados em um período menor que o do tempo do contágio ao tempo de notificação
#serão truncados e utilizar-se-á a função auto.arima, que seleciona entre modelos de arima e de
#suavização esponencial, para o nowcasting desse período.
#O perído de incubação é definido como tempo do contágio ao início dos sintomas. (Prem K et al, 2020; He X et al, 2020).
#A média do período de incubação da COVID-19 foi estimada em 5.0 dias (IC95% 4.2, 6.0). (Prem K et al, 2020)
#Assim, utilizar-se-á 5 dias como tempo de contato com o vírus ao início dos sintomas. O tempo do início dos sintomas
#à notificação pode ser inferido dos dados da SMS.

#################################################
#Estimativa do número de óbitos, internações e tempos
#################################################
source("nowcasting/00-tempos.R")
source("nowcasting/00_conexoes_com_a_base_obitos_srag.R")

# srag <- read_csv("nowcasting/dados/srag_anonim.csv")
# obitos <- read_csv("nowcasting/dados/obitos_anonim.csv")
# gd_t_sint_not<- read_csv("nowcasting/dados/gd_t_sint_not.csv")
# leitos_uti <- read_csv("nowcasting/dados/leitos_uti.csv")
inter_recup <- read_csv("nowcasting/dados/inter_recup.csv")


#Truncandos os dados da contaminação à notificação
tempo_contaminacao_inicio_sintomas <- 5
recorte <- (tempo_contaminacao_inicio_sintomas + tail(gd_t_sint_not$q80_smooth,1))
covid <- subset(covid, covid$INICIO_SINTOMAS <
			(tail(covid$INICIO_SINTOMAS,1) - recorte))

#Utilizando auto.arima para nowcasting do dados truncados
covid_proj <- forecast(auto.arima(covid$MEDIANA_CASOS),
		       h=((Sys.Date()-1)-max(as.Date(covid$INICIO_SINTOMAS))))$mean[1:((Sys.Date()-1)-max(as.Date(covid$INICIO_SINTOMAS)))] %>%
	as.data.frame()

covid_proj <- data.frame(MEDIANA_CASOS = covid_proj,
			 INICIO_SINTOMAS = c(max(as.Date(covid$INICIO_SINTOMAS)+1):(Sys.Date()-1)))

names(covid_proj) <- c("MEDIANA_CASOS", "INICIO_SINTOMAS")
covid_proj$INICIO_SINTOMAS <- as.Date(covid_proj$INICIO_SINTOMAS, origin = "1970-01-01")

covid <- rbind(covid, covid_proj) %>% as.data.frame()

ggplot(covid, aes(INICIO_SINTOMAS, MEDIANA_CASOS, group = 1)) +
	geom_line()

covid$MEDIANA_CASOS <- round(covid$MEDIANA_CASOS, digits = 0)

id_covid <-  "https://docs.google.com/spreadsheets/d/1Ya0urjD781uutQwRu95Pbc9JXsilb6qmnDHU8Kg7nq8/edit#gid=868562419"
write_sheet(id_covid,"covid_nowcasted", data = covid)
write.csv(covid,"nowcasting/dados/covid_nowcasted.csv", row.names = F)


#################################################
#Estimativa do número de expostos
#################################################
#O perído de exposição pode ser definido como tempo entre o contato com o vírus (início da incubação) ao início da infectividade.
#Os pacientes expostos iniciam o contágio, em média, 2 a 3 dias antes do início dos sintomas. (Wölfel R et all, 2020).
#Adotou-se, então, como período de exposição aquele entrea 5 dias e 3 dias antes do início dos sintomas.
#Para se analisar a quantidade de pacientes expostos por dia, subtraiu-se 5 da data de início de sintomas e utilizou-se
#soma móvel de 2 dias (5 dia ao 3 dia antes do início dos sintomas).Para corrigir o truncados à direita,
#utilizou-se o modelo de suavização esponencial ou ARIMA, com o menor erro quadrado.
expostos <- covid
expostos$DATA <- (expostos$INICIO_SINTOMAS - 5) %>% as.character()
expostos$INICIO_SINTOMAS <- NULL
expostos$EXPOSTOS <- roll_sum(expostos$MEDIANA_CASOS,2, fill = 0, align = "right") #menos de 3 dias do início dos sintomas
expostos_proj <- forecast(auto.arima(expostos$EXPOSTOS),
			  h=((Sys.Date()-1)-max(as.Date(expostos$DATA))))$mean[1:((Sys.Date()-1)-max(as.Date(expostos$DATA)))] %>%
	as.data.frame()
expostos_proj$CASOS_NOVOS <- forecast(auto.arima(expostos$MEDIANA_CASOS),
				      h=((Sys.Date()-1)-max(as.Date(expostos$DATA))))$mean[1:((Sys.Date()-1)-max(as.Date(expostos$DATA)))]

expostos_proj$DATA <- c((max(as.Date(expostos$DATA))+1):(Sys.Date()-1)) %>% as.Date(origin = "1970-01-01") %>%
	as.character()
names(expostos_proj) <- c("EXPOSTOS", "MEDIANA_CASOS", "DATA")
expostos <- rbind(expostos, expostos_proj) %>% as.data.frame()
names(expostos)[1] <- "CASOS_NOVOS"


#################################################
#Estimativa do número de infectantes
#################################################
#De acordo com estudos recentes(Zou L et al, 2020; To KKW et al, 2020), a carga viral diminui monotonicamente
#após o início dos sintomas. Outro estudo de Wuhan detectou o vírums em pacientes 20 dias (mediana)
#após o início dos sintomas (Zhou F et al, 2020). Contudo, após 8 didas do início dos sintomas,
#o vírus vivo não pode mais ser cultivado, o que pode indicar o fim do perído de infectividade. (Wölfel R et al, 2020)
#Esta pesquisa adotou, então, como período infectante aquele entre dois dias antes e 8 dias após o início dos sintomas.
#Para a estimativa dos casos truncados, utilizou-se o modelo de suavização esponencial
#ou ARIMA, com o menor erro quadrado
infectantes <- covid
infectantes$DATA <- (infectantes$INICIO_SINTOMAS - 3) %>% as.character()
infectantes$INICIO_SINTOMAS <- NULL
infectantes$INFECTANTES <- roll_sum(infectantes$MEDIANA_CASOS,11, fill = 0, align = "right") 
infectantes <- infectantes %>% dplyr::select(DATA, INFECTANTES)
infectantes_proj <- forecast(auto.arima(infectantes$INFECTANTES),
			     h=((Sys.Date()-1)-max(as.Date(infectantes$DATA))))$mean[1:((Sys.Date()-1)-max(as.Date(infectantes$DATA)))] %>%
	as.data.frame()
infectantes_proj$DATA <- c((max(as.Date(infectantes$DATA))+1):(Sys.Date()-1)) %>% as.Date(origin = "1970-01-01") %>%
	as.character()
names(infectantes_proj) <- c("INFECTANTES", "DATA")
infectantes <- rbind(infectantes, infectantes_proj) %>% as.data.frame()



#################################################
#Estimativa do número de recuperados
#################################################
#Considerou-se recuperado o indivíduo com mais de 8 dias de início dos sintomas e que não foi a óbito. Óbitos e recuperados
#são cumulativos
recuperados <- covid
recuperados$DATA <- (recuperados$INICIO_SINTOMAS + 9) %>% as.character()
recuperados$INICIO_SINTOMAS <- NULL
names(recuperados)[1] <- "RECUPERADOS"
recuperados$DATA <- as.Date(recuperados$DATA)
recuperados <- merge(recuperados, obitos, by = "DATA", all = T)
recuperados[is.na(recuperados)] <- 0
recuperados$RECUPERADOS <- recuperados$RECUPERADOS - recuperados$OBITOS
#Base SEIRD
expostos$DATA <- as.Date(expostos$DATA)
infectantes$DATA <- as.Date(infectantes$DATA)

base <- merge(recuperados, expostos, by = "DATA", all = T)
base <- merge(base, infectantes, by = "DATA", all = T)
##Cumulativos e suceptíveis
base$DATA <- as.Date(base$DATA, "%Y-%m-%d")
base <- base[order(base$DATA),]
base[is.na(base$RECUPERADOS), names(base) == "RECUPERADOS"] <- 0
base$CUM_RECUPERADOS <- cumsum(base$RECUPERADOS)
base[is.na(base$OBITOS), names(base) == "OBITOS"] <- 0
base$CUM_OBITOS <- cumsum(base$OBITOS)

#################################################
#Estimativa do número de suscetíveis
#################################################
POP <- 500973
base$SUSCETIVEIS <- POP - base$CUM_RECUPERADOS - base$CUM_OBITOS - base$EXPOSTOS - base$INFECTANTES

#################################################
#merge
#################################################
#merge dos dados de ocupação de leitos e número de intenação com os outros dados
base <- merge(base, leitos_uti, by = "DATA", all = T)
base <- merge(base, inter_recup, by.x = "DATA", by.y = "DT_ENTUTI", all = T)
base[is.na(base$LEITOS_UTI), names(base) == "LEITOS_UTI"] <- 0
base[is.na(base$INTERNACAO_UTI), names(base) == "INTERNACAO_UTI"] <- 0
base$CUM_INTERNACAO_UTI <- cumsum(base$INTERNACAO_UTI)
base[is.na(base$RECUPERADOS_UTI), names(base) == "RECUPERADOS_UTI"] <- 0
base$CUM_RECUPERADOS_UTI <- cumsum(base$RECUPERADOS_UTI)
base <- subset(base, base$DATA < Sys.Date())
base <- na.omit(base)

write.csv(base, "nowcasting/dados/base_nowcasting.csv", row.names = F)

ggplot(base, aes(DATA, LEITOS_UTI))+
	geom_line()

# Estimando o Rt ----------------------------------------------------------
source("nowcasting/apeEstim.R")
source("nowcasting/apePredPost.R")

incidencia <- base
incidencia <- subset(incidencia, incidencia$DATA > c(Sys.Date()-92)) #Utilizando dados dos últimos trës meses
incidencia_proj <- incidencia %>% dplyr::select(DATA, CASOS_NOVOS)

#Left trunc
trunc <- 0

Icovid <- incidencia$CASOS_NOVOS #Incidência
gencovid <- EpiEstim::discr_si(c(0:max(incidencia$CASOS_NOVOS)), mu = 4.8, sigma = 2.3) #distribuição gama
Lcovid = overall_infectivity(Icovid, gencovid)

#Priors and settings
Rprior = c(1, 5); a = 0.025 #Confidence interval level

#Clean Lam vectors of NAs
Lcovid[is.na(Lcovid)] = 0# <------ important

#Best estimates and prediction
Rmodcovid <- apeEstim(Icovid, gencovid, Lcovid, Rprior, a, trunc, "covid")
Rcovid <- Rmodcovid[[2]][[4]]
RcovidCI_025 <- Rmodcovid[[2]][[5]][1,]
RcovidCI_975 <- Rmodcovid[[2]][[5]][2,]
DATA <- tail(incidencia$DATA,-1)
res_base <- data.frame(DATA = DATA, MEDIA = Rcovid, IC025 = RcovidCI_025, IC975 = RcovidCI_975)
res_base <- subset(res_base, res_base$DATA >= (Sys.Date() -61))

res_melt <- melt(res_base, id.vars = "DATA")
res_melt$DATA <- as.Date(res_melt$DATA, origin = "1970-01-01")
write.csv(res_melt, "nowcasting/dados/rt.csv",row.names = F)
res_base$DATA <- as.Date(res_base$DATA, origin = "1970-01-01")
res_base_14dias <- subset(res_base, res_base$DATA > Sys.Date() -15)
write_sheet(id_covid,"reff", data = res_base_14dias)
write.csv(res_base_14dias, "nowcasting/dados/res_base_14dias.csv", row.names = F)


ggplot(res_melt, aes(DATA, value, group = variable, color = variable))+
	geom_line()+
	geom_hline(yintercept = 1) +
	theme_bw()



# Forecast do número de casos e dos óbitos --------------------------------
#Estados Iniciais
#Estados iniciais
##Suscetiveis = s.num = População - Expostos - Infectados - Recuperados - Óbitos
##Expostos (não transmissores) = e.num = Estimado do nowcasting
##Infectados (transmissores) = i.num = Estimado do nowcasting
##Recuperados = r.num = Estimado do nowcasting
##Óbitos = d.num = Estimado do nowcasting
#Parâmetros
##Número de reprodução efetivo = Rt = Estimado do nowcasting
##Duração da exposição (não transmissível) = e.dur = 2 dias
##Duração da infecção (trainsmissível) = i.dur = 12 dias (3 antes do início dos sintomas, 1 do início dos sintomas e 8 após o início dos sintomas)
#prob1 = Taxa de internação
##prob2 = Taxa de letalidade entre os internados
tempos <- read_csv("nowcasting/dados/tempos.csv")
uti.dur.mean <- tempos$uti.dur.mean
tempo_infectividade_uti <- tempos$tempo_infectividade_uti
tempo_uti_obito <- tempos$tempo_uti_obito
tempo_uti_recuperacao <- tempos$tempo_uti_recuperacao

S <- tail(base$SUSCETIVEIS,1)[1]
E <- tail(base$EXPOSTOS,1)[1]
I <- tail(base$INFECTANTES,1)[1]
H <- tail(base$LEITOS_UTI,1)[1]
R <- tail(base$CUM_RECUPERADOS,1)[1]
D <- tail(base$CUM_OBITOS,1)[1]
N <- S + E + I +H +  R + D
ir.dur <- 12
ei.dur <- 2
ih.dur <- tempo_infectividade_uti
hd.dur <- tempo_uti_obito
hr.dur <- tempo_uti_recuperacao
etha <- 1/ir.dur
betha <- 1/ei.dur
delta <- 1/ih.dur
mu <- 1/hd.dur
epsilon <- 1/hr.dur

#Estimativa das probabilidade para o modelo
base$TOTAL <- base$EXPOSTOS + base$INFECTANTES + base$CUM_RECUPERADOS + base$CUM_OBITOS
base$CUM_CASOS_NOVOS <- cumsum(base$CASOS_NOVOS)
defasagem_uti_recuperacao <- nrow(base) - (ei.dur + ih.dur + hr.dur) #Lag de tempo para calcular a probabilidade da pessoa entrar na UTI e se recuperar
defasagem_uti_obito <- nrow(base) - (ei.dur + ih.dur + hd.dur) #Lag de tempo para calcular a probabilidade da pessoa entrar na UTI e falecer
prob1 <- tail(base$CUM_RECUPERADOS_UTI,1)[1]/base[,names(base) == "TOTAL"][defasagem_uti_recuperacao] #Taxa de hospitalizados recuperados de UTI - denominador usando a defasagem
prob2 <- tail(base$CUM_OBITOS,1)[1]/base[,names(base) == "TOTAL"][defasagem_uti_obito] #Taxa de letalidade entre os internados em UTI - denominador usando a defasagem
prob <- prob1 + prob2

init <- init.dcm(S = S,
		 E = E,
		 I = I,
		 H = H,
		 R = R,
		 D = D,
		 se.flow = 0,
		 ei.flow = 0,
		 ir.flow = 0,
		 ih.flow = 0,
		 hr.flow = 0,
		 hd.flow = 0
)



param <- param.dcm(Rt = c(tail(res_base$IC025,1),
			  tail(res_base$MEDIA,1),
			  tail(res_base$IC975,1)),
		   etha = 1/ir.dur,
		   betha = 1/ei.dur,
		   delta = 1/ih.dur,
		   mu = 1/hd.dur,
		   epsilon = 1/hr.dur,
		   prob1 = prob1,
		   prob2 = prob2,
		   prob = prob
)


#Função SEIRD
SEIHRD <- function(t, t0, parms) {
	with(as.list(c(t0, parms)), {
		
		N <- S + E + I + H +  R + D
		 
		alpha <- etha * Rt * I/N
		
		#Equações diferenciais
		dS <- -alpha*S
		dE <- alpha*S - betha*E
		dI <- betha*E - prob*delta*I - (1-prob)*etha*I
		dH <- prob*delta*I - (prob2/prob)*mu*H - (prob1/prob)*epsilon*H
		dR <- (1 - prob)*etha*I + (prob1/prob)*epsilon*H
		dD <- (prob2/prob)*mu*H
		
		#Outputs
		list(c(dS, dE, dI, dH, dR, dD,
		       se.flow = alpha * S,
		       ei.flow = betha * E,
		       ir.flow = (1 - prob)*etha*I,
		       ih.flow = prob1*delta*I,
		       hr.flow = (prob1/prob)*epsilon*H,
		       hd.flow = (prob2/prob)*mu*H),
		     num = N,
		     s.prev = S / N,
		     e.prev = E / N,
		     i.prev = I / N,
		     ei.prev = (E + I)/N,
		     h.prev = H /N,
		     r.prev = R / N,
		     d.prev = D / N)
	})
}


#Resolvendo as equações diferenciais
projecao <- 21
control <- control.dcm(nsteps = projecao, new.mod = SEIHRD)
mod <- dcm(param, init, control)



######################################
#Cenário Rt 1 - IC2.5
######################################
resultados_cenario_1 <- data.frame(SUSCETIVEIS = mod$epi$S$run1,
				   EXPOSTOS = mod$epi$E$run1,
				   INFECTANTES = mod$epi$I$run1,
				   LEITOS_UTI = mod$epi$H$run1,
				   CUM_RECUPERADOS = mod$epi$R$run1,
				   CUM_OBITOS = mod$epi$D$run1
)

resultados_cenario_1 <- resultados_cenario_1[-1,]
resultados_cenario_1$DATA <- c((Sys.Date()):(Sys.Date()+projecao-2))
resultados_cenario_1$DATA  <- as.Date(resultados_cenario_1$DATA , origin = "1970-01-01")
base_select <- base %>% dplyr::select(DATA,SUSCETIVEIS, CUM_RECUPERADOS, EXPOSTOS, INFECTANTES, CUM_OBITOS, LEITOS_UTI)
resultados_cenario_1 <- rbind(base_select, resultados_cenario_1)
names(resultados_cenario_1) <-c("DATA", "SUSCETIVEIS_CENARIO_1", "CUM_RECUPERADOS_CENARIO_1", "EXPOSTOS_CENARIO_1", "INFECTANTES_CENARIO_1", "CUM_OBITOS_CENARIO_1", "LEITOS_UTI_CENARIO_1")

######################################
#Cenário 2 - Rt Mediana
######################################
resultados_cenario_2 <- data.frame(SUSCETIVEIS = mod$epi$S$run2,
				   EXPOSTOS = mod$epi$E$run2,
				   INFECTANTES = mod$epi$I$run2,
				   LEITOS_UTI = mod$epi$H$run2,
				   CUM_RECUPERADOS = mod$epi$R$run2,
				   CUM_OBITOS = mod$epi$D$run2)

resultados_cenario_2 <- resultados_cenario_2[-1,]
resultados_cenario_2$DATA <- c((Sys.Date()):(Sys.Date()+projecao-2))
resultados_cenario_2$DATA  <- as.Date(resultados_cenario_2$DATA , origin = "1970-01-01")
base_select <- base %>% dplyr::select(DATA,SUSCETIVEIS, CUM_RECUPERADOS, EXPOSTOS, INFECTANTES, CUM_OBITOS, LEITOS_UTI)
resultados_cenario_2 <- rbind(base_select, resultados_cenario_2)
names(resultados_cenario_2) <-c("DATA", "SUSCETIVEIS_CENARIO_2", "CUM_RECUPERADOS_CENARIO_2", "EXPOSTOS_CENARIO_2", "INFECTANTES_CENARIO_2", "CUM_OBITOS_CENARIO_2", "LEITOS_UTI_CENARIO_2")

######################################
#Cenário 3 - Rt IC975
######################################
resultados_cenario_3 <- data.frame(SUSCETIVEIS = mod$epi$S$run3,
				   EXPOSTOS = mod$epi$E$run3,
				   INFECTANTES = mod$epi$I$run3,
				   LEITOS_UTI = mod$epi$H$run3,
				   CUM_RECUPERADOS = mod$epi$R$run3,
				   CUM_OBITOS = mod$epi$D$run3)

resultados_cenario_3 <- resultados_cenario_3[-1,]
resultados_cenario_3$DATA <- c((Sys.Date()):(Sys.Date()+projecao-2))
resultados_cenario_3$DATA  <- as.Date(resultados_cenario_3$DATA , origin = "1970-01-01")
base_select <- base %>% dplyr::select(DATA,SUSCETIVEIS, CUM_RECUPERADOS, EXPOSTOS, INFECTANTES, CUM_OBITOS, LEITOS_UTI)
resultados_cenario_3 <- rbind(base_select, resultados_cenario_3)
names(resultados_cenario_3) <-c("DATA", "SUSCETIVEIS_CENARIO_3", "CUM_RECUPERADOS_CENARIO_3", "EXPOSTOS_CENARIO_3", "INFECTANTES_CENARIO_3", "CUM_OBITOS_CENARIO_3", "LEITOS_UTI_CENARIO_3")

#Unindo as bases de resultados
resultados <- merge(resultados_cenario_1, resultados_cenario_2, by= "DATA", all = T)
resultados <- merge(resultados_cenario_3, resultados, by= "DATA", all = T)


write.csv(resultados, "nowcasting/dados/resultados.csv", row.names = F)
#resultados$DATA <- as.Date(resultados$DATA, origin = "1970-01-01")
write_sheet(id_covid,"resultados", data = resultados)


#Análise dos resultados

resultados_melt <- resultados %>% dplyr::select(DATA, CUM_OBITOS_CENARIO_2, LEITOS_UTI_CENARIO_2)
resultados_melt <- melt(resultados_melt, id.vars = "DATA")

ggplot(resultados_melt, aes(DATA, value, color = variable))+
	geom_line()+
	theme_bw()


