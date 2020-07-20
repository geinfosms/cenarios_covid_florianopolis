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


source("nowcasting/apeEstim.R")
source("nowcasting/apePredPost.R")



# Nowcasting --------------------------------------------------------------------
#Utiliza-se os dados da classificação, ou seja,
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

#Importando base para cáculo do tempo entre notificação e início dos sintomas
not_fname <- paste0('nowcasting/dados/notificacoes_i_',
                    today(), '.csv')
tst_fname <- paste0('nowcasting/dados/testes_',
                    today(), '.csv')

not_query <- read_file('nowcasting/dados/not_query.sql')
tst_query <- read_file('nowcasting/dados/tst_query.sql')

update_with_query <- function(fname, query) {
  if (file.exists(fname)) {
    return(read.csv(fname))
  } else {
    drv <- dbDriver('PostgreSQL')
    con <- dbConnect(drv, dbname = 'florianopolis_saude',
                     host = 'dbsaudeflorianopolisleitura.celk.com.br',
                     port = 5432,
                     user = 'matheus_andrade',
                     password = 'ma63148F13498')
    df <- dbGetQuery(con, query)
    write_csv(df, fname)
    return(df)
  }
}

# Load and tidy
not_data <- update_with_query(not_fname, not_query)
not_data$dt_nascimento <- as.Date(not_data$dt_nascimento, format = '%d/%m/%Y')
not_data$dt_coleta_teste <- as.Date(not_data$dt_coleta_teste, format = '%d/%m/%Y')
not_data$dt_notificacao <- as.Date(not_data$dt_notificacao, format = '%d/%m/%Y')
not_data$dt_inicio_sintomas <- as.Date(not_data$dt_inicio_sintomas, format = '%d/%m/%Y')
not_data$dt_teste_result <- as.Date(not_data$dt_teste_result, format = '%d/%m/%Y')
not_data$dt_encerramento <- as.Date(not_data$dt_encerramento, format = '%d/%m/%Y')

tst_data <- update_with_query(tst_fname, tst_query) %>%
  mutate(dt_teste = as.Date(dt_teste))

not_data %<>% filter(mun_residencia == 'FLORIANOPOLIS')
not_data %<>%
  mutate(t_sint_not = ifelse((dt_notificacao - dt_inicio_sintomas) < 0, 0, dt_notificacao - dt_inicio_sintomas),
         t_not_coleta = ifelse(dt_coleta_teste >= dt_inicio_sintomas, dt_coleta_teste - dt_notificacao, NA),
         fl_testado = ifelse(!is.na(dt_teste_result), 1, 0),
         tp_teste = ifelse(fl_testado != 1,
                           NA,
                           ifelse(teste_tipo_ficha == 'RT-PCR' |
                                    (is.na(teste_tipo_ficha) & teste_tipo_prontuario == 'PCR'),
                                  'PCR', 'TR')),
         t_coleta_resultado = ifelse(tp_teste == 'PCR' & dt_coleta_teste >= dt_inicio_sintomas, dt_teste_result - dt_coleta_teste, NA),
         t_sint_coleta = ifelse(tp_teste == 'PCR' & dt_coleta_teste >= dt_inicio_sintomas, dt_coleta_teste - dt_inicio_sintomas, NA),
         fl_encerrado = ifelse(fl_status == "Concluído", 1, 0),
         fl_monitorado = ifelse(substr(mon_d3, 1, 4) != 'Sem ' &
                                  substr(mon_d7, 1, 4) != 'Sem ' &
                                  substr(mon_d10, 1, 4) != 'Sem ' &
                                  substr(mon_d14, 1, 4) != 'Sem ',
                                1, 0),
         sem_epid = epiweek(dt_notificacao),
         fl_pcr_oportuno = ifelse(t_sint_not <= 7,
                                  ifelse(tp_teste == 'PCR', 1, 0),
                                  NA),
         fl_confirmado = ifelse(class_conf_lab == 'SIM' |
                                  class_conf_clinep == 'SIM' |
                                  teste_result_ficha == 'SIM', 1, 0))


gd_t_sint_not <- not_data %>%
  filter(dt_notificacao >= as.Date('2020-01-01') & dt_notificacao < today() &
           dt_inicio_sintomas >= as.Date('2020-01-01') & dt_inicio_sintomas < today() &
           dt_coleta_teste >= as.Date('2020-01-01') & dt_coleta_teste < today() &
           dt_teste_result >= as.Date('2020-01-01') & dt_teste_result < today()) %>%
  group_by(dt_notificacao) %>%
  summarise(q20 = quantile(t_sint_not, probs = c(0.20), na.rm = T),
            median = median(t_sint_not, na.rm = T),
            q80 = quantile(t_sint_not, probs = c(0.80), na.rm = T))

gd_t_sint_not$q80_smooth <- predict(loess(gd_t_sint_not$q80 ~ as.numeric(gd_t_sint_not$dt_notificacao))) # Percentil 80 suavizado do tempo entre início dos sintomas e notificação

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
expostos$EXPOSTOS <- roll_sum(covid$MEDIANA_CASOS,2, fill = 0, align = "right") #menos de 3 dias do início dos sintomas
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
source("nowcasting/00_conexoes_com_a_base_obitos_srag.R")
obitos_raw <- obitos
obitos <- data.frame(DATA = obitos$DT_OBITO) %>% na.omit()
obitos$OBITOS <- 1
obitos <- obitos %>% group_by(DATA) %>% summarise(OBITOS = sum(OBITOS, na.rm = T))
obitos$DATA <- as.Date(obitos$DATA)

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
base <- na.omit(base)
base$CUM_RECUPERADOS <- cumsum(base$RECUPERADOS)
base$CUM_OBITOS <- cumsum(base$OBITOS)

#################################################
#Estimativa do número de suscetíveis
#################################################
POP <- 500973
base$SUSCETIVEIS <- POP - base$CUM_RECUPERADOS - base$CUM_OBITOS - base$EXPOSTOS - base$INFECTANTES

#################################################
#Estimativa de SRAG por COVID-19
#################################################
srag$INTERNACAO_UTI <- 1
srag <- subset(srag, srag$CLASSI_FIN == 5) #selecionando srag por covid
srag <- subset(srag, srag$CO_MUN_RES == 420540) #selecionando srag por covid
srag <- merge(srag, obitos_raw, by.x = "NM_PACIENT", by.y = "NOME", all = T)
srag <- as.data.frame(srag)

#Análise do tempo total de internação em uti
tempo_uti <- subset(srag, !is.na(srag$DT_ENTUTI))#Usando dados de pacientes que já tiveram alta da uti
tempo_uti <- tempo_uti %>% dplyr::select(DT_ENTUTI, DT_SAIDUTI,INTERNACAO_UTI)
tempo_uti[is.na(tempo_uti$DT_SAIDUTI), names(tempo_uti) == "DT_SAIDUTI"] <- Sys.Date()-1
tempo_uti <- as.Date(tempo_uti$DT_SAIDUTI, format = "%d/%m/%Y")  - as.Date(tempo_uti$DT_ENTUTI, format = "%d/%m/%Y")
hist(as.numeric(tempo_uti))
uti.dur.median <- median(tempo_uti) %>% as.numeric()

#Análise do tempo do início do contágio à  internação em uti
tempo_sintoma_uti <- median(as.Date(srag$DT_ENTUTI) - as.Date(srag$DT_SIN_PRI), na.rm = T)  %>% as.numeric()
tempo_contagio_uti <- tempo_sintoma_uti + 5 #Os sitomas aparecem, em média, 5 dias após o contágio

#Análise do tempo da internação em uti ao óbito
tempo_uti_obito <- subset(srag, !is.na(srag$DT_OBITO))
tempo_uti_obito <- median(as.Date(tempo_uti_obito$DT_OBITO) - as.Date(tempo_uti_obito$DT_ENTUTI), na.rm = T)  %>% as.numeric()

#Análise do tempo da internação em uti à recuperação
tempo_uti_recuperacao <- subset(srag, is.na(srag$DT_OBITO))
tempo_uti_recuperacao <- median(as.Date(tempo_uti_recuperacao$DT_SAIDUTI) - as.Date(tempo_uti_recuperacao$DT_ENTUTI), na.rm = T) %>% as.numeric()

tempos <- data.frame(uti.dur.median = uti.dur.median,
                     tempo_contagio_uti = tempo_contagio_uti,
                     tempo_uti_obito = tempo_uti_obito,
                     tempo_uti_recuperacao = tempo_uti_recuperacao)
write.csv(tempos, "nowcasting/dados/tempos.csv", row.names = F)


#Cálculo da ocupacão de leitos de uti
srag <- srag %>% dplyr::select(DT_ENTUTI, DT_SAIDUTI,INTERNACAO_UTI)
srag$DT_ENTUTI <- as.Date(srag$DT_ENTUTI, format = "%d/%m/%Y")
srag$DT_SAIDUTI <- as.Date(srag$DT_SAIDUTI)
srag <- srag[!is.na(srag$DT_ENTUTI),]
srag[is.na(srag$DT_SAIDUTI), names(srag) == "DT_SAIDUTI"] <- (Sys.Date()-1)
LEITOS_UTI <- list()
for(i in 1:nrow(srag)){
  LEITOS_UTI[[i]] <- seq.Date(from = srag$DT_ENTUTI[i], to = srag$DT_SAIDUTI[i],by = 1)
}
LEITOS_UTI <- as.Date(unlist(LEITOS_UTI), origin = "1970-01-01")
leitos_uti <- data.frame(DATA = LEITOS_UTI,
                         LEITOS_UTI = 1)
leitos_uti <- leitos_uti %>%
  group_by(DATA) %>%
  summarise(LEITOS_UTI = sum(LEITOS_UTI, na.rm = T))

srag <- srag %>%
  group_by(DT_ENTUTI) %>%
  summarise(INTERNACAO_UTI = sum(INTERNACAO_UTI, na.rm = T))
#merge dos dados de ocupação de leitos e número de intenação com os outros dados
base <- merge(base, leitos_uti, by = "DATA", all = T)
base <- merge(base, srag, by.x = "DATA", by.y = "DT_ENTUTI", all = T)
base[is.na(base$LEITOS_UTI), names(base) == "LEITOS_UTI"] <- 0
base[is.na(base$INTERNACAO_UTI), names(base) == "INTERNACAO_UTI"] <- 0
base$CUM_INTERNACAO_UTI <- cumsum(base$INTERNACAO_UTI)


write.csv(base, "nowcasting/dados/base_nowcasting.csv", row.names = F)

ggplot(base, aes(DATA, LEITOS_UTI))+
  geom_line()

# Estimando o Rt ----------------------------------------------------------
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
Rmodcovid = apeEstim(Icovid, gencovid, Lcovid, Rprior, a, trunc, "covid")
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
res_base_5dias <- subset(res_base, res_base$DATA > Sys.Date() -6)
write_sheet(id_covid,"reff", data = res_base_5dias)
write.csv(res_base_5dias, "nowcasting/dados/res_base_5dias.csv", row.names = F)


ggplot(res_melt, aes(DATA, value, group = variable, color = variable))+
  geom_line()+
  geom_hline(yintercept = 1) +
  theme_bw()



# Forecast do número de casos e dos óbitos --------------------------------
base$TOTAL <- base$EXPOSTOS + base$INFECTANTES + base$CUM_RECUPERADOS + base$CUM_OBITOS
base$CUM_CASOS_NOVOS <- cumsum(base$CASOS_NOVOS)
tx_hosp <- tail(base$CUM_INTERNACAO_UTI,1)[1]/tail(base$TOTAL,1) #Taxa de letalidade
tx_let <- tail(base$CUM_OBITOS,1)[1]/tail(base$CUM_INTERNACAO_UTI,1)[1] #Taxa de letalidade entre os internados em UTI


#Estados Iniciais
#Estados iniciais
##Suscetiveis = s.num = População - Expostos - Infectados - Recuperados - Óbitos
##Expostos (não transmissores) = e.num = Estimado do nowcasting
##Infectados (transmissores) = i.num = Estimado do nowcasting
##Recuperados = r.num = Estimado do nowcasting
##Óbitos = d.num = Estimado do nowcasting
S <- tail(base$SUSCETIVEIS,1)[1]
E <- tail(base$EXPOSTOS,1)[1]
I <- tail(base$INFECTANTES,1)[1]
H <- tail(base$LEITOS_UTI,1)[1]
R <- tail(base$CUM_RECUPERADOS,1)[1]
D <- tail(base$CUM_OBITOS,1)[1]
N <- S + E + I +H +  R + D
ir.dur <- 11
ei.dur <- 2
ih.dur <- tempo_contagio_uti
hd.dur <- tempo_uti_obito
hr.dur <- tempo_uti_recuperacao
prob1 <- tx_hosp
prob2 <- tx_let
etha <- 1/ir.dur
betha <- 1/ei.dur
delta <- 1/ih.dur
mu <- 1/hd.dur
epsilon <- 1/hr.dur

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

#Parâmetros
##Número de reprodução efetivo = Rt = Estimado do nowcasting
##Duração da exposição (não transmissível) = e.dur = 3 dias
##Duração da infecção (trainsmissível) = i.dur = 11 dias
##Letalidade = cfr
param <- param.dcm(Rt = c(tail(res_base$IC025,1),
                          tail(res_base$MEDIA,1),
                          tail(res_base$IC975,1)),
                   etha = 1/ir.dur,
                   betha = 1/ei.dur,
                   delta = 1/ih.dur,
                   mu = 1/hd.dur,
                   epsilon = 1/hr.dur,
                   prob1 = prob1,
                   prob2 = prob2
)


#Função SEIRD
SEIHRD <- function(t, t0, parms) {
  with(as.list(c(t0, parms)), {
    
    N <- S + E + I + H +  R + D
    
    
    EC <- Rt * etha
    alpha <- EC * I/N
    
    
    #Equações diferenciais
    dS <- -alpha*S
    dE <- alpha*S - betha*E
    dI <- betha*E - (prob1*delta + (etha*(1-prob1)*I))
    dH <- prob1*delta*I - (1-prob2)*epsilon*H - prob2*mu*H
    dR <- (1 - prob1)*etha*I + (1 - prob2)*epsilon*H
    dD <- prob2*mu*H
    
    #Outputs
    list(c(dS, dE, dI, dH, dR, dD,
           se.flow = alpha * S,
           ei.flow = betha * E,
           ir.flow = (1 - prob1)*etha*I,
           ih.flow = prob1*delta*I,
           hr.flow = (1-prob1)*etha*I,
           hd.flow = prob2*mu*H),
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
projecao <- 90
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

resultados_cenario_1$DATA <- c((Sys.Date()):(Sys.Date()+projecao-1))
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

resultados_cenario_2$DATA <- c((Sys.Date()):(Sys.Date()+projecao-1))
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

resultados_cenario_3$DATA <- c((Sys.Date()):(Sys.Date()+projecao-1))
resultados_cenario_3$DATA  <- as.Date(resultados_cenario_3$DATA , origin = "1970-01-01")
base_select <- base %>% dplyr::select(DATA,SUSCETIVEIS, CUM_RECUPERADOS, EXPOSTOS, INFECTANTES, CUM_OBITOS, LEITOS_UTI)
resultados_cenario_3 <- rbind(base_select, resultados_cenario_3)
names(resultados_cenario_3) <-c("DATA", "SUSCETIVEIS_CENARIO_3", "CUM_RECUPERADOS_CENARIO_3", "EXPOSTOS_CENARIO_3", "INFECTANTES_CENARIO_3", "CUM_OBITOS_CENARIO_3", "LEITOS_UTI_CENARIO_3")

#Unindo as bases de resultados
resultados <- merge(resultados_cenario_1, resultados_cenario_2, by= "DATA", all = T)
resultados <- merge(resultados_cenario_3, resultados, by= "DATA", all = T)


write.csv(resultados, "nowcasting/dados/resulados.csv", row.names = F)
resultados$DATA <- as.Date(resultados$DATA, origin = "1970-01-01")
write_sheet(id_covid,"resultados", data = resultados)


#Análise dos resultados

resultados_melt <- resultados %>% dplyr::select(DATA, CUM_OBITOS_CENARIO_2, LEITOS_UTI_CENARIO_2)
resultados_melt <- melt(resultados_melt, id.vars = "DATA")

ggplot(resultados_melt, aes(DATA, value, color = variable))+
  geom_line()+
  theme_bw()


