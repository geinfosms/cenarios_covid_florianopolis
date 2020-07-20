# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)

# Pacotes -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(RPostgreSQL)

source("nowcasting/00-conexoes_com_a_base_celk.R")


# Tempo do inicio de sintomas até liberacao de resultados
tempo <- covid
tempo$INIC_NOT <- as.Date(tempo$`Data da notificação`, format = "%d/%m/%Y") - as.Date(tempo$`Data do início dos sintomas`, format = "%d/%m/%Y")
tempo$NOT_RESULT <- as.Date(tempo$`Data da liberação do resultado`, format = "%d/%m/%Y") - as.Date(tempo$`Data da notificação`, format = "%d/%m/%Y")
tempo <- subset(tempo, tempo$INIC_NOT >= 0)
tempo <- subset(tempo, tempo$NOT_RESULT >= 0)
median(tempo$INIC_NOT, na.rm = T)
median(tempo$NOT_RESULT, na.rm = T)



# Transformando base ------------------------------------------------------
# Criando ID
covid <- mutate(covid, ID = rownames(covid))

#Selecionando as variáveis
covid <- covid %>%
	dplyr::select(ID,
		`UF de notificação`,                                         
		`Município de notificação`,
		`Estrangeiro`,
		`É profissional de saúde`,
		#`CBO`,
		`Data de nascimento`,
		#`País de origem`,
		`Sexo`,
		`UF de residência`,
		`Município de residência`,
		`Bairro`,
		`Data da notificação`,
		`Sintomas - Dor de garganta`,
		`Sintomas - Dispneia`,
		`Sintomas - Febre`,
		`Sintomas - Tosse`,
		#`Sintomas - outros`,
		`Data do início dos sintomas`,
		#`Tipo do teste - teste rápido - anticorpo`,
		#`Tipo do teste - teste rápido - antígeno`,
		#`Tipo do teste - RT-PCR`,
		`Resultado do teste`, #Resultado na ficha de investigação
		`Tipo de exame - ficha de investigação`,
		`Resultado do exame - prontuário`,
		`Tipo de exame - prontuário`,
		`Unidade de referência`, 
		`Equipe de referência`,
		`Raça` )

covid$`Tipo de exame` <- ifelse(is.na(covid$`Tipo de exame - ficha de investigação`),
				covid$`Tipo de exame - prontuário`,
				covid$`Tipo de exame - ficha de investigação`)

covid$`Resultado do teste` <- ifelse(is.na(covid$`Resultado do teste`),
				covid$`Resultado do exame - prontuário`,
				covid$`Resultado do teste`)

covid$`Tipo de exame - ficha de investigação` <- NULL
covid$`Tipo de exame - prontuário` <- NULL
covid$`Resultado do exame - prontuário` <- NULL

## Ajustando data do inicio dos sintomas, tirando as erradas e substituindo essas e as missing pela data de notificacao
covid$`Data da notificação` <- as.Date(covid$`Data da notificação`, format = "%d/%m/%Y")
covid$`Data do início dos sintomas` <- as.Date(covid$`Data do início dos sintomas`, format = "%d/%m/%Y")
covid$`Data do início dos sintomas` <- ifelse(!is.na(covid$`Data do início dos sintomas`), 
					      covid$`Data do início dos sintomas`, 
					      covid$`Data da notificação`)
covid$`Data do início dos sintomas` <- ifelse(covid$`Data do início dos sintomas` > as.Date("2020-02-01") &
					      covid$`Data do início dos sintomas` <= Sys.Date(),
					      covid$`Data do início dos sintomas`, 
					      covid$`Data da notificação`)

covid$`Data do início dos sintomas` <- as.Date(covid$`Data do início dos sintomas`, origin = "1970-01-01")

## Criando a variável `idade` para retirar a data de nascimento
covid$`Data de nascimento` <- as.Date(covid$`Data de nascimento`, format = "%d/%m/%Y")
covid$IDADE <- round((covid$`Data do início dos sintomas` - covid$`Data de nascimento`)/365,0)
covid$`Data de nascimento` <- NULL
## Anonimizando país, pois são poucos os casos
# covid$`País de origem`<- ifelse(covid$`País de origem` == `brasil`, `brasil`,
#        				ifelse(is.na(covid$`País de origem`), ``, `estrangeiro`))


covid <- subset(covid, covid$`Data da notificação` >= as.Date("2020-02-01"))
covid <- subset(covid, covid$`Data da notificação` <= (Sys.Date()-1))


# Exportando base ---------------------------------------------------------
write.csv(covid, "nowcasting/dados/covid_anonimizado.csv", row.names = F,fileEncoding = "UTF-8")


