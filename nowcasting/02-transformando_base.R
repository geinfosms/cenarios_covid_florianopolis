# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)

# Pacotes -----------------------------------------------------------------
library(readr)
library(readxl)
library(tidyverse)
library(reshape2)
library(purrr)
#devtools::install_github("tidyverse/googlesheets4")
library(googlesheets4)
library(tigerstats)

# Funcões -----------------------------------------------------------------
## Ler as tabelas dentro da planilha de excel
read_excel_allsheets <- function(filename, tibble = FALSE) {
    sheets <- readxl::excel_sheets(filename)
    x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
    if(!tibble) x <- lapply(x, as.data.frame)
    names(x) <- sheets
    x
}

#Retirar acentos
rm_accent <- function(str,pattern="all") {
	  # Rotinas e funções úteis V 1.0
	  # rm.accent - REMOVE ACENTOS DE PALAVRAS
	  # Função que tira todos os acentos e pontuações de um vetor de strings.
	  # Parâmetros:
	  # str - vetor de strings que terão seus acentos retirados.
	  # patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
	  #            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
	  #            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
	  #            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "`", "^", "~", "¨", "ç")
	  if(!is.character(str))
	    str <- as.character(str)
	
	  pattern <- unique(pattern)
	
	  if(any(pattern=="Ç"))
	    pattern[pattern=="Ç"] <- "ç"
	
	  symbols <- c(
	    acute = "áéíóúÁÉÍÓÚýÝ",
	    grave = "àèìòùÀÈÌÒÙ",
	    circunflex = "âêîôûÂÊÎÔÛ",
	    tilde = "ãõÃÕñÑ",
	    umlaut = "äëïöüÄËÏÖÜÿ",
	    cedil = "çÇ"
	  )
	
	  nudeSymbols <- c(
	    acute = "aeiouAEIOUyY",
	    grave = "aeiouAEIOU",
	    circunflex = "aeiouAEIOU",
	    tilde = "aoAOnN",
	    umlaut = "aeiouAEIOUy",
	    cedil = "cC"
	  )
	
	  accentTypes <- c("´","`","^","~","¨","ç")
	
	  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
	    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
	
	  for(i in which(accentTypes%in%pattern))
	    str <- chartr(symbols[i],nudeSymbols[i], str)
	
	  return(str)
}

## Ajutar títulos das bases
ajustar_nomes <- function(x){
	 x%>%
	  stringr::str_trim() %>%                        #Remove espaços em branco sobrando
	  stringr::str_to_lower() %>%                    #Converte todas as strings para minusculo
	  rm_accent() %>%                                #Remove os acentos com a funcao criada acima
	  stringr::str_replace_all("[/' '.()]", "_") %>% #Substitui os caracteres especiais por "_"
	  stringr::str_replace_all("_+", "_") %>%        #Substitui os caracteres especiais por "_"   
	  stringr::str_replace_all("-", "_") %>%        #Substitui os caracteres especiais por "_"   
	  stringr::str_replace("_$", "")                 #Substitui o caracter especiais por "_"
	}


# Importanto bases --------------------------------------------------------
## Dados demográficos
populacao <- read_excel_allsheets("nowcasting/dados/demografia/Estima_Pop_Genero_2000_a_2023/Estima_Pop_Genero_2000_a_2023_ULS.xlsx")
populacao <- map(populacao,`[`,"44075")#extraindo coluna de 2020
titulos_pop <- c("populacao", "homens", "mulheres")
populacao <- do.call(cbind,populacao)
names(populacao) <- titulos_pop

escolaridade <- read_excel_allsheets("nowcasting/dados/demografia/Estima_Escolaridade_2000_a_2023/Estima_Escolaridade_Pessoas_2000_2030_Unidade_Saude.xlsx")
escolaridade <- map(escolaridade,`[`,"44075")#extraindo coluna de 2020
titulos_esc <- names(escolaridade)
titulos_esc <- paste("escolaridade", titulos_esc)
titulos_esc <- ajustar_nomes(titulos_esc)
escolaridade <- do.call(cbind,escolaridade)
names(escolaridade) <- titulos_esc

cor_pele <- read_excel_allsheets("nowcasting/dados/demografia/Estima_Raça_Cor_2000_a_2023/Eatima_Raca_Cor_2000_a_2023_ULS.xlsx")
cor_pele <- map(cor_pele,`[`,"44075")#extraindo coluna de 2020
titulos_cp <- names(cor_pele)
titulos_cp <- sub(x = titulos_cp,pattern = "-",replacement = " ")
titulos_cp <- sub(x = titulos_cp,pattern = " ",replacement = "")
titulos_cp <- sub(x = titulos_cp,pattern = " ",replacement = "")
titulos_cp <-  paste("cor_pele", titulos_cp)
titulos_cp <- ajustar_nomes(titulos_cp)
cor_pele <- do.call(cbind,cor_pele)
names(cor_pele) <- titulos_cp
cor_pele <- cor_pele[-54,] #retirando o total

renda <- read_excel_allsheets("nowcasting/dados/demografia/Estima_Renda_2000_a_2023/Estima_Renda_ULS.xlsx")
renda <- map(renda,`[`,"44075")#extraindo coluna de 2020
titulos_renda <- names(renda)
titulos_renda <- ajustar_nomes(titulos_renda)
renda <- do.call(cbind,renda)
names(renda) <- titulos_renda
renda <- renda[-54,] #retirando o total

demografia <- cbind(populacao,escolaridade, cor_pele, renda)
demografia <- demografia[-c(1:4),]#retirando linhas sem informação

idade <- read_excel_allsheets("nowcasting/dados/demografia/Estima_Idade_2000_a_2023/Estima_Idade_0_a_100_ou_mais_de_2000_a_2030_ULS.xlsx")
nome_cs <- map(idade,`[`,5)#extraindo o nome dos cs
nome_cs <- do.call(cbind,nome_cs)
nome_cs <- names(nome_cs)
nome_cs <- ajustar_nomes(nome_cs)
nome_fe <- map(idade,`[`,4)#extraindo o nome das faixas estárias
nome_fe <- unlist(nome_fe[1]) %>% as.data.frame()
nome_fe <- nome_fe[-c(1:3),]
nome_fe <- as.character(nome_fe)
nome_fe <- paste0("idade_", nome_fe)
nome_fe[which(is.na(nome_fe))] <- "Total"
nome_fe <- ajustar_nomes(nome_fe)
idade <- map(idade,`[`,"...25")#extraindo coluna de 2020
idade <- do.call(cbind,idade)
names(idade) <- nome_cs
idade <- idade[-c(1:3),]
idade <- idade %>% t() %>% as.data.frame()
names(idade) <- nome_fe
idade$total <- NULL

demografia <- cbind(idade, demografia)
demografia$territorio <- row.names(demografia)
## Dados de casos suspeitos
source("nowcasting/01-anonimizando_base.R")
#covid <- read_csv("nowcasting/dados/covid_anonimizado.csv")

# Transformando base ------------------------------------------------------

# Convertendo, ajustando e criando variáveis
covid <- sapply(covid, as.character) %>% as.data.frame()
covid <- sapply(covid, tolower) %>% as.data.frame()
covid <- sapply(covid, rm_accent) %>% as.data.frame()


## Ajustando a variável Bairro para corrigir erros causasdos por caracteres especiais ou falha na digitação.
## Os bairros foram aproximados para coincidirem com as áreas dos Centros de Saúde
## Bairros de outros municípios foram categorizados como outros
covid$Bairro <- as.character(covid$Bairro)

covid[which(covid$Bairro == "alto aririu"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "aracatuba"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "areias"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "aririu da formiga"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "barra do aririu"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "barra do sambaqui"),names(covid) == "Bairro"]<- "santo antonio de lisboa"
covid[which(covid$Bairro == "barreiros"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "bela vista"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "bom abrigo"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "bom jesus"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "caeira do saco dos limoes"),names(covid) == "Bairro"]<- "saco dos limoes"
covid[which(covid$Bairro == "caminho novo"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "campinas"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "canto (continente)"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "capao"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "carvoeira"),names(covid) == "Bairro"]<- "pantanal"
covid[which(covid$Bairro == "colonia santana"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "costa de dentro"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "da praca"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "do aririu"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "estacio"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "estacio"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "fazenda da armacao"),names(covid) == "Bairro"]<- "armacao"
covid[which(covid$Bairro == "estacio"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "fazenda santo antonio"),names(covid) == "Bairro"]<- "santo antonio de lisboa"
covid[which(covid$Bairro == "florquilinhas"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "forquilhas"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "forquilhinha"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "fundos"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "ipiranga"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "itaguacu"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "itoupava central"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "jardim acalipto"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "jardim cidade de florianopolis"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "jardim cidade florianopolis"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "jardim janaina"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "jose mendes"),names(covid) == "Bairro"]<- "prainha"
covid[which(covid$Bairro == "jurere internacional"),names(covid) == "Bairro"]<- "jurere"
covid[which(covid$Bairro == "kobrasol"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "madri"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "monte verde"),names(covid) == "Bairro"]<- "saco grande"
covid[which(covid$Bairro == "nao identificado"),names(covid) == "Bairro"]<- NA
covid[which(covid$Bairro == "nao informado"),names(covid) == "Bairro"]<- NA
covid[which(covid$Bairro == "nossa senhora do rosario"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "nova palhoca"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "passa vinte"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "pagani"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "pedra branca"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "ponte do imaruim"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "prado de baixo"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "praia brava"),names(covid) == "Bairro"]<- "ponta das canas"
covid[which(covid$Bairro == "praia joao rosa"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "real parque"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "rio caveiras"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "rocado"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "santa monica"),names(covid) == "Bairro"]<- "corrego grande"
covid[which(covid$Bairro == "sao joao do rio vermelho"),names(covid) == "Bairro"]<- "rio vermelho"
covid[which(covid$Bairro == "sao luiz"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "sao sebastiao"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "sao vicente"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "serraria"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "silveira"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "umarizal"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "vargem do bom jesus"),names(covid) == "Bairro"]<- "canasvieiras"
covid[which(covid$Bairro == "varginha"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "vila suzana"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "gracas"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "aririu"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "bom viver"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "caeira da barra do sul"),names(covid) == "Bairro"]<- "caieira da barra do sul"
covid[which(covid$Bairro == "forquilhinhas"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "guarda do cubatao"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "itaquera"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "parque albina"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "rio pequeno"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "sem denominacao"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "vila sao geraldo"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "alto iriru"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "altos de potecas"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "area rural de biguacu"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "areias do meio"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "azenha"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "barra do aririu"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "bela vista iii"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "bela vita"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "bella vista"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "bigorrilho"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "bisenello"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "boa vista"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "bosque das mansoes"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "agua verde"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "br 101"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "brajaru"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "bucarein"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "caieira do norte"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "calehrios"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "campina"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "campo"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "campo duna"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "campos novos"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "cantor"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "cecilia"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "ceniro martins"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "cenrto"),names(covid) == "Bairro"]<- "centro"
covid[which(covid$Bairro == "centro i"),names(covid) == "Bairro"]<- "centro"
covid[which(covid$Bairro == "condominio panorama"),names(covid) == "Bairro"]<- "monte cristo"
covid[which(covid$Bairro == "enseada do brito"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "cs enseada do brito ens brito"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "erondina stefanes iachitzki"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "fazenda sagramento ii"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "fazendo de sto antonio"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "ferrugem"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "flor de napoles"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "flor de napolis"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "floresta"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "foquilhinhas"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "funddos"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "ganchos do meio"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "garo"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "geral"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "guarita"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "guiomar dentro"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "janaina"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "jardim angelica"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "jardim eldorado"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "jardim marco zero"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "jardim paulista"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "jardim santiago"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "joaia"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "machados"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "morro da bina"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "municipios"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "n a"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "nacoes"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "nossa sra do rosario"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "orvalio"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "pacheco"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "pachecos"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "passo da areia"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "picadas do norte"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "picadas do sul"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "pinheira ens brito"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "pioneiro"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "poesa vinte"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "ponta de baixo"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "ponta russa"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "popular"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "potecas"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "prado"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "praia"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "praia comprida"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "praia de fora"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "praia do mar aberto   pinheira"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "praia redonda"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "quinze de novembro"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "real park"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "rio grande"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "santa filomena"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "sao domingos"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "sao jose"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "sao luis"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "saudade"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "saveiro"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "serrariar"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "sertao do maruim"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "setor habitacional vicente pires"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "setor residencial oeste sao sebastiao"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "sntos dumont"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "sorocaba do sul"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "tijuquinhas"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "tijuquinhas guaporanga"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "universitario"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "vendaval"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "barra  do aririu"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "enseada do brito (ens brito)"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "n/a"),names(covid) == "Bairro"]<- NA
covid[which(covid$Bairro == "nossa sra. do rosario"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "pinheira (ens brito)"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "praia do mar aberto - pinheira"),names(covid) == "Bairro"]<- "outro"
covid[which(covid$Bairro == "setor residencial oeste (sao sebastiao)"),names(covid) == "Bairro"]<- "outro"


covid$Bairro <- ifelse(is.na(covid$Bairro) | covid$Bairro == "outro", covid$Bairro, paste0("cs ",covid$Bairro))
covid$Bairro <- ajustar_nomes(covid$Bairro)
sort(unique(covid$Bairro))


sort(unique(covid$`Unidade de referência`))

##Convertendo idade em categoria
covid$IDADE <- as.numeric(as.character(covid$IDADE)) 
covid[which(covid$IDADE < 0.001), colnames(covid) == "IDADE"] <- NA #Excluindo dados de idade incorretos
covid$FAIXA_ETARIA <- ifelse(covid$IDADE < 10, "10 menos", 
			    ifelse(covid$IDADE < 20, "10 a 20",
			           ifelse(covid$IDADE < 40, "20 a 40",
			                  ifelse(covid$IDADE < 60, "40 a 60",
			                         ifelse(covid$IDADE < 80, "60 a 80", "80 mais")
			                         )
			                  )
			           )
			)

## Criando a variável Triagem. O primeiro modelo de triagem para classificação de casos suspeitos para COVID-19 
## em Florianópolis foi utilizado até 2020-03-24, 
## o segundo modelo até 2020-04-06 e o terceiro a partir de então.
covid$`Data do início dos sintomas` <- as.Date(covid$`Data do início dos sintomas`, format = "%Y-%m-%d")
covid$TRIAGEM <- ifelse(covid$`Data do início dos sintomas` < as.Date("2020-03-24", format = "%Y-%m-%d"), "modelo 1",
			ifelse(covid$`Data do início dos sintomas` < as.Date("2020-04-06", format = "%Y-%m-%d"), "modelo 2", "modelo 3"))

## Transformando outros municípios na categoria "outros", pois o número é pequeno
covid$`Município de residência` <- as.character(covid$`Município de residência`)
covid$`Município de residência` <- ifelse(covid$`Município de residência` == "florianopolis", "florianopolis", "outro")


## Mantendo apenas resultados "confirmado" ou "descartado" ou "não detectado" (essas duas últimas categorias são a mesma coisa). 
## Os pacientes aguardando resultado ou com resultado inconclusivo foram classificados como missing
covid$`Resultado do teste` <- ifelse(covid$`Resultado do teste` == "positivo", "confirmado",
			  		ifelse(covid$`Resultado do teste` == "negativo", "descartado",
			  	       		"missing"))

## Ajustando a variável território (Áreas dos centros de saúde) e subterritório (áreas das equipes de saúde da família)
## Florianópolis possui projeções demográficos (população, escolaridade, renda, sexo, idade), por território.
## Quando o dado de CS de referência não estava disponível, utilizou-se o endereço para se inferir o terrotório
covid <- covid %>% rename(Territorio = `Unidade de referência`,
			 Subterritorio = `Equipe de referência`)
covid$Territorio <- ajustar_nomes(covid$Territorio)
covid$Territorio <- ifelse(is.na(covid$Territorio), covid$Bairro, covid$Territorio)
covid$Bairro <- NULL
sort(unique(covid$Territorio))

## Construindo variável Quantidade de Infectados (pessoas com até 14 dias de início de sintomas) por território
covid$`Data do início dos sintomas` <- as.Date(covid$`Data do início dos sintomas`, format = "%Y-%m-%d", origin = "1970-01-01")
infec <- covid
infec <- subset(infec, infec$`Resultado do teste` == "confirmado")
infec$INFECTADO <-infec$`Data do início dos sintomas` 
INFECTADO <- table(infec$Territorio, infec$INFECTADO)
infec$INFECTADO_1 <-infec$`Data do início dos sintomas` + 1
INFECTADO_1 <- table(infec$Territorio, infec$INFECTADO_1)
infec$INFECTADO_2 <-infec$`Data do início dos sintomas` + 2
INFECTADO_2 <- table(infec$Territorio, infec$INFECTADO_2)
infec$INFECTADO_3 <-infec$`Data do início dos sintomas` + 3
INFECTADO_3 <- table(infec$Territorio, infec$INFECTADO_3)
infec$INFECTADO_4 <-infec$`Data do início dos sintomas` + 4
INFECTADO_4 <- table(infec$Territorio, infec$INFECTADO_4)
infec$INFECTADO_5 <-infec$`Data do início dos sintomas` + 5
INFECTADO_5 <- table(infec$Territorio, infec$INFECTADO_5)
infec$INFECTADO_6 <-infec$`Data do início dos sintomas` + 6
INFECTADO_6 <- table(infec$Territorio, infec$INFECTADO_6)
infec$INFECTADO_7 <-infec$`Data do início dos sintomas` + 7
INFECTADO_7 <- table(infec$Territorio, infec$INFECTADO_7)
infec$INFECTADO_8 <-infec$`Data do início dos sintomas` + 8
INFECTADO_8 <- table(infec$Territorio, infec$INFECTADO_8)
infec$INFECTADO_9 <-infec$`Data do início dos sintomas` + 9
INFECTADO_9 <- table(infec$Territorio, infec$INFECTADO_9)
infec$INFECTADO_10 <-infec$`Data do início dos sintomas` + 10
INFECTADO_10 <- table(infec$Territorio, infec$INFECTADO_10)
infec$INFECTADO_11 <-infec$`Data do início dos sintomas` + 11
INFECTADO_11 <- table(infec$Territorio, infec$INFECTADO_11)
infec$INFECTADO_12 <-infec$`Data do início dos sintomas` + 12
INFECTADO_12 <- table(infec$Territorio, infec$INFECTADO_12)
infec$INFECTADO_13 <-infec$`Data do início dos sintomas` + 13
INFECTADO_13 <- table(infec$Territorio, infec$INFECTADO_13)
INFECTADO <- melt(INFECTADO)
INFECTADO_1 <- melt(INFECTADO_1)
INFECTADO_2 <- melt(INFECTADO_2)
INFECTADO_3 <- melt(INFECTADO_3)
INFECTADO_4 <- melt(INFECTADO_4)
INFECTADO_5 <- melt(INFECTADO_5)
INFECTADO_6 <- melt(INFECTADO_6)
INFECTADO_7 <- melt(INFECTADO_7)
INFECTADO_8 <- melt(INFECTADO_8)
INFECTADO_9 <- melt(INFECTADO_9)
INFECTADO_10 <- melt(INFECTADO_10)
INFECTADO_11 <- melt(INFECTADO_11)
INFECTADO_12 <- melt(INFECTADO_12)
INFECTADO_13 <- melt(INFECTADO_13)

infec <- Reduce(function(x,y) merge(x = x, y = y, by = c("Var1", "Var2")), 
       		list(INFECTADO,
			INFECTADO_1,
			INFECTADO_2,
			INFECTADO_3,
			INFECTADO_4,
			INFECTADO_5,
			INFECTADO_6,
			INFECTADO_7,
			INFECTADO_8,
			INFECTADO_9,
			INFECTADO_10,
			INFECTADO_11,
			INFECTADO_12,
			INFECTADO_13
)) %>% as.data.frame()

infec$INFECTADOS_TERRITORIO <- rowSums(infec[,c(3:16)], na.rm = T)
infec <- infec[,c(1,2,17)]
names(infec) <- c("Territorio", "Data do início dos sintomas", "INFECTADOS_TERRITORIO")
	
covid <- merge(covid, infec, by = c("Territorio", "Data do início dos sintomas"), all.x = T)

## Substituindo NA por "missing" nas variáveis que serão utilizadas como categógicas.
covid[,!(names(covid) %in% c("ID", "Data do início dos sintomas", "IDADE"))] <- sapply(covid[,!(names(covid) %in% c("ID", "Data do início dos sintomas", "Data de notificação", "IDADE"))], as.factor) %>% as.data.frame()
covid$Territorio <-fct_explicit_na(covid$Territorio, "missing")
covid$`UF de notificação` <-fct_explicit_na(covid$`UF de notificação`, "missing")
covid$`Município de notificação` <-fct_explicit_na(covid$`Município de notificação`, "missing")
covid$Estrangeiro <-fct_explicit_na(covid$Estrangeiro, "missing")
covid$`É profissional de saúde` <-fct_explicit_na(covid$`É profissional de saúde`, "missing")
covid$Sexo <-fct_explicit_na(covid$Sexo, "missing")
covid$`UF de residência` <-fct_explicit_na(covid$`UF de residência`, "missing")
covid$`Município de residência` <-fct_explicit_na(covid$`Município de residência`, "missing")
covid$`Sintomas - Dor de garganta` <-fct_explicit_na(covid$`Sintomas - Dor de garganta`, "missing")
covid$`Sintomas - Dispneia` <-fct_explicit_na(covid$`Sintomas - Dispneia`, "missing")
covid$`Sintomas - Febre` <-fct_explicit_na(covid$`Sintomas - Febre`, "missing")
covid$`Sintomas - Tosse` <-fct_explicit_na(covid$`Sintomas - Tosse`, "missing")
covid$`Resultado do teste` <-fct_explicit_na(covid$`Resultado do teste`, "missing")
covid$`Tipo de exame` <-fct_explicit_na(covid$`Tipo de exame`, "missing")
covid$`Subterritorio` <-fct_explicit_na(covid$`Subterritorio`, "missing")
covid$`Raça` <-fct_explicit_na(covid$`Raça`, "missing")
covid$`FAIXA_ETARIA` <-fct_explicit_na(covid$`FAIXA_ETARIA`, "missing")
covid$`TRIAGEM` <-fct_explicit_na(covid$`TRIAGEM`, "missing")
covid$`INFECTADOS_TERRITORIO` <-fct_explicit_na(covid$`INFECTADOS_TERRITORIO`, "missing")

covid$`Data da notificação` <- as.Date(covid$`Data da notificação`, format = "%Y-%m-%d")



# Merge das bases ---------------------------------------------------------
covid$Territorio <- as.character(covid$Territorio)
demografia$territorio <- as.character(demografia$territorio)
covid <- merge(covid,demografia, by.x = "Territorio",by.y = "territorio", all.y = T) #Retirando dados de outros municípios

## Calculando a taxa de infectados por território
covid$TX_INFECTADOS_TERRITORIO <- as.numeric(covid$INFECTADOS_TERRITORIO)/as.numeric(covid$populacao)*100000



## Ajustando nome da base
names(covid)[c(1:23,163)] <- c("TERRITORIO", 
			       "INICIO_SINTOMAS", 
			       "ID",
			       "UF_NOTIFICACAO",
			       "MUNICIPIO_NOTIFICACAO",
			       "ESTRANGEIRO",
			       "PROF_SAUDE",
			       "SEXO",
			       "UF_RESIDENCIA",
			       "MUNICIPIO_RESIDENCIA",
			       "DATA_NOTIFICACAO",
			       "DOR_GARGANTA",
			       "DISPINEIA",
			       "FEBRE",
			       "TOSSE",
			       "RESULTADO",
			       "SUBTERRITORIO",
			       "RACA_COR",
			       "TIPO_EXAME",
			       "IDADE",
			       "FAIXA_ETARIA",  
			       "TRIAGEM",  
			       "INFECTADOS_TERRITORIO", 
			       "TX_INFECTADOS_TERRITORIO")
summary(covid)


#UF e municipio de notificação e uf e municipio de residencia são praticamente só SC e Fpolis. Por isso, serão excluídos
covid$ESTRANGEIRO <- NULL
covid$UF_NOTIFICACAO <- NULL
covid$MUNICIPIO_NOTIFICACAO <- NULL
covid$UF_RESIDENCIA <- NULL
covid$MUNICIPIO_RESIDENCIA <- NULL


## Alguns algoritmos como o Random Forest não trabalham com variáveis com grande númer de categorias, por isso, selecionaram-se
## território com mais de 50 notificações e subterritórios com mais de 25 notificações, 
## os demais foram transformados em "outro"
# table_territorio <- table(covid$Territorio) %>% as.data.frame()
# table_territorio <- subset(table_territorio, table_territorio$Freq > 50)
# covid$Territorio <- as.character(covid$Territorio)
# covid$Territorio <- ifelse(covid$Territorio %in% table_territorio$Var1,covid$Territorio, "outro")
# covid$Territorio <- ajustar_nomes(covid$Territorio)
table_subterritorio <- table(covid$SUBTERRITORIO) %>% as.data.frame()
table_subterritorio <- subset(table_subterritorio, table_subterritorio$Freq > 25)
covid$SUBTERRITORIO <- as.character(covid$SUBTERRITORIO)
covid$SUBTERRITORIO <- ifelse(covid$SUBTERRITORIO %in% table_subterritorio$Var1,covid$SUBTERRITORIO, "outro")

covid$INFECTADOS_TERRITORIO <- as.numeric(covid$INFECTADOS_TERRITORIO)
covid$INFECTADOS_TERRITORIO <- if_else(is.na(covid$INFECTADOS_TERRITORIO),0,covid$INFECTADOS_TERRITORIO)
covid$TX_INFECTADOS_TERRITORIO <- as.numeric(covid$TX_INFECTADOS_TERRITORIO)
covid$TX_INFECTADOS_TERRITORIO <- if_else(is.na(covid$TX_INFECTADOS_TERRITORIO),0,covid$TX_INFECTADOS_TERRITORIO)


# Dados de trânsito -------------------------------------------------------
## Utilizou-se o deslocamento por carro como proxy de contato social
## O tráfego é mensurado em 4 avenidas da cidade. Utilizou-se a média destas 4 aveindas
## Trabalhou-se com a hipótese de que a chance de transimissão pode variar de acordo com o fluxo de carros em 
## diferentes períodos. Por isso, utilizaram-se a mobilidade 14 períodos (Periodo atual e 13 dias anteriores à data de primeiros sintomas)
# id_transito <- "1lfiVlAcIyMB2lBA3GXEwbN3qQJw0XdvoiNMJfxh1tPI"
# transito <- read_sheet(id_transito,"fluxo_automoveis",skip = 0,col_names = T) %>% as.data.frame()
# transito <- transito[,c(1,5)]
# transito <- na.omit(transito)
# names(transito) <- c("INICIO_SINTOMAS", "MEDIA_TRANSITO")
# transito$INICIO_SINTOMAS <- as.Date(transito$INICIO_SINTOMAS, format = "%d/%m/%Y")
# transito_lag1 <- transito
# transito_lag1$INICIO_SINTOMAS <- transito_lag1$INICIO_SINTOMAS + 1
# transito_lag2 <- transito
# transito_lag2$INICIO_SINTOMAS <- transito_lag2$INICIO_SINTOMAS + 2
# transito_lag3 <- transito
# transito_lag3$INICIO_SINTOMAS <- transito_lag3$INICIO_SINTOMAS + 3
# transito_lag4 <- transito
# transito_lag4$INICIO_SINTOMAS <- transito_lag4$INICIO_SINTOMAS + 4
# transito_lag5 <- transito
# transito_lag5$INICIO_SINTOMAS <- transito_lag5$INICIO_SINTOMAS + 5
# transito_lag6 <- transito
# transito_lag6$INICIO_SINTOMAS <- transito_lag6$INICIO_SINTOMAS + 6
# transito_lag7 <- transito
# transito_lag7$INICIO_SINTOMAS <- transito_lag7$INICIO_SINTOMAS + 7
# transito_lag8 <- transito
# transito_lag8$INICIO_SINTOMAS <- transito_lag8$INICIO_SINTOMAS + 8
# transito_lag9 <- transito
# transito_lag9$INICIO_SINTOMAS <- transito_lag9$INICIO_SINTOMAS + 9
# transito_lag10 <- transito
# transito_lag10$INICIO_SINTOMAS <- transito_lag10$INICIO_SINTOMAS + 10
# transito_lag11 <- transito
# transito_lag11$INICIO_SINTOMAS <- transito_lag11$INICIO_SINTOMAS + 11
# transito_lag12 <- transito
# transito_lag12$INICIO_SINTOMAS <- transito_lag12$INICIO_SINTOMAS + 12
# transito_lag13 <- transito
# transito_lag13$INICIO_SINTOMAS <- transito_lag13$INICIO_SINTOMAS + 13
# transito <- Reduce(function(x,y) merge(x = x, y = y, by = c("INICIO_SINTOMAS"), all = T),
#        		list(transito,
#        		     transito_lag1,
#        		     transito_lag2,
#        		     transito_lag3,
#        		     transito_lag4,
#        		     transito_lag5,
#        		     transito_lag6,
#        		     transito_lag7,
#        		     transito_lag8,
#        		     transito_lag9,
#        		     transito_lag10,
#        		     transito_lag11,
#        		     transito_lag12,
#        		     transito_lag13
# )) %>% as.data.frame()
# 
# 
# 
# names(transito) <- c("INICIO_SINTOMAS",
# 		     "MEDIA_TRANSITO",
# 		     "MEDIA_TRANSITO_LAG1",
# 		     "MEDIA_TRANSITO_LAG2",
# 		     "MEDIA_TRANSITO_LAG3",
# 		     "MEDIA_TRANSITO_LAG4",
# 		     "MEDIA_TRANSITO_LAG5",
# 		     "MEDIA_TRANSITO_LAG6",
# 		     "MEDIA_TRANSITO_LAG7",
# 		     "MEDIA_TRANSITO_LAG8",
# 		     "MEDIA_TRANSITO_LAG9",
# 		     "MEDIA_TRANSITO_LAG10",
# 		     "MEDIA_TRANSITO_LAG11",
# 		     "MEDIA_TRANSITO_LAG12",
# 		     "MEDIA_TRANSITO_LAG13")
# 
# covid <- merge(covid, transito, by = "INICIO_SINTOMAS", all = T)


## Extraindo dados missing que foram inseridos com a base de transito
covid <- na.omit(covid)

## Proporcao de masculino
covid$homens <- as.numeric(as.character(covid$homens))
covid$mulheres <- as.numeric(as.character(covid$mulheres))
covid$PROP_MASC <- covid$homens/covid$mulheres

## Percentual de pessoas com 60 anos ou mais
covid[,c(19:119)] <- apply(covid[,c(19:119)], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))
covid$PERC_60_MAIS <- rowSums(covid[,c(79:119)])/rowSums(covid[,c(19:112)])

covid$idade_na <- NULL

## Percentual de pessoas NÃO brancas
covid[,c(145:150)] <- apply(covid[,c(145:150)], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))
covid$PERC_NAO_BRANCA <- rowSums(covid[,c(146:150)])/rowSums(covid[,c(145:150)])

## Percentual de pessoas 10 anos ou menos de escolaridade
covid[,c(123:144)] <- apply(covid[,c(123:144)], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))
covid$PERC_ESC_10_MENOS <- rowSums(covid[,c(123:135)])/rowSums(covid[,c(123:144)])


# Transformando base ------------------------------------------------------
covid$ID <- as.factor(covid$ID)
covid$PROF_SAUDE <-  as.factor(covid$PROF_SAUDE)
covid$DOR_GARGANTA <- as.factor(covid$DOR_GARGANTA)
covid$DISPINEIA <- as.factor(covid$DISPINEIA)
covid$FEBRE <- as.factor(covid$FEBRE)
covid$TOSSE <- as.factor(covid$TOSSE)
covid$TIPO_EXAME <-as.factor(covid$TIPO_EXAME)
covid$RESULTADO <- as.factor(covid$RESULTADO)
covid$TERRITORIO <-as.factor(covid$TERRITORIO)
covid$SEXO <-as.factor(covid$SEXO)
covid$SUBTERRITORIO <-as.factor(covid$SUBTERRITORIO)
covid$TRIAGEM <-as.factor(covid$TRIAGEM)
covid$RACA_COR <-as.factor(covid$RACA_COR)
covid$FAIXA_ETARIA <-as.factor(covid$FAIXA_ETARIA)
covid$INICIO_SINTOMAS <-  as.numeric(covid$INICIO_SINTOMAS)#Transformando em número, pois o learner do mlr não trabalha com data
covid$DATA_NOTIFICACAO <- as.numeric(covid$DATA_NOTIFICACAO)#Transformando em número, pois o learner do mlr não trabalha com data
covid$renda_med_pess <- as.numeric(covid$renda_med_pess)
covid$renda_med_por_domic <- as.numeric(covid$renda_med_por_domic)
covid$renda_tot_resp <- as.numeric(covid$renda_tot_resp)
covid$renda_med_resp <- as.numeric(covid$renda_med_resp)
covid$renda_tot_pess <- as.numeric(covid$renda_tot_pess)
covid$renda_tot_domic <- as.numeric(covid$renda_tot_domic)

covid$populacao <- as.numeric(covid$populacao)



# Formação das bases de treino, teste e predição --------------------------
train_test_base <- subset(covid, covid$RESULTADO == "descartado" |
			  	covid$RESULTADO == "confirmado")
train_test_base$RESULTADO <- factor(train_test_base$RESULTADO, levels = c("confirmado", "descartado"))
summary(train_test_base)

#Base para predição
predic_base <- subset(covid, !(covid$RESULTADO %in% c("confirmado", "descartado")))
summary(predic_base)

#Analisando distribuição das variáveis entre a base de treino_teste e a base de predição
#Variáveis com muita diferença serão excluídas, pois não hã suporte na base de predição
train_test_base$TIPO <- "train_test"
predic_base$TIPO <- "predict"

base_comp <- rbind(train_test_base, predic_base) %>% as.data.frame()
rowPerc(table(base_comp$TIPO, base_comp$TERRITORIO))
rowPerc(table(base_comp$TIPO, base_comp$PROF_SAUDE)) # Retirar por estar muito desbalanceado entre as duas bases
rowPerc(table(base_comp$TIPO, base_comp$SEXO))
rowPerc(table(base_comp$TIPO, base_comp$DATA_NOTIFICACAO))
rowPerc(table(base_comp$TIPO, base_comp$DOR_GARGANTA)) # Retirar por estar muito desbalanceado entre as duas bases
rowPerc(table(base_comp$TIPO, base_comp$DISPINEIA)) # Retirar por estar muito desbalanceado entre as duas bases
rowPerc(table(base_comp$TIPO, base_comp$FEBRE)) # Retirar por estar muito desbalanceado entre as duas bases
rowPerc(table(base_comp$TIPO, base_comp$TOSSE)) # Retirar por estar muito desbalanceado entre as duas bases
rowPerc(table(base_comp$TIPO, base_comp$RESULTADO))
rowPerc(table(base_comp$TIPO, base_comp$TIPO_EXAME)) # Retirar por estar muito desbalanceado entre as duas bases
rowPerc(table(base_comp$TIPO, base_comp$IDADE))
rowPerc(table(base_comp$TIPO, base_comp$FAIXA_ETARIA))
rowPerc(table(base_comp$TIPO, base_comp$TRIAGEM))
rowPerc(table(base_comp$TIPO, base_comp$INFECTADOS_TERRITORIO))
rowPerc(table(base_comp$TIPO, base_comp$TX_INFECTADOS_TERRITORIO))


covid$PROF_SAUDE <- NULL
covid$DOR_GARGANTA <- NULL
covid$DISPINEIA <- NULL
covid$FEBRE <- NULL
covid$TOSSE <- NULL
covid$TIPO_EXAME <- NULL
covid$SUBTERRITORIO <-NULL
covid$TRIAGEM <- NULL

# Exportando base ---------------------------------------------------------
write.csv(covid, "nowcasting/dados/covid_ajustado.csv", row.names = F, fileEncoding = "UTF-8")
