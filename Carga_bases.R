# Carregar os pacotes necessários
library(tidyverse)
library(data.table)

rm(list = ls())
gc()

#####################################################
########## Carregar a base de eleitorado ###########
####################################################

# Mudar o diretório de trabalho
setwd("F:/OneDrive/Doutorado/Tese/Bases/")

# Criar diretórios para download dos dados de eleitorado
dir.create("TSE/Eleitorado", recursive = T)

# Anos a serem baixados
anos <- seq(1994,format(Sys.Date(), "%Y"),2)

# Endereço dos dados de eleitorado
dwlpath <- "http://agencia.tse.jus.br/estatistica/sead/odsele/perfil_eleitorado/perfil_eleitorado_"


# looping para baixar os dados
for (i in anos){
  tf <- paste0(dwlpath, i, ".zip")
  td <- paste0("./TSE/Eleitorado/", i, ".zip")
  print(i)
  download.file(tf, td, mode="wb")
}

# Descompactar arquivos
filenames <- list.files("./TSE/Eleitorado/", pattern=".zip", full.names=TRUE)
lapply(filenames,unzip, exdir = "./TSE/Eleitorado",junkpaths=T)

# Carregar os anos de interesse
anos <- seq(2000,2014,2)

# Looping para carregar todas as bases

eleitorado <- c()

for(i in anos){
  
base <- data.table::fread(paste0("TSE/Eleitorado/perfil_eleitorado_",i,
                                 if (a >= 2018){".csv"}else{".txt"}),
                                encoding = "Latin-1") %>%
  dplyr::mutate(ANO=paste0(i)) %>%
  dplyr::rename_all(funs(c("PERIODO","UF","MUNICIPIO","COD_MUNICIPIO_TSE",
                           "NR_ZONA","SEXO","FAIXA_ETARIA","GRAU_DE_ESCOLARIDADE",
                           "QTD_ELEITORES_NO_PERFIL","ANO"))) 

base <- bind_rows(
  
  base %>%
    dplyr::mutate(var=case_when(GRAU_DE_ESCOLARIDADE=="SUPERIOR COMPLETO"~"superior",
                                   GRAU_DE_ESCOLARIDADE %in% c("SUPERIOR INCOMPLETO",
                                                               "SEGUNDO GRAU COMPLETO",
                                                               "ENSINO MÉDIO COMPLETO")~"medio")) %>%
    dplyr::group_by(ANO,COD_MUNICIPIO_TSE,MUNICIPIO,var) %>%
    dplyr::summarise(n=sum(QTD_ELEITORES_NO_PERFIL)) %>%
    na.omit,
  
  base %>%
    dplyr::mutate(var=case_when(FAIXA_ETARIA %in% c("16 ANOS","17 ANOS")~"facult_jov",
                    FAIXA_ETARIA %in% c("18 A 20 ANOS",
                                        "21 A 24 ANOS",
                                        "25 A 34 ANOS",
                                        "35 A 44 ANOS",
                                        "45 A 59 ANOS",
                                        "60 A 69 ANOS")~"Obrigatorio",
                    FAIXA_ETARIA %in% c("70 A 79 ANOS",
                                        "SUPERIOR A 79 ANOS")~"facult_idoso")) %>%
    dplyr::group_by(ANO,COD_MUNICIPIO_TSE,MUNICIPIO,var) %>%
    dplyr::summarise(n=sum(QTD_ELEITORES_NO_PERFIL)) %>%
    na.omit,
  
  base %>%
    dplyr::mutate(var=case_when(SEXO=="FEMININO"~"sex_feminino",
                                SEXO=="MACULINO"~"sex_masculino",
                                TRUE~"sex_na")) %>%
    dplyr::group_by(ANO,COD_MUNICIPIO_TSE,MUNICIPIO,var) %>%
    dplyr::summarise(n=sum(QTD_ELEITORES_NO_PERFIL)) %>%
    na.omit)  %>%
    tidyr::spread(var,n)

eleitorado <<- rbind(eleitorado,base)

}

# Remover objetos desnecessarios
rm(base)

# Remover arquivos descompactados
file.remove(list.files("TSE/Eleitorado",pattern = ".txt|csv",full.names = T))

##############################################################
########## Carregar a base de Resultado - Apuração ###########
##############################################################

# Criar diretórios para download dos dados de eleitorado
dir.create("TSE/Resultado", recursive = T)

# Anos a serem baixados
anos <- seq(1994,format(Sys.Date(), "%Y"),2)

# Endereço dos dados de eleitorado
dwlpath <- "http://agencia.tse.jus.br/estatistica/sead/odsele/detalhe_votacao_munzona/detalhe_votacao_munzona_"

# looping para baixar os dados
for (i in anos){
  tf <- paste0(dwlpath, i, ".zip")
  td <- paste0("./TSE/Resultado/","detalhe",i, ".zip")
  print(i)
  download.file(tf, td, mode="wb")
}

# Descompactar arquivos
filenames <- list.files("./TSE/Resultado/", pattern=".zip", full.names=TRUE)
lapply(filenames,unzip, exdir = "./TSE/Resultado",junkpaths=T)

# Carregar a base de resultado do TSE de 2018
# Fazer o looping para carregar todos os municípios

# Caminho inicial
initp <- "TSE/Resultado/detalhe_votacao_munzona_"

# Anos para análise
anos <- seq(2000,2014,2)

# Estrutura de nomes até 2012
names2012 <- c("DT_GERACAO","HH_GERACAO","ANO_ELEICAO","NR_TURNO",
               "DS_ELEICAO","SG_UF","SG_UE","CD_MUNICIPIO",
               "NM_MUNICIPIO","NR_ZONA","CD_CARGO","DS_CARGO",
               "QT_APTOS","QT_SECOES","QT_SECOES_AGREGADAS",
               "QT_APTOS_TOT","QT_SECOES_TOT","QT_COMPARECIMENTO",
               "QT_ABSTENCOES","QT_VOTOS_NOMINAIS","QT_VOTOS_BRANCOS",
               "QT_VOTOS_NULOS","QT_VOTOS_LEGENDA",
               "QT_VOTOS_ANULADOS","DT_ULTIMA_TOTALIZACAO",
               "HH_ULTIMA_TOTALIZACAO")

# Estrutura de nomes de 2014 a 2016
# Obs.: 2018 já vem com names no arquivo
names2014 <- c(names2012,"TRANSITO","QTD_VOTOS_ANULADOS")


# UFs
ufs <- c("AC","AL","AM","AP","BA","CE","ES","GO","MA",
         "MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO",
         "RR","RS","SC","SE","SP","TO")

ufs2 <- c(ufs,"BR","DF")

# Construir objeto para receber valores
resultado <- c()

for(a in anos){

for(i in if(a %in% c(seq(1994,format(Sys.Date(),"%Y"),4))){ufs2}else{ufs}){
  

base <- data.table::fread(paste0(initp,a,"_",i,if (a >= 2018){".csv"}else{".txt"}),
                          encoding = "Latin-1")

if(a<=2012){
    
    base <- base %>%
    
    dplyr::rename_all(funs(eval(names2012)))} else{
      
      if(a %in% c(2014,2016)){
        
        base <- base %>%
        
        dplyr::rename_all(funs(eval(names2014)))}  else{
            
        }
      }
base <-
base %>%
  dplyr::group_by(CD_MUNICIPIO,DS_CARGO) %>%
  dplyr::summarise(NM_MUNICIPIO=first(NM_MUNICIPIO),
                   ANO_ELEICAO=first(ANO_ELEICAO),
                   aptos=sum(QT_APTOS),
                   aptos_tot=sum(QT_APTOS_TOT),
                   compareceu=sum(QT_COMPARECIMENTO),
                   abstencao=sum(QT_ABSTENCOES),
                   brancos=sum(QT_VOTOS_BRANCOS),
                   nulos=sum(QT_VOTOS_NULOS)) %>%
  dplyr::mutate(participacao=compareceu/aptos_tot,
                pct_nulos=nulos/compareceu,
                pct_brancos=brancos/compareceu)

resultado <<- rbind(resultado,base)

}
}

# Remover arquivos descompactados
file.remove(list.files("TSE/Resultado",pattern = ".txt|csv",full.names = T))

#############################################################
########## Carregar a base de Resultado - Votação ###########
#############################################################

# Criar novo diretório
dir.create("TSE/Resultado/Partidos", recursive = T)

# Anos a serem baixados
anos <- seq(1994,format(Sys.Date(), "%Y"),2)

# Endereço dos dados de eleitorado
dwlpath <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_partido_munzona/votacao_partido_munzona_"

# looping para baixar os dados
for (i in anos){
  tf <- paste0(dwlpath, i, ".zip")
  td <- paste0("./TSE/Resultado/Partidos/","partido",i, ".zip")
  print(i)
  download.file(tf, td, mode="wb")
}

# Descompactar arquivos
filenames <- list.files("./TSE/Resultado/Partidos", pattern=".zip", full.names=TRUE)
lapply(filenames,unzip, exdir = "./TSE/Resultado/Partidos",junkpaths=T)


# Selecionar anos de interesse
anos <- seq(2000,2014,2)

# Caminho inicial dos arquivos
initp <- "TSE/Resultado/Partidos/votacao_partido_munzona_"

# Nomes até 2012
names2012 <- c("DT_GERACAO","HH_GERACAO","ANO_ELEICAO","NR_TURNO",
               "DS_ELEICAO","SG_UF","SG_UE","CD_MUNICIPIO",
               "NM_MUNICIPIO","NR_ZONA","CD_CARGO","DS_CARGO",
               "TP_AGREMIACAO","NM_COLIGACAO","DS_COMPOSICAO_COLIGACAO",
               "SG_PARTIDO","NR_PARTIDO","NM_PARTIDO",
               "QT_VOTOS_NOMINAIS","QT_VOTOS_LEGENDA","SEQUENCIAL_COLIGACAO")

# Nomes até 2014
names2014 <- c(names2012[1:20],"TRANSITO",names2012[21])

# # Excluir arquivos vazios da pasta
# ## Get vector of all file names
# ff <- dir("./TSE/Resultado/Partidos", recursive=TRUE, full.names=TRUE)
# ## Extract vector of empty files' names
# eff <- ff[file.info(ff)[["size"]]==0]
# ## Remove empty files
# file.remove(eff, recursive=TRUE, force=FALSE)

# UFs
ufs <- c("AC","AL","AM","AP","BA","CE","ES","GO","MA",
         "MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO",
         "RR","RS","SC","SE","SP","TO")

ufs2 <- c(ufs,"DF")

ufs3 <- c(ufs,"BR","DF")


# Construir objeto para receber valores
votacao <- c()

# Iniciar loop
for(a in anos){
  
  for(i in if(a%in%c(1994,seq(1996,format(Sys.Date(),"%Y"),4))){ufs}else{if(a%in%c(1998,2002)){ufs2}else{ufs3}}){

base <- data.table::fread(paste0(initp,a,"_",i,if (a >= 2018){".csv"}else{".txt"}),
                          encoding = "Latin-1")


if(a<=2012){
  
  base <- base %>%
    
    dplyr::rename_all(funs(eval(names2012)))} else{
      
      if(a %in% c(2014,2016)){
        
        base <- base %>%
          
          dplyr::rename_all(funs(eval(names2014)))}  else{
            
          }
    }

base <- base %>%
  dplyr::mutate(votos=QT_VOTOS_NOMINAIS+QT_VOTOS_LEGENDA) %>%
  dplyr::group_by(ANO_ELEICAO,CD_MUNICIPIO,NM_MUNICIPIO,DS_CARGO,SG_PARTIDO) %>%
  dplyr::summarise(votos=sum(votos))

votacao <<- rbind(votacao,base)

  }
}

# Remover arquivos descompactados
file.remove(list.files("TSE/Resultado/Partidos",pattern = ".txt|csv",full.names = T))

#############################################################
########## Carregar a base da BLS - Zucco ####################
#############################################################
load("bls7_released_v01.RData")

# Fazer a média ponderada da orientação ideológica dos partidos
indexparty <- bls %>%
  dplyr::select(party_survey,czideo,pweight) %>%
  dplyr::mutate(czideo=as.numeric(czideo),
                party_survey=as.character(party_survey)) %>%
  dplyr::group_by(party_survey) %>%
  dplyr::summarise(index=weighted.mean(czideo,pweight ,na.rm = T)) %>%
  dplyr::arrange(index) 

# Armazenar 
left_parties <- indexparty[indexparty$index<=quantile(indexparty$index, na.rm=T,0.25),]$party_survey
center_parties <- indexparty[indexparty$index>quantile(indexparty$index, na.rm=T,0.25)&
                               indexparty$index<quantile(indexparty$index, na.rm=T,0.75),]$party_survey
right_parties <- indexparty[indexparty$index>=quantile(indexparty$index, na.rm=T,0.75),]$party_survey

#############################################################
########## Carregar a base de Candidatos ####################
#############################################################

# Criar novo diretório
dir.create("TSE/Candidatos", recursive = T)

# Anos a serem baixados
anos <- seq(1994,format(Sys.Date(), "%Y"),2)

# Endereço dos dados de eleitorado
dwlpath <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_"

# looping para baixar os dados
for (i in anos){
  tf <- paste0(dwlpath, i, ".zip")
  td <- paste0("./TSE/Candidatos/","candidato",i, ".zip")
  print(i)
  download.file(tf, td, mode="wb")
}

# Descompactar arquivos
filenames <- list.files("./TSE/Candidatos", pattern=".zip", full.names=TRUE)
lapply(filenames,unzip, exdir = "./TSE/Candidatos",junkpaths=T)


# Selecionar anos de interesse
anos <- seq(2000,2014,2)

initp <- "TSE/Candidatos/consulta_cand_"

names2010 <- c("DT_GERACAO","HH_GERACAO","ANO_ELEICAO",
               "NR_TURNO","DS_ELEICAO","SG_UF","SG_UE","DS_UE",
               "CD_CARGO","DS_CARGO","NM_CANDIDATO", 
               "SQ_CANDIDATO","NR_CANDIDATO",
               "NR_CPF_CANDIDATO","NM_URNA_CANDIDATO",
               "CD_SITUACAO_CANDIDATURA","DS_SITUACAO_CANDIDATURA",
               "NR_PARTIDO","SG_PARTIDO","NM_PARTIDO","SQ_COLIGACAO",
               "SG_COLIGACAO","DS_COMPOSICAO_COLIGACAO","NM_COLIGACAO",
               "CD_OCUPACAO","DS_OCUPACAO","DT_NASCIMENTO",
               "NR_TITULO_ELEITORAL_CANDIDATO","NR_IDADE_DATA_POSSE",
               "CD_GENERO","DS_GENERO","CD_GRAU_INSTRUCAO","DS_GRAU_INSTRUCAO",
               "CD_ESTADO_CIVIL","DS_ESTADO_CIVIL",
               "CD_NACIONALIDADE","DS_NACIONALIDADE","SG_UF_NASCIMENTO",
               "CD_MUNICIPIO_NASCIMENTO","NM_MUNICIPIO_NASCIMENTO",
               "NR_DESPESA_MAX_CAMPANHA","CD_SIT_TOT_TURNO","DS_SIT_TOT_TURNO")

names2012 <- c(names2010,"NM_EMAIL")

# UFs
ufs <- c("AC","AL","AM","AP","BA","CE","ES","GO","MA",
         "MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO",
         "RR","RS","SC","SE","SP","TO")

ufs2 <- c(ufs,"BR")

ufs3 <- c(ufs,"BR","DF")

candidato <- c()

for(a in anos){
  
  for(i in if(a==1994){ufs2}else{if(a %in% c(seq(1998,format(Sys.Date(),"%Y"),4))){ufs3}else{ufs}}){

base <- data.table::fread(paste0(initp,a,"_",i,if (a >= 2014){".csv"}else{".txt"}),
                          encoding = "Latin-1")

if(a<=2010){
  
  base <- base %>%
    
    dplyr::rename_all(funs(eval(names2010)))} else{
      
      if(a==2012){
        
        base <- base %>%
          
          dplyr::rename_all(funs(eval(names2012)))}  else{
            
          }
    }

base <-
base %>%
  dplyr::filter(DS_SITUACAO_CANDIDATURA%in%c("DEFERIDO","DEFERIDO COM RECURSO","APTO")) %>%
  dplyr::mutate(orientacao=case_when(SG_PARTIDO %in% c("PSTU","PSOL","PC do B",
                                                       "PT","PSDB")~"Esquerda",
                                     SG_PARTIDO %in% c("PDT","PV","PCB",
                                                       "PPS","PSDB","PMDB",
                                                       "PTB","PSD","PL",
                                                       "PRONA","PR","PSC",
                                                       "PRB")~"Centro",
                                     SG_PARTIDO %in% c("PRN","PFL","DEM",
                                                       "PDS","PPR","PDC",
                                                       "PPB","PP","PMN")~"Direita",
                                     TRUE~"Outros")) %>%
  dplyr::count(ANO_ELEICAO,DS_CARGO,SG_UF,orientacao,
               DS_SIT_TOT_TURNO)


candidato <<- rbind(candidato,base)


  }
}

# Remover arquivos descompactados
file.remove(list.files("TSE/Candidatos",pattern = ".txt|csv",full.names = T))

# Remover objetos desnecessários
rm(bls,base,a,anos,dwlpath,filenames,i,initp,names2010,
   names2012,names2014,td,tf,ufs,ufs2,ufs3)

#################################################################
########## Carregar dados Interlegis         ####################
#################################################################
# Carregar uma tabela do Censo Legislativo para pegar a correspondencia entre
# o COD_MUN do IBGE e do TSE

censolegis <- data.table::fread("http://www.interlegis.leg.br/produtos_servicos/informacao/censo/relatorios/1-cadastro-da-cm.csv/download",
                                encoding = "Latin-1") %>%
  dplyr::select(Cod_TSE,Cod_IBGE,localidade) %>%
  dplyr::rename(CD_MUNICIPIO=Cod_TSE,
                COD_IBGE=Cod_IBGE) %>%
  # Adicionar código das cidades sem informações no interlegis
  dplyr::bind_rows(bind_cols(
    CD_MUNICIPIO=c(4863,12734,33090,37915,55050,55069,83909,
                   83925,89540,89931,89931,91065,91081,91103,
                   97012,99901,99902,99903,99904,99905,99906,
                   99907,99908,99909,99910,99911,99912,99913,
                   99914,99915,99916,99917,99918,99919,99920,
                   99921,99922,99923,99924,99925,99926,99927,12726,30015),
    COD_IBGE=c(1504752,2200950,2900504,2924504,5006275,5003856,4220000,
               4212650,4314548,5107107,5107107,4103008,5104535,
               5104545,5300108,3550308,3106200,3304557,4314902,
               2927408,4106902,2304400,2611606,4205407,5208707,
               2111300,2507507,1501402,3205309,2211001,2408102,
               2704302,5103403,5002704,5300108,2800308,1302603,
               1100205,1200401,1600303,1400100,1721000,2206720,2605459),
    localidade=c("MOJUÍ DOS CAMPOS","AROEIRAS DO ITAIM","ERICO CARDOSO","PINDAI",
                "PARAÍSO DAS ÁGUAS","FIGUEIRÃO","BALNEÁRIO RINCÃO",
                "PESCARIA BRAVA","PINTO BANDEIRA","QUATRO MARCOS",
                "SÃO JOSÉ DOS QUATRO MARCOS","BOA ESPERANCA DO NORTE",
                "IPIRANGA DO NORTE","ITANHANGÁ","BRASÍLIA",
                "SÃO PAULO","BELO HORIZONTE","RIO DE JANEIRO",
                "PORTO ALEGRE","SALVADOR","CURITIBA","FORTALEZA",
                "RECIFE","FLORIANÓPOLIS","GOIÂNIA","SÃO LUÍS",
                "JOÃO PESSOA","BELÉM","VITÓRIA","TERESINA","NATAL",
                "MACEIÓ","CUIABÁ","CAMPO GRANDE","BRASÍLIA","ARACAJU",
                "MANAUS","PORTO VELHO","RIO BRANCO","MACAPÁ","BOA VISTA",
                "PALMAS","NAZÁRIA","FERNANDO DE NORONHA")))

#################################################################
########## Carregar dados da Anatel, backhaul####################
#################################################################

backhaul <- readstata13::read.dta13("anatel_backhaul.dta") %>%
  dplyr::select(fk_cod_municipio,ano,backhaul_ano,backhaul,popIBGE_anoantes,
                Situao,tecnologiadeatendimento,UF,Municpio,
                concessionaria)

#################################################################
########## Carregar dados da Anatel, Brutos #####################
#################################################################

# Carregar dados de internet de 2007 a 2010
SCM_2007_2010 <- data.table::fread("Acessos_SCM_2007-2010_-_Total.csv",
                                   skip = 1) %>%
  dplyr::mutate_at(vars(`2007-03`:`2010-12`),
                   funs(case_when(is.na(.)==T~0,
                             TRUE~as.numeric(.)))) %>%
  dplyr::filter(!Faixa %in% c("64Kbps a 512Kbps","0Kbps a 64Kbps","0kbps a 512kbps")) %>%
  dplyr::select(-c(Autorizada,CNPJ,Grupo,`Região`,UF)) %>%
  tidyr::gather("Ref","valor",5:length(.)) %>%
  dplyr::mutate(Ano=stringr::str_sub(Ref,1,4)) %>%
  dplyr::group_by(`Município`,`Cod IBGE`,
                  #Tecnologia,
                  Faixa,Ano) %>%
  dplyr::summarise(valor=sum(valor)) %>%
  dplyr::arrange(`Município`,Ano)

# Carregar dados de internet de 2011 a 2012
SCM_2011_2012 <- data.table::fread("Acessos_SCM_2011-2012_-_Total.csv",
                                   skip = 1) %>%
  dplyr::mutate_at(vars(`2011-01`:`2012-12`),
                   funs(case_when(is.na(.)==T~0,
                                  TRUE~as.numeric(.)))) %>%
  dplyr::filter(!Faixa %in% c("64Kbps a 512Kbps","0Kbps a 64Kbps","0kbps a 512kbps")) %>%
  dplyr::select(-c(Autorizada,CNPJ,Grupo,`Região`,UF)) %>%
  tidyr::gather("Ref","valor",5:length(.)) %>%
  dplyr::mutate(Ano=stringr::str_sub(Ref,1,4)) %>%
  dplyr::group_by(`Município`,`Cod IBGE`,
                  #Tecnologia,
                  Faixa,Ano) %>%
  dplyr::summarise(valor=sum(valor)) %>%
  dplyr::arrange(`Município`,Ano)
  
# Carregar dados de internet de 2013 e 2014 
SCM_2013_2014 <- data.table::fread("Acessos_SCM_2013-2014_-_Total.csv",
                                   skip = 0) %>%
  dplyr::mutate_at(vars(`2013-01`:`2014-12`),
                   funs(case_when(is.na(.)==T~0,
                                  TRUE~as.numeric(.)))) %>%
  dplyr::filter(!Faixa %in% c("64Kbps a 512Kbps","0Kbps a 64Kbps","0kbps a 512kbps")) %>%
  dplyr::select(-c(Autorizada,CNPJ,Grupo,`Região`,UF)) %>%
  tidyr::gather("Ref","valor",5:length(.)) %>%
  dplyr::mutate(Ano=stringr::str_sub(Ref,1,4)) %>%
  dplyr::rename(`Cod IBGE`=`Cod Municipio`) %>%
  dplyr::group_by(`Município`,`Cod IBGE`,
                  #Tecnologia,
                  Faixa,Ano) %>%
  dplyr::summarise(valor=sum(valor)) %>%
  dplyr::arrange(`Município`,Ano)

# Carregar dados de internet de 2015 até hoje
SCM_2015_2018 <- data.table::fread("Acessos_SCM_2015-2018_-_Total.csv",
                                   skip = 0) %>%
  dplyr::mutate_at(vars(`2015-01`:`2018-12`),
                   funs(case_when(is.na(.)==T~0,
                                  TRUE~as.numeric(.)))) %>%
  dplyr::filter(!Faixa %in% c("64Kbps a 512Kbps","0Kbps a 64Kbps","0kbps a 512kbps")) %>%
  dplyr::select(-c(Autorizada,CNPJ,Grupo,`Região`,UF)) %>%
  tidyr::gather("Ref","valor",5:length(.)) %>%
  dplyr::mutate(Ano=stringr::str_sub(Ref,1,4)) %>%
  dplyr::rename(`Cod IBGE`=`Código Município`) %>%
  dplyr::group_by(`Município`,`Cod IBGE`,
                  #Tecnologia,
                  Faixa,Ano) %>%
  dplyr::summarise(valor=sum(valor)) %>%
  dplyr::arrange(`Município`,Ano)

# Juntar a porra toda
SCM_2007_2018 <- SCM_2007_2010 %>%
  dplyr::bind_rows(SCM_2011_2012,
                   SCM_2013_2014,
                   SCM_2015_2018) %>%
  dplyr::arrange(`Município`,Ano)

# Excluir objetos desnecessários
rm(SCM_2007_2010,SCM_2011_2012,SCM_2013_2014,SCM_2015_2018)

# Carregar os dados de internet móvel
# 2009 não resolve pq tem somente por região
#smp_2005_200901 <- data.table::fread("Acessos_SMP_2005-200901_-_Total.csv") 

# Carregar dados 200902 a 2014
smp_200902_2014 <- data.table::fread("Acessos_SMP_200902-2014_-_Total.csv") %>%
  dplyr::mutate_at(vars(`2009-02`:`2014-12`),
                   funs(case_when(is.na(.)==T~0,
                                  TRUE~as.numeric(.)))) %>%
  tidyr::gather("Ref","valor",8:length(.)) %>%
  dplyr::mutate(Ano=stringr::str_sub(Ref,1,4)) %>%
  dplyr::group_by(DDD,Tipo,Ano) %>%
  dplyr::summarise(Valor=sum(valor))

# Carregar dados de 2015 até hoje
smp_2015_2018 <- data.table::fread("Acessos_SMP_2015-2018_-_Total.csv") %>%
  dplyr::mutate_at(vars(`2015-01`:`2018-12`),
                   funs(case_when(is.na(.)==T~0,
                                  TRUE~as.numeric(.)))) %>%
  tidyr::gather("Ref","valor",9:length(.)) %>%
  dplyr::mutate(Ano=stringr::str_sub(Ref,1,4)) %>%
  dplyr::group_by(DDD,Tipo,Ano) %>%
  dplyr::summarise(Valor=sum(valor))

# Juntar a porra toda
smp2009_2018 <- smp_200902_2014 %>%
  dplyr::bind_rows(smp_2015_2018) 

# Excluir objetos desnecessários
rm(smp_2005_200901, smp_200902_2014, smp_2015_2018)

# Carregar correspondências municípios DDD
dic_DDD <- data.table::fread("PGCN com código IBGE.csv")

#################################################################
########## Carregar dados do Censo ##############################
#################################################################
censo <- readxl::read_excel("./Censo/censo_vars.xlsx")

