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
anos <- seq(2000,2018,2)

# Looping para carregar todas as bases

eleitorado <- c()

for(a in anos){
  
  if(a>=2018){
    
    base <- data.table::fread(paste0("TSE/Eleitorado/perfil_eleitorado_",a,
                                     if (a >= 2018){".csv"}else{".txt"}),
                              encoding = "Latin-1") %>%
      dplyr::mutate(ANO=paste0(a)) %>%
      dplyr::select(MUNICIPIO=NM_MUNICIPIO,
                    COD_MUNICIPIO_TSE=CD_MUNICIPIO,
                    SEXO=DS_GENERO,
                    FAIXA_ETARIA=DS_FAIXA_ETARIA,
                    ANO=ANO,
                    GRAU_DE_ESCOLARIDADE=DS_GRAU_ESCOLARIDADE,
                    QTD_ELEITORES_NO_PERFIL=QT_ELEITORES_PERFIL)
    
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
        dplyr::mutate(var=case_when(toupper(str_trim(FAIXA_ETARIA)) %in% c("16 ANOS","17 ANOS")~"facult_jov",
                                    toupper(str_trim(FAIXA_ETARIA)) %in% c("18 A 20 ANOS",
                                                        "21 A 24 ANOS",
                                                        "25 A 34 ANOS",
                                                        "35 A 44 ANOS",
                                                        "45 A 59 ANOS",
                                                        "60 A 69 ANOS")~"Obrigatorio",
                                    toupper(str_trim(FAIXA_ETARIA)) %in% c("70 A 79 ANOS",
                                                        "SUPERIOR A 79 ANOS")~"facult_idoso")) %>%
        dplyr::group_by(ANO,COD_MUNICIPIO_TSE,MUNICIPIO,var) %>%
        dplyr::summarise(n=sum(QTD_ELEITORES_NO_PERFIL)) %>%
        na.omit,
      
      base %>%
        dplyr::mutate(var=case_when(SEXO=="FEMININO"~"sex_feminino",
                                    SEXO=="MASCULINO"~"sex_masculino",
                                    TRUE~"sex_na")) %>%
        dplyr::group_by(ANO,COD_MUNICIPIO_TSE,MUNICIPIO,var) %>%
        dplyr::summarise(n=sum(QTD_ELEITORES_NO_PERFIL)) %>%
        na.omit)  %>%
      tidyr::spread(var,n)
    
  }else{
  
base <- data.table::fread(paste0("TSE/Eleitorado/perfil_eleitorado_",a,
                                 if (a >= 2018){".csv"}else{".txt"}),
                                encoding = "Latin-1") %>%
  dplyr::mutate(ANO=paste0(a)) %>%
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
                                SEXO=="MASCULINO"~"sex_masculino",
                                TRUE~"sex_na")) %>%
    dplyr::group_by(ANO,COD_MUNICIPIO_TSE,MUNICIPIO,var) %>%
    dplyr::summarise(n=sum(QTD_ELEITORES_NO_PERFIL)) %>%
    na.omit)  %>%
    tidyr::spread(var,n)}

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
anos <- seq(2000,2018,2)

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
  

base <- data.table::fread(paste0(initp,a,"_",i,if (a >= 2016){".csv"}else{".txt"}),
                          encoding = "Latin-1")

if(a<=2012){
    
    base <- base %>%
    
    dplyr::rename_all(funs(eval(names2012)))} else{
      
      if(a %in% c(2014)){
        
        base <- base %>%
        
        dplyr::rename_all(funs(eval(names2014)))}  else{
            
        }
    }

base <-
  base %>%
  dplyr::group_by(CD_MUNICIPIO,DS_CARGO, NR_TURNO,DS_ELEICAO) %>%
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

esquerda <- c("PSTU","PSOL","PC do B",
              "PT","PSB","PC DO B",
              "PCO")

centro <- c("PDT","PV","PCB","PAN","PGT","PMB","PROS",
            "PST","REDE","PPS","CIDADANIA","PSDB","PMDB",
            "MDB","PTB","PSD","PL","PRONA","PR","PSC",
            "PRB","REPUBLICANOS","PPL","PRP","PSN","PHS",
            "PT do B","AVANTE", "PTN","PODEMOS",
            "SD","SOLIDARIEDADE","NOVO")

direita <- c("PRN","PTC","PFL","DEM",
             "PSDC","DC","PDS","PPR","PDC",
             "PPB","PP","PMN","PSL",     
             "PRTB","PEN","PATRIOTA","PATRI")

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
anos <- seq(2000,2018,2)

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

base <- data.table::fread(paste0(initp,a,"_",i,if (a >= 2016){".csv"}else{".txt"}),
                          encoding = "Latin-1")


if(a<=2012){
  
  base <- base %>%
    
    dplyr::rename_all(funs(eval(names2012)))} else{
      
      if(a %in% c(2014)){
        
        base <- base %>%
          
          dplyr::rename_all(funs(eval(names2014)))}  else{
            
          }
    }

base <- base %>%
  dplyr::mutate(votos=QT_VOTOS_NOMINAIS+QT_VOTOS_LEGENDA,
                orientacao=case_when(SG_PARTIDO %in% esquerda~"Esquerda",
                                     SG_PARTIDO %in% centro~"Centro",
                                     SG_PARTIDO %in% direita~"Direita",
                                     TRUE~"Outros")) %>%
  dplyr::group_by(ANO_ELEICAO,CD_MUNICIPIO,NM_MUNICIPIO,DS_CARGO,NR_TURNO,SG_PARTIDO,orientacao) %>%
  dplyr::summarise(votos=sum(votos))

votacao <<- rbind(votacao,base)

  }
}

# Remover arquivos descompactados
file.remove(list.files("TSE/Resultado/Partidos",pattern = ".txt|csv",full.names = T))

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
anos <- seq(2000,2018,2)

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
            
            base <- base %>%
              dplyr::rename(DS_UE=NM_UE)
            
          }
    }

base <-
base %>%
  #dplyr::filter(DS_SITUACAO_CANDIDATURA%in%c("DEFERIDO","DEFERIDO COM RECURSO","APTO","SUB JUDICE")) %>%
  dplyr::mutate(orientacao=case_when(SG_PARTIDO %in% esquerda~"Esquerda",
                                     SG_PARTIDO %in% centro~"Centro",
                                     SG_PARTIDO %in% direita~"Direita",
                                     TRUE~"Outros")) %>%
  dplyr::count(ANO_ELEICAO,SG_UF,SG_UE,DS_UE,DS_CARGO,NR_TURNO,
               DS_ELEICAO,DS_SITUACAO_CANDIDATURA,orientacao,DS_SIT_TOT_TURNO)


candidato <<- rbind(candidato,base)


  }
}

candidato2 <- c()

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
                
                base <- base %>%
                  dplyr::rename(DS_UE=NM_UE)
                
              }
        }
    
    base <-
      base %>%
      #dplyr::filter(DS_SITUACAO_CANDIDATURA%in%c("DEFERIDO","DEFERIDO COM RECURSO","APTO","SUB JUDICE")) %>%
      dplyr::mutate(orientacao=case_when(SG_PARTIDO %in% esquerda~"Esquerda",
                                         SG_PARTIDO %in% centro~"Centro",
                                         SG_PARTIDO %in% direita~"Direita",
                                         TRUE~"Outros")) %>%
      dplyr::count(ANO_ELEICAO,SG_UF,SG_UE,DS_UE,DS_CARGO,NR_TURNO,
                   DS_ELEICAO,DS_SITUACAO_CANDIDATURA,orientacao,SG_PARTIDO,DS_SIT_TOT_TURNO)
    
    
    candidato2 <<- rbind(candidato2,base)
    
    
  }
}

# Remover arquivos descompactados
file.remove(list.files("TSE/Candidatos",pattern = ".txt|csv",full.names = T))

# Remover objetos desnecessários
rm(bls,base,a,anos,dwlpath,filenames,i,initp,names2010,
   names2012,names2014,td,tf,ufs,ufs2,ufs3)

#############################################################
########## Carregar a base de prestação de contas ####################
#############################################################

# Criar novo diretório
dir.create("TSE/Contas", recursive = T)

# Anos a serem baixados
anos <- seq(2002,format(Sys.Date(), "%Y"),2)

# Endereço dos dados de eleitorado
dwlpath <- "http://agencia.tse.jus.br/estatistica/sead/odsele/prestacao_contas/prestacao_contas_"
dwlpath2<-"http://agencia.tse.jus.br/estatistica/sead/odsele/prestacao_contas/prestacao_final_"
dwlpath3<-"http://agencia.tse.jus.br/estatistica/sead/odsele/prestacao_contas/prestacao_de_contas_eleitorais_candidatos_"


# looping para baixar os dados
for (i in anos){
  tf <- paste0(if(i>=2018){dwlpath3}else{if(i %in% c(2012,2014)){dwlpath2}else{dwlpath}}, i, ".zip")
  td <- paste0("./TSE/Contas/","conta",i, ".zip")
  print(i)
  download.file(tf, td, mode="wb")
}

# Descompactar arquivos
filenames <- list.files("./TSE/Contas", pattern=".zip", full.names=TRUE)
lapply(filenames[-c(1,2,3,5)],unzip, exdir = "./TSE/Contas",junkpaths=T)
lapply(filenames[c(2,5)],unzip, exdir = "./TSE/Contas",junkpaths=F)
lapply(filenames[c(1,3)],untar, exdir = "./TSE/Contas")


receitas <- c()

for(a in c(2002,2004,2006)){
  
  if(a==2004){
    
    base <- data.table::fread(paste0("./TSE/Contas/",
                                     a,"/Candidato/Receita/ReceitaCandidato.csv")) %>% 
      dplyr::select(DS_CARGO,
                    SG_PARTIDO=SG_PART,
                    SG_UE,
                    VR_RECEITA) %>% 
      dplyr::mutate(orientacao=case_when(SG_PARTIDO %in% esquerda~"Esquerda",
                                         SG_PARTIDO %in% centro~"Centro",
                                         SG_PARTIDO %in% direita~"Direita",
                                         TRUE~"Outros"),
                    ANO=a,
                    SG_UE=as.character(SG_UE)) %>% 
      dplyr::group_by(ANO,SG_UE,DS_CARGO,SG_PARTIDO,orientacao) %>% 
      dplyr::summarise(VR_RECEITA=sum(as.numeric(gsub(",",".",VR_RECEITA))))
    
    receitas <<- rbind(receitas,base)
    
    
    
  }else{
  
  base <- data.table::fread(paste0("./TSE/Contas/prestacao_contas_",
                                   a,"/",a, "/Candidato/Receita/ReceitaCandidato.csv")) %>% 
    dplyr::select(DS_CARGO=if(a==2002){"DS_CARGO"}else{"DESCRICAO_CARGO"},
                  SG_PARTIDO=if(a==2002){"SG_PART"}else{"SIGLA_PARTIDO"},
                  SG_UE=if(a==2002){"SG_UF"}else{"UNIDADE_ELEITORAL_CANDIDATO"},
                  VR_RECEITA=if(a==2002){"VR_RECEITA"}else{"VALOR_RECEITA"}) %>% 
    dplyr::mutate(orientacao=case_when(SG_PARTIDO %in% esquerda~"Esquerda",
                                       SG_PARTIDO %in% centro~"Centro",
                                       SG_PARTIDO %in% direita~"Direita",
                                       TRUE~"Outros"),
                  ANO=a) %>% 
    dplyr::group_by(ANO,SG_UE,DS_CARGO,SG_PARTIDO,orientacao) %>% 
    dplyr::summarise(VR_RECEITA=sum(as.numeric(gsub(",",".",VR_RECEITA))))
  
  receitas <<- rbind(receitas,base)

}
}


base <- data.table::fread("./TSE/Contas/receitas_candidatos_2008_brasil.csv") %>% 
  dplyr::mutate(orientacao=case_when(SG_PARTIDO %in% esquerda~"Esquerda",
                                     SG_PARTIDO %in% centro~"Centro",
                                     SG_PARTIDO %in% direita~"Direita",
                                     TRUE~"Outros"),
                ANO=2008,
                SG_UE=as.character(SG_UE)) %>%
  dplyr::group_by(ANO,SG_UE,DS_CARGO,SG_PARTIDO,orientacao) %>% 
  dplyr::summarise(VR_RECEITA=sum(as.numeric(gsub(",",".",VR_RECEITA))))

receitas <<- rbind(receitas,base)

for(u in list.files("./TSE/Contas/candidato")){
 
  base <- data.table::fread(paste0("./TSE/Contas/candidato/",u,"/ReceitasCandidatos.txt")) %>% 
    dplyr::select(DS_CARGO=Cargo,
                  SG_PARTIDO=`Sigla Partido`,
                  SG_UE=UF,
                  VR_RECEITA=`Valor receita`) %>% 
    dplyr::mutate(orientacao=case_when(SG_PARTIDO %in% esquerda~"Esquerda",
                                       SG_PARTIDO %in% centro~"Centro",
                                       SG_PARTIDO %in% direita~"Direita",
                                       TRUE~"Outros"),
                  ANO=2010) %>% 
    dplyr::group_by(ANO,SG_UE,DS_CARGO,SG_PARTIDO,orientacao) %>% 
    dplyr::summarise(VR_RECEITA=sum(as.numeric(gsub(",",".",VR_RECEITA))))
    
    receitas <<- rbind(receitas,base)
  
}

for(a in c(2012,2016)){


for(u in ufs){
  
  base <- data.table::fread(paste0("./TSE/Contas/receitas_candidatos_",a,"_",u,".txt")) %>% 
    dplyr::select(DS_CARGO=Cargo,
                  SG_PARTIDO=`Sigla  Partido`,
                  SG_UE=if(a==2012){"Numero UE"}else{"Sigla da UE"},
                  VR_RECEITA=`Valor receita`) %>% 
    dplyr::mutate(orientacao=case_when(SG_PARTIDO %in% esquerda~"Esquerda",
                                       SG_PARTIDO %in% centro~"Centro",
                                       SG_PARTIDO %in% direita~"Direita",
                                       TRUE~"Outros"),
                  ANO=a,
                  SG_UE=as.character(SG_UE)) %>% 
    dplyr::group_by(ANO,SG_UE,DS_CARGO,SG_PARTIDO,orientacao) %>% 
    dplyr::summarise(VR_RECEITA=sum(as.numeric(gsub(",",".",VR_RECEITA))))
  
  receitas <<- rbind(receitas,base)

}
}


for(a in c(2014,2018)){
  
  
  for(u in c(ufs3,"BRASIL")){
    
    base <- data.table::fread(paste0("./TSE/Contas/receitas_candidatos_",a,"_",u,if(a==2014){".txt"}else{".csv"})) %>% 
      dplyr::select(DS_CARGO=if(a==2014){"Cargo"}else{"DS_CARGO"},
                    SG_PARTIDO=if(a==2014){"Sigla  Partido"}else{"SG_PARTIDO"},
                    SG_UE=if(a==2014){"UF"}else{"SG_UF"},
                    VR_RECEITA=if(a==2014){"Valor receita"}else{"VR_RECEITA"}) %>% 
      dplyr::mutate(orientacao=case_when(SG_PARTIDO %in% esquerda~"Esquerda",
                                         SG_PARTIDO %in% centro~"Centro",
                                         SG_PARTIDO %in% direita~"Direita",
                                         TRUE~"Outros"),
                    ANO=a) %>% 
      dplyr::group_by(ANO,SG_UE,DS_CARGO,SG_PARTIDO,orientacao) %>% 
      dplyr::summarise(VR_RECEITA=sum(as.numeric(gsub(",",".",VR_RECEITA))))
    
    receitas <<- rbind(receitas,base)
    
  }
}

file.remove(grep(list.files(path="./TSE/Contas/",full.names=T), 
                 pattern='.zip', 
                 invert=TRUE, 
                 value=TRUE))


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
                "PALMAS","NAZÁRIA","FERNANDO DE NORONHA"))) %>%
  dplyr::filter(localidade!="QUATRO MARCOS")

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
  dplyr::summarise(Valor=sum(valor)/12)

# Carregar dados de 2015 até hoje
smp_2015_2018 <- data.table::fread("Acessos_SMP_2015-2018_-_Total.csv") %>%
  dplyr::mutate_at(vars(`2015-01`:`2018-12`),
                   funs(case_when(is.na(.)==T~0,
                                  TRUE~as.numeric(.)))) %>%
  tidyr::gather("Ref","valor",9:length(.)) %>%
  dplyr::mutate(Ano=stringr::str_sub(Ref,1,4)) %>%
  dplyr::group_by(DDD,Tipo,Ano) %>%
  dplyr::summarise(Valor=sum(valor)/12)

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
censo <- readxl::read_excel("./Censo/censo_vars.xlsx") %>%
  dplyr::mutate_at(vars(linha_tel_2000,pc_2000,carro_2000,
                        pc_internet_2010,carro_2010,ens_med_2000,
                        pea_2000,rural_2000,rural_2010),
                   funs(as.numeric(.))) %>%
  dplyr::mutate_at(vars(`0_14_anos_2000`:`60_anos_2000`,
                        negro_2000,iluminacao_2000:carro_2000,
                        ens_med_2000,ens_sup_2000,casado_2000,
                        pea_2000,rural_2000),
                   funs(./Total_2000)) %>%
  dplyr::mutate_at(vars(`0_14_anos_2010`:`60_anos_2010`,
                        negro_2010,iluminacao_2010:carro_2010,
                        ens_med_2010,ens_sup_2010,casado_2010,
                        pea_2010,rural_2010),
                   funs(./Total_2010)) %>%
  dplyr::mutate(renda_med_2000=renda_med_2000*inflator,
                COD_IBGE=as.character(COD_IBGE))

summary(censo)

#################################################################
########## Carregar dados de população  #########################
#################################################################

# Fazer para 2012 a 2018
files <- list.files("./Populacao",pattern = "estimativa",
                    full.names = T)

populacao <- c()

for(i in files){
  
  a<-str_extract(i,"[0-9]{4}")
  
  pop<-
  readxl::read_excel(i,
                     sheet =2,
                     skip=1,
                     col_types = "text") %>%
    na.omit %>%
    dplyr::transmute(COD_IBGE=paste0(`COD. UF`,`COD. MUNIC`),
                     pop=as.numeric(`POPULAÇÃO ESTIMADA`),
                     ano=paste0("pop_",a))
  
  populacao <- rbind(populacao,pop)
}

# Fazer para 2007 a 2009 e 2011
files <- list.files("./Populacao",pattern = "pop",
                    full.names = T)
for(i in files){
  
  a<-str_extract(i,"[0-9]{4}")
  
  pop<-
    readxl::read_excel(i,
                       sheet =1,
                       skip=1,
                       col_types = "text") %>%
    na.omit %>%
    dplyr::rename_all(funs(c("uf","cod_uf","cod_mun",
                             "mun","pop"))) %>%
    dplyr::transmute(COD_IBGE=paste0(cod_uf,stringr::str_pad(cod_mun,5,"left",pad = 0)),
                     pop=as.numeric(pop),
                     ano=paste0("pop_",a))
  
  populacao <- rbind(populacao,pop)
}

populacao <- populacao %>%
  tidyr::spread(ano,pop) %>%
  dplyr::left_join(censo %>%
                     dplyr::select(COD_IBGE,Total_2010) %>%
                     dplyr::rename(pop_2010=Total_2010) %>%
                     dplyr::mutate(COD_IBGE=as.character(COD_IBGE)))


# Salvar as bases
save.image(file = "paper1.rda")

load("paper1.rda")


# Carregar base CETIC
library(survey)
library(srvyr)


cetic_2015 <- data.table::fread("http://cetic.br/media/microdados/81/ticdom_2015_individuos_base_de_microdados_v1.0.csv",
                                dec = ",")

sample_cetic <-
  survey::svydesign(ids = ~UPA,
                    strata=~ESTRATO,
                    weights=~Peso,
                    data=cetic_2015,
                    nest=T)

# wgts <-
#   survey::bootweights(
#     strata = cetic_2015$ESTRATO ,
#     psu = cetic_2015$UPA ,
#     replicates = 80 
#   )

# cetic_design <-
#   survey::svrepdesign(
#     weight = ~ Peso ,
#     repweights = wgts$repweights ,
#     type = "bootstrap",
#     combined.weights = FALSE ,
#     scale = wgts$scale ,
#     rscales = wgts$rscales ,
#     data = cetic_2015
#   )

sample_cetic <- as_survey(sample_cetic)
# cetic_design <- as_survey(cetic_design)

sample_cetic %>%
  srvyr::filter(idade %in% c(18:69)) %>%
  srvyr::group_by(C7_D) %>%
  srvyr::summarise(n=survey_total(vartype="ci"),
                   pct=survey_mean(vartype = "ci"))


# Baixar base de dados BPC

anos <- c(2004:2018)

BPC <- c()

for(i in anos){
  
  BPC_t <- data.table::fread(paste0("http://aplicacoes.mds.gov.br/sagi/servicos/misocial?q=*&fq=anomes_s:",i,"*&fq=tipo_s:mes_mu&wt=csv&omitHeader=true&fl=ibge:codigo_ibge,anomes:anomes_s,bpc_ben:bpc_ben_i,bpc_pcd_ben:bpc_pcd_ben_i,bpc_idoso_ben:bpc_idoso_ben_i,bpc_pcd_val:bpc_pcd_val_s,bpc_idoso_val:bpc_idoso_val_s,bpc_val:bpc_val_s&rows=100000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"))
  
  BPC <- rbind(BPC,BPC_t)
  
}

BOLSA <- c()

for(i in anos){
  
  BOLSA_t <- data.table::fread(paste0("http://aplicacoes.mds.gov.br/sagi/servicos/misocial?q=*&fq=anomes_s:",i,"*&fq=tipo_s:mes_mu&wt=csv&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"))
  
  BOLSA <- rbind(BOLSA,BOLSA_t)
  
}

rm(BPC_t,BOLSA_t,i,anos)

BOLSA_anual <- BOLSA %>%
  dplyr::mutate(Ano=substr(anomes,1,4),
                ibge=as.character(ibge)) %>%
  dplyr::group_by(ibge,Ano) %>% 
  dplyr::summarise(qtd_fam_bolsa=round(mean(qtd_familias_beneficiarias_bolsa_familia)),
                   valor_bolsa=sum(valor_repassado_bolsa_familia)) %>%
  dplyr::rename(COD_IBGE=ibge)

BPC_anual <- BPC %>% 
  dplyr::mutate(Ano=substr(anomes,1,4),
                ibge=as.character(ibge)) %>%
  dplyr::group_by(ibge,Ano) %>% 
  dplyr::summarise(qtd_bpc_idoso=round(mean(bpc_idoso_ben)),
                   qtd_bpc_pcd=round(mean(bpc_pcd_ben)),
                   valor_bpc_idoso=sum(bpc_idoso_val),
                   valor_bpc_pcd=sum(bpc_pcd_val)) %>%
  dplyr::rename(COD_IBGE=ibge)

BPC_anual %>%
  dplyr::group_by(Ano) %>%
  dplyr::summarise(n_idoso=sum(valor_bpc_idoso),
                   n_pcd=sum(valor_bpc_pcd),
                   b_idoso=sum(qtd_bpc_idoso),
                   b_pcd=sum(qtd_bpc_pcd)) %>%
  dplyr::mutate(n_idoso=n_idoso/1000000000,
                n_pcd=n_pcd/1000000000,
                total=n_idoso+n_pcd,
                b_idoso=b_idoso/1000000,
                b_pcd=b_pcd/1000000,
                total_b=b_idoso+b_pcd)

BOLSA_anual %>%
  dplyr::group_by(Ano) %>%
  dplyr::summarise(n_fam=sum(qtd_fam_bolsa),
                   n_valor=sum(valor_bolsa)) %>%
  dplyr::mutate(n_valor=n_valor/1000000000,
                n_fam=n_fam/1000000)

pib <- data.table::fread("pibmun.csv",encoding = "UTF-8")

# Carregar informações da RAIS
rais <- data.table::fread("rais_bi_vinc.csv") %>%
  dplyr::select(-18) %>%
  dplyr::filter(`Município`!="{ñ class}") %>%
  tidyr::gather("Ano","vinculos_rais",-1) %>%
  dplyr::rename(COD_IBGE=`Município`) %>%
  dplyr::arrange(COD_IBGE,Ano) %>% 
  dplyr::left_join(data.table::fread("rais_bi_salarios.csv",
                                     dec = ",") %>%
                     dplyr::select(-18,-19) %>%
                     dplyr::filter(!`Município` %in% c("{ñ class}","Total")) %>%
                     tidyr::gather("Ano","salarios_rais",-1) %>%
                     dplyr::rename(COD_IBGE=`Município`) %>%
                     dplyr::arrange(COD_IBGE,Ano)) 

# Carregar informações do Imet

# Listar arquivos
files <- list.files("imet/mensal", full.names = T)

# Carregar bases

info_estacao <- c()

for(i in files){

base <- data.table::fread(i,
                          stringsAsFactors=F) %>%
  t() %>% 
  data.frame(stringsAsFactors = F) %>%
  janitor::row_to_names(1) %>%
  dplyr::rename_all(list(~c("UF","ESTACAO","COD_WMO","LAT","LONG","ALTITUDE",
                    "DAT_INICIO_OP","DATA_FIM_OP","STATUS"))) %>%
  remove_rownames

info_estacao <<- rbind(info_estacao,base)

}

files <- list.files("imet/diario", full.names = T)

dados_estacao <- c()

for(i in files){

wmo <- data.table::fread(i,
                          stringsAsFactors=F) %>%
  .[[3,2]] 

base <- data.table::fread(i,skip=8, header = T) %>%
  dplyr::select(-7) %>% 
  dplyr::rename_all(list(~c("DATA","PRECIP_TOT",
                            "TEMP_MAX","TEMP_MED","TEMP_MIN",
                            "URA_MED"))) %>%
  dplyr::mutate(COD_WMO=wmo)

dados_estacao <<- rbind(dados_estacao,base)
}


dados_estacao <- dados_estacao %>%
  dplyr::left_join(info_estacao)

problemas <- dados_estacao %>%
  dplyr::filter(is.na(ESTACAO)==T)

table(problemas$COD_WMO)

# As estações problemáticas não apresentam dados recentes
# Todos anteriores a 1966

rm(base,info_estacao,problemas,wmo,files,i)

# Carregar dados do IPEA Data

ipea <- ipeadatar::ipeadata(c("THOMIC","TSUICID",
                              "DISTCAPF","DISTCAPU",
                              "DESPCORRM","DINVESTM","DESPCUPM")) %>%
  dplyr::filter(uname=="Municipality")



