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
dir.create("TSE2/Eleitorado", recursive = T)

# Anos a serem baixados
anos <- seq(1994,format(Sys.Date(), "%Y"),2)

# Endereço dos dados de eleitorado
dwlpath <- "http://agencia.tse.jus.br/estatistica/sead/odsele/perfil_eleitorado/perfil_eleitorado_"

# # Arquivos temporários
# tf <- tempfile()
# td <- tempdir()

# looping para baixar os dados
for (i in anos){
  tf <- paste0(dwlpath, i, ".zip")
  td <- paste0("./TSE2/Eleitorado/", i, ".zip")
  print(i)
  download.file(tf, td, mode="wb")
}

# Descompactar arquivos
filenames <- list.files("./TSE2/Eleitorado/", pattern=".zip", full.names=TRUE)
lapply(filenames,unzip, exdir = "./TSE2/Eleitorado",junkpaths=T)

# Carregar os anos de interesse
anos <- seq(2000,2014,2)

# Looping para carregar todas as bases

eleitorado <- c()

for(i in anos){
  
base <- data.table::fread(paste0("TSE2/Eleitorado/perfil_eleitorado_",i,
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
file.remove(list.files("TSE2/Eleitorado",pattern = ".txt|csv",full.names = T))

#####################################################
########## Carregar a base de resultado ###########
####################################################

# Criar diretórios para download dos dados de eleitorado
dir.create("TSE2/Resultado", recursive = T)

# Anos a serem baixados
anos <- seq(1994,format(Sys.Date(), "%Y"),2)

# Endereço dos dados de eleitorado
dwlpath <- "http://agencia.tse.jus.br/estatistica/sead/odsele/detalhe_votacao_munzona/detalhe_votacao_munzona_"

# looping para baixar os dados
for (i in anos){
  tf <- paste0(dwlpath, i, ".zip")
  td <- paste0("./TSE2/Resultado/","detalhe",i, ".zip")
  print(i)
  download.file(tf, td, mode="wb")
}

# Descompactar arquivos
filenames <- list.files("./TSE2/Resultado/", pattern=".zip", full.names=TRUE)
lapply(filenames,unzip, exdir = "./TSE2/Resultado",junkpaths=T)

# Carregar a base de resultado do TSE de 2018
# Fazer o looping para carregar todos os municípios

# Caminho inicial
initp <- "TSE2/Resultado/detalhe_votacao_munzona_"

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
  dplyr::summarise(municipio=first(NM_MUNICIPIO),
                   ano=first(ANO_ELEICAO),
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


  
# Caregar base de votação
# Caminho inicial
initp <- "TSE/Resultado/votacao_partido_munzona_2018_"


# Construir objeto para receber valores
votacao_18 <- c()

for(i in ufs){
base <- data.table::fread(paste0(initp,i,finp)) %>%
  dplyr::mutate(votos=QT_VOTOS_NOMINAIS+QT_VOTOS_LEGENDA) %>%
  dplyr::group_by(ANO_ELEICAO,CD_MUNICIPIO,NM_MUNICIPIO,DS_CARGO,SG_PARTIDO) %>%
  dplyr::summarise(votos=sum(votos))

votacao_18 <<- rbind(votacao_18,base)

}

# Carregar dados dos candidatos

candidatos_18 <-data.table::fread("TSE/Candidatos/2018/consulta_cand_2018_BRASIL.csv") %>%
  dplyr::filter(CD_SITUACAO_CANDIDATURA==12) %>%
  dplyr::mutate(orientacao=case_when(SG_PARTIDO %in% c("PT","PDT","PSB",
                                                       "PC do B","PPS","PMN",
                                                       "PV","PATRI","PCB",
                                                       "PCO","PSOL","PSTU",
                                                       "SOLIDARIEDADE")~"Esquerda",
                                     SG_PARTIDO %in% c("MDB,PMDB",
                                                       "PSDB","PTB","Rede",
                                                       "Avante","NOVO",
                                                       "PTN","PODEMOS")~"Centro",
                                     SG_PARTIDO %in% c("PFL","PPB","PL",
                                                       "PSD","PSC","Prona",
                                                       "PSL","PST","DEM",
                                                       "PP","PTC")~"Direita",
                                     TRUE~"Outros")) %>%
  dplyr::count(ANO_ELEICAO,DS_CARGO,SG_UF,orientacao)
  
  
  
  
# Ler dados da Anatel, backhaul
backhaul <- readstata13::read.dta13("anatel_backhaul.dta") %>%
  dplyr::select(fk_cod_municipio,ano,backhaul_ano,backhaul,popIBGE_anoantes,
                Situao,tecnologiadeatendimento,UF,Municpio,
                concessionaria)

# Ler dados brutos da Anatel
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
smp_2005_200901 <- data.table::fread("Acessos_SMP_2005-200901_-_Total.csv") 

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

# Carregar base com correspondências entre TSE e IBGE para municípios
dic_tse_ibge <- data.table::fread("TSE/6-capital-humano.csv") %>%
  dplyr::select(Cod_TSE,Cod_IBGE)

# Pegar dados do censo
library(lodown)

